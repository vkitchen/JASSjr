// JASSJR_INDEX.ha
// ---------------
// Copyright (c) 2026 Vaughan Kitchen
// Minimalistic BM25 search engine.

use ascii;
use bufio;
use endian;
use fmt;
use hash::fnv;
use io;
use os;
use strings;
use types;

// Struct lexer
// ------------
type lexer = struct {
	buffer: []u8,
	current: size,
};

fn get_next(l: *lexer) str = {
	// Skip whitespace and punctuation, but not XML tags.
	for (l.current < len(l.buffer) && !ascii::isalnum(l.buffer[l.current]: rune) && l.buffer[l.current] != '<') {
		l.current += 1;
	};

	if (l.current >= len(l.buffer)) {
		return "";
	};

	// A token is either an XML tag '<'..'>' or a sequence of alpha-numerics.
	const start = l.current;
	if (ascii::isalnum(l.buffer[l.current]: rune)) {
		// TREC <DOCNO> primary keys have a hyphen in them
		for (l.current < len(l.buffer) && (ascii::isalnum(l.buffer[l.current]: rune) || l.buffer[l.current] == '-')) {
			l.current += 1;
		};
	} else if (l.buffer[l.current] == '<') {
		l.current += 1;
		for (l.current < len(l.buffer) && l.buffer[l.current-1] != '>') {
			l.current += 1;
		};
	};

	// Return the token
	return strings::fromutf8(l.buffer[start..l.current])!;
};

fn truncate(s: str, to: size) str = {
	const buf = strings::toutf8(s);
	if (len(buf) <= to) {
		return s;
	};
	return strings::fromutf8_unsafe(buf[..to]);
};

// Struct posting
// --------------
type posting = struct { d: int, tf: int };

// Struct dict
// -----------
type entry = struct { key: str, val: *[]posting };
type dict = struct { store: [4096][]entry };

fn get_or_put(d: *dict, key: str) *[]posting = {
	const hash = fnv::string(key);
	const bucket = &d.store[hash % len(d.store)];
	for (let i = 0z; i < len(bucket); i += 1) {
		if (bucket[i].key == key) {
			return bucket[i].val;
		};
	};

	const obj = entry{ key = key, val = alloc([]: []posting)! };
	append(bucket, obj)!;
	return obj.val;
};

fn raw(input: []int) []u8 = {
	let header = &input : *types::slice;
	header.length *= size(int);
	header.capacity *= size(int);
	return *(header: *[]u8);
};

fn raw_postings(input: []posting) []u8 = {
	let header = &input : *types::slice;
	header.length *= size(posting);
	header.capacity *= size(posting);
	return *(header: *[]u8);
};

// MAIN()
// ------
// Simple indexer for TREC WSJ collection
export fn main() void = {
	// Make sure we have one parameter, the filename
	if (len(os::args) != 2) {
		fmt::fatalf("Usage: {} <infile.xml>", os::args[0]);
	};

	let docid = -1;
	let document_length = 0;

	let vocab: dict = dict{ ... };
	let doc_ids = alloc([]: []str)!;
	let doc_lengths = alloc([]: []int)!;

	// open the file to index
	const fh = os::open(os::args[1])!;
	defer io::close(fh)!;

	// is the next token the primary key?
	let push_next = false;
	for (let line => bufio::read_line(fh)!) {
		let lex = lexer{ buffer = line, current = 0 };
		for (let token = get_next(&lex); len(token) > 0; token = get_next(&lex)) {
			// If we see a <DOC> tag then we're at the start of the next document
			if (token == "<DOC>") {
				// Save the previous document length
				if (docid != -1) {
					append(doc_lengths, document_length)!;
				};

				// Move on to the next document
				docid += 1;
				document_length = 0;

				if (docid % 1000 == 0) {
					fmt::printfln("{} documents indexed", docid)!;
				};
			};

			// if the last token we saw was a <DOCNO> then the next token is the primary key
			if (push_next) {
				append(doc_ids, token)!;
				push_next = false;
			};
			if (token == "<DOCNO>") {
				push_next = true;
			};

			// Don't index XML tags
			if (strings::hasprefix(token, '<')) {
				continue;
			};

			// lower case the string
			token = ascii::strlower(token)!;

			// truncate any long tokens at 255 characters (so that the length can be stored first and in a single byte)
			token = truncate(token, 255);

			// Add the posting to the in-memory index
			const postings = get_or_put(&vocab, token);

			if (len(postings) == 0 || postings[len(postings)-1].d != docid) {
				// if the docno for this occurence has changed then create a new <d,tf> pair
				append(postings, posting{d = docid, tf = 1})!;
			} else {
				// else increase the tf
				postings[len(postings)-1].tf += 1;
			};

			// compute the document length
			document_length += 1;
		};
	};

	// If we didn't index any documents then we're done.
	if (docid == -1) {
		os::exit(0);
	};

	// Save the final document length
	append(doc_lengths, document_length)!;

	// tell the user we've got to the end of parsing
	fmt::printfln("Indexed {} documents. Serialising...", docid + 1)!;

	// store the primary keys
	let docid_fh = os::create_buffered("docids.bin", 644)!;
	defer bufio::flush(&docid_fh)!;
	defer io::close(&docid_fh)!;
	for (let id .. doc_ids) {
		io::write(&docid_fh, strings::toutf8(id))!;
		io::write(&docid_fh, strings::toutf8("\n"))!;
	};

	// serialise the in-memory index to disk
	let postings_fh = os::create_buffered("postings.bin", 644)!;
	defer bufio::flush(&postings_fh)!;
	defer io::close(&postings_fh)!;

	let vocab_fh = os::create_buffered("vocab.bin", 644)!;
	defer bufio::flush(&vocab_fh)!;
	defer io::close(&vocab_fh)!;

	let whence: size = 0;
	for (let bucket .. vocab.store) {
		for (let pair .. bucket) {
			// Write the postings list to one file
			io::write(&postings_fh, raw_postings(*pair.val))!;

			// Write the vocabulary to a second file (one byte length, string, '\0', 4 byte where, 4 byte size)
			io::write(&vocab_fh, [len(pair.key): u8])!;
			io::write(&vocab_fh, strings::toutf8(pair.key))!;
			io::write(&vocab_fh, [0])!;

			let buf: [4]u8 = [0...];
			endian::leputu32(buf, whence: u32);
			io::write(&vocab_fh, buf)!;
			endian::leputu32(buf, (len(pair.val) * 8): u32);
			io::write(&vocab_fh, buf)!;

			whence += len(pair.val) * 8;
		};
	};

	// store the document lengths
	let lengths_fh = os::create_buffered("lengths.bin", 644)!;
	defer bufio::flush(&lengths_fh)!;
	defer io::close(&lengths_fh)!;
	io::write(&lengths_fh, raw(*doc_lengths))!;
};
