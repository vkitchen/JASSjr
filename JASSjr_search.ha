// JASSJR_INDEX.ha
// ---------------
// Copyright (c) 2026 Vaughan Kitchen
// Minimalistic BM25 search engine.

use bufio;
use endian;
use errors;
use fmt;
use hash::fnv;
use io;
use math;
use os;
use sort;
use strconv;
use strings;
use types;

// Constants
// ---------
const k1 = 0.9; // BM25 k1 parameter
const b = 0.4;  // BM25 b parameter

// Struct posting
// --------------
type posting = struct { d: int, tf: int };

// Struct dict
// -----------
type entry = struct {
	key: str,

	// where on the disk and how large (in bytes) is the postings list?
	whence: u32,
	sz: u32,
};
type dict = struct { store: [4096][]entry };

fn put(d: *dict, key: str, whence: u32, sz: u32) void = {
	const hash = fnv::string(key);
	const bucket = &d.store[hash % len(d.store)];

	const obj = entry{ key = key, whence = whence, sz = sz };
	append(bucket, obj)!;
};

fn get(d: *dict, key: str) (errors::noentry | *entry) = {
	const hash = fnv::string(key);
	const bucket = &d.store[hash % len(d.store)];
	for (let i = 0z; i < len(bucket); i += 1) {
		if (bucket[i].key == key) {
			return &bucket[i];
		};
	};

	return errors::noentry;
};

fn raw(input: []int) []u8 = {
	let header = &input : *types::slice;
	header.length *= size(int);
	header.capacity *= size(int);
	return *(header: *[]u8);
};

// COMPARE_RSV()
// -------------
// Callback from sort::sort for two rsv values. Tie break on the document id.
fn compare_rsv(a: *opaque, b: *opaque) int = {
	const first = *(a: **f64);
	const second = *(b: **f64);

	return if (*first > *second) -1
		else if (*first < *second) 1
		else (second: uintptr - first: uintptr): int;
};

// main()
// ------
// Simple search engine ranking on BM25.
export fn main() void = {
	// Read the document lengths
	let stat = os::stat("lengths.bin")!;
	const documents_in_collection = stat.sz / size(int);

	let lengths_fh = os::open("lengths.bin")!;
	defer io::close(lengths_fh)!;

	let doc_lengths = alloc([0...], documents_in_collection)!;
	let doc_bytes = raw(doc_lengths);
	io::read(lengths_fh, doc_bytes)!;

	// Compute the average document length for BM25
	let average_document_length = 0.0;
	for (let which .. doc_lengths) {
		average_document_length += which: f64;
	};
	average_document_length /= documents_in_collection: f64;

	// Read the primary keys
	let primary_keys = alloc([""...], documents_in_collection)!; // the list of global IDs (i.e. primary keys)
	let docids_fh = os::open_buffered("docids.bin")!;
	defer io::close(&docids_fh)!;

	for (let i = 0z; i < documents_in_collection; i += 1) {
		const line = bufio::read_line(&docids_fh)!;
		match (line) {
			case let bytes: []u8 =>
				primary_keys[i] = strings::fromutf8(bytes)!;
			case io::EOF =>
				abort();
		};
	};

	// Open the postings list file
	let postings_fh = os::open("postings.bin")!;

	// Build the vocabulary in memory
	let vocab: dict = dict{ ... };

	stat = os::stat("vocab.bin")!;
	let vocab_raw: []u8 = alloc([0...], stat.sz)!;
	let vocab_fh = os::open("vocab.bin")!;
	defer io::close(vocab_fh)!;
	io::readall(vocab_fh, vocab_raw)!;

	let idx = 0z;
	for (idx < stat.sz) {
		const str_len = vocab_raw[idx];
		idx += 1;

		const term = strings::fromutf8(vocab_raw[idx..idx + str_len])!;
		idx += str_len + 1; // read the '\0' string terminator

		const whence = endian::legetu32(vocab_raw[idx..idx + 4]);
		idx += 4;

		const sz = endian::legetu32(vocab_raw[idx..idx + 4]);
		idx += 4;

		put(&vocab, term, whence, sz);
	};

	// Allocate buffers
	let postings_buffer = alloc([0...], documents_in_collection * 2)!; // the postings list once loaded from disk
	let postings_bytes = raw(postings_buffer);
	let rsv: []f64 = alloc([0.0...], documents_in_collection)!; // array of rsv values

	// Set up the rsv pointers
	let rsv_pointers: []*f64 = alloc([null: *f64...], documents_in_collection)!;
	for (let i = 0z; i < documents_in_collection; i += 1) {
		rsv_pointers[i] = &rsv[i];
	};

	// Search (one query per line)
	for (true) {
		const line = bufio::read_line(os::stdin)!;
		match (line) {
		case let bytes: []u8 =>
			// Zero the accumulator array.
			for (let i = 0z; i < documents_in_collection; i += 1) {
				rsv[i] = 0.0;
			};

			let first_token = true;
			let query_id = 0;
			let tok = strings::tokenize(strings::fromutf8(bytes)!, " \t\r\n");
			for (true) {
				match (strings::next_token(&tok)) {
				case let token: str =>
					// If the first token is a number then assume a TREC query number, and skip it
					if (first_token) {
						match (strconv::stoi(token)) {
						case let num: int =>
							query_id = num;
							continue;
						case let err: strconv::error =>
							void;
						};
					};
					first_token = false;

					// Does the term exist in the collection?
					match (get(&vocab, token)) {
					case let term_details: *entry =>
						// if IDF == 0 then don't process this postings list as the BM25 contribution of this term will be zero.
						const postings = term_details.sz / 8;
						if (postings == documents_in_collection) {
							continue;
						};

						// Seek and read the postings list
						io::seek(postings_fh, term_details.whence: i64, io::whence::SET)!;
						io::read(postings_fh, postings_bytes[0..term_details.sz])!;

						// Compute the IDF component of BM25 as log(N/n).
						const idf = math::logf64(documents_in_collection: f64 / postings: f64);

						// Process the postings list by simply adding the BM25 component for this document into the accumulators array
						for (let i = 0z; i < postings * 2; i += 2) {
							const d = postings_buffer[i];
							const tf = postings_buffer[i+1]: f64;
							rsv[d] += idf * ((tf * (k1 + 1.0)) / (tf + k1 * (1.0 - b + b * (doc_lengths[d]: f64 / average_document_length))));
						};

					case errors::noentry =>
						continue;
					};
				case done =>
					break;
				};
			};

			// Sort the results list
			sort::sort(rsv_pointers: []opaque, size(f64), &compare_rsv)!;

			// Print the (at most) top 1000 documents in the results list in TREC eval format which is:
			// query-id Q0 document-id rank score run-name
			for (let i = 0z; *rsv_pointers[i] != 0.0 && i < 1000; i += 1) {
				const idx = (rsv_pointers[i]: uintptr - &rsv[0]: uintptr) / size(f64);
				fmt::printfln("{} Q0 {} {} {:.4f} JASSjr", query_id, primary_keys[idx], i+1, *rsv_pointers[i])!;
			};
		case io::EOF =>
			break;
		};
	};
};
