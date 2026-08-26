/*
	JASSjr_search.odin
	------------------
	Copyright (c) 2026 Vaughan Kitchen
	Minimalistic BM25 search engine.
*/

package main

import "core:bufio"
import "core:encoding/endian"
import "core:fmt"
import "core:math"
import "core:mem"
import "core:os"
import "core:slice"
import "core:strconv"
import "core:strings"

/*
	CONSTANTS
	---------
*/
k1 := 0.9	// BM25 k1 parameter
b := 0.4	// BM25 b parameter

/*
	STRUCT VOCAB_ENTRY
	------------------
*/

VocabEntry :: struct {
	// where on the disk and how large (in bytes) is the postings list?
	whence: u32,
	size: u32,
}

/*
	main()
	------
	Simple search engine ranking on BM25.
*/
main :: proc() {
	/*
		Read the document lengths
	*/
	data, err := os.read_entire_file("lengths.bin", context.allocator)
	assert(err == nil)

	doc_lengths := slice.reinterpret([]u32, data)

	/*
		Compute the average document length for BM25
	*/
	documents_in_collection := len(doc_lengths)
	average_document_length := f64(math.sum(doc_lengths)) / f64(documents_in_collection)

	/*
		Read the primary keys
	*/
	data, err = os.read_entire_file("docids.bin", context.allocator)
	assert(err == nil)

	primary_keys := strings.split_lines(string(data))

	/*
		Open the postings list file
	*/
	postings_fh: ^os.File
	postings_fh, err = os.open("postings.bin")
	assert(err == nil)
	defer os.close(postings_fh)

	/*
		Build the vocabulary in memory
	*/
	dictionary := make(map[string]VocabEntry) // the vocab
	data, err = os.read_entire_file("vocab.bin", context.allocator)
	assert(err == nil)
	for offset := 0; offset < len(data); {
		string_length := int(data[offset])
		offset += 1

		term := string(data[offset : offset+string_length])
		offset += string_length + 1 // read the '\0' string terminator

		whence, _ := endian.get_u32(data[offset:], .Little)
		offset += 4
		size, _ := endian.get_u32(data[offset:], .Little)
		offset += 4

		dictionary[term] = VocabEntry{whence, size}
	}

	/*
		Allocate buffers
	*/
	postings_buffer := make([]u32, documents_in_collection * 2)	// the postings list once loaded from disk
	rsv := make([]f64, documents_in_collection)			// array of rsv values

	/*
		Set up the rsv pointers
	*/
	rsv_pointers := make([]^f64, documents_in_collection)
	for i := 0; i < len(rsv); i += 1 {
		rsv_pointers[i] = &rsv[i]
	}

	/*
		Search (one query per line)
	*/
	scanner: bufio.Scanner
	bufio.scanner_init(&scanner, os.to_stream(os.stdin), context.temp_allocator)
	defer bufio.scanner_destroy(&scanner)
	for bufio.scanner_scan(&scanner) {
		/*
			Zero the accumulator array.
		*/
		mem.zero(raw_data(rsv), len(rsv) * size_of(f64))

		line := bufio.scanner_text(&scanner)
		query_id: int
		for token, i in strings.fields(line) {
			/*
				If the first token is a number then assume a TREC query number, and skip it
			*/
			if i == 0 {
				if num, ok := strconv.parse_int(token); ok {
					query_id = num
					continue
				}
			}

			/*
				Does the term exist in the collection?
			*/
			term_details, ok := dictionary[token]
			if !ok {
				continue
			}

			/*
				if IDF == 0 then don't process this postings list as the BM25 contribution of this term will be zero.
			*/
			postings := int(term_details.size) / 8
			if postings == documents_in_collection {
				continue
			}

			/*
				Seek and read the postings list
			*/
			os.seek(postings_fh, i64(term_details.whence), .Start)
			os.read(postings_fh, slice.reinterpret([]u8, postings_buffer)[:term_details.size])

			/*
				Compute the IDF component of BM25 as log(N/n).
			*/
			idf := math.ln(f64(documents_in_collection) / f64(postings))

			/*
				Process the postings list by simply adding the BM25 component for this document into the accumulators array
			*/
			for i := 0; i < postings * 2; i += 2 {
				d := postings_buffer[i]
				tf := f64(postings_buffer[i+1])
				rsv[d] += idf * ((tf * (k1 + 1)) / (tf + k1 * (1 - b + b * (f64(doc_lengths[d]) / average_document_length))))
			}
		}

		/*
			Sort the results list
		*/
		slice.sort_by(rsv_pointers, proc(a, b: ^f64) -> bool {
			return a^ > b^ ? true : a^ == b^ ? a > b : false
		})

		/*
			Print the (at most) top 1000 documents in the results list in TREC eval format which is:
			query-id Q0 document-id rank score run-name
		*/
		for i := 0; rsv_pointers[i]^ != 0 && i < 1000; i += 1 {
			idx := int(uintptr(rsv_pointers[i]) - uintptr(raw_data(rsv))) / size_of(f64)
			fmt.printfln("%d Q0 %s %d %.4f JASSjr", query_id, primary_keys[idx], i+1, rsv_pointers[i]^)
		}
	}
}
