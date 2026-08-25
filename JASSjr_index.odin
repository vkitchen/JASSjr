/*
	JASSjr_index.odin
	-----------------
	Copyright (c) 2026 Vaughan Kitchen
	Minimalistic BM25 search engine.
*/

package main

import "core:bufio"
import "core:fmt"
import "core:os"
import "core:strings"

/*
	Struct posting
	--------------
*/
Posting :: struct {
	d: i32,
	tf: i32,
}

is_alpha :: proc(c: byte) -> bool {
	return ('A' <= c && c <= 'Z') || ('a' <= c && c <= 'z')
}

is_digit :: proc(c: byte) -> bool {
	return '0' <= c && c <= '9'
}

is_alnum:: proc(c: byte) -> bool {
	return is_alpha(c) || is_digit(c)
}

/*
	Struct lexer
	------------
*/
Lexer :: struct {
	buffer: string,
	current: int,
}

/*
	lex_get_next()
	--------------
	One-character lookahead lexical analyser
*/
get_next :: proc(l: ^Lexer) -> string {
	/*
		Skip whitespace and punctuation, but not XML tags.
	*/
	for l.current < len(l.buffer) && !is_alnum(l.buffer[l.current]) && l.buffer[l.current] != '<' {
		l.current += 1
	}

	/*
		A token is either an XML tag '<'..'>' or a sequence of alpha-numerics.
	*/
	start := l.current
	if l.current >= len(l.buffer) {
		return ""
	}

	if is_alnum(l.buffer[l.current]) {
		// TREC <DOCNO> primary keys have a hyphen in them
		for l.current < len(l.buffer) && (is_alnum(l.buffer[l.current]) || l.buffer[l.current] == '-') {
			l.current += 1
		}
	} else if l.buffer[l.current] == '<' {
		for l.current < len(l.buffer) {
			c := l.buffer[l.current]
			l.current += 1

			if c == '>' {
				break
			}
		}
	}

	/*
		Return the token
	*/
	return l.buffer[start:l.current]
}

/*
	main()
	------
	Simple indexer for TREC WSJ collection
*/
main :: proc() {
	buffer: [2048]byte				// index line at a time where a line fits in this buffer
	vocab := make(map[string][dynamic]Posting)	// the in-memory index
	doc_ids: [dynamic]string			// the primary keys
	doc_lengths: [dynamic]i32			// hold the length of each document

	docid: i32 = -1
	document_length: i32 = 0

	// Make sure we have one parameter, the filename
	if len(os.args) != 2 {
		fmt.eprintfln("Usage: %s <infile.xml>", os.args[0])
		os.exit(0)
	}

	// open the file to index
	fh, err := os.open(os.args[1])
	if err != nil {
		fmt.eprintfln("Can't open file %v", err);
		os.exit(1);
	}
	defer os.close(fh)

	reader: bufio.Reader
	bufio.reader_init_with_buf(&reader, os.to_stream(fh), buffer[:])
	defer bufio.reader_destroy(&reader)

	push_next := false // is the next token the primary key?
	for {
		line, err := bufio.reader_read_string(&reader, '\n', context.allocator)
		if err != nil {
			break
		}
		defer delete(line, context.allocator)

		lex := Lexer{ line, 0 }
		for token := get_next(&lex); len(token) > 0; token = get_next(&lex) {
			/*
				If we see a <DOC> tag then we're at the start of the next document
			*/
			if token == "<DOC>" {
				/*
					Save the previous document length
				*/
				if docid != -1 {
					append(&doc_lengths, document_length)
				}

				/*
					Move on to the next document
				*/
				docid += 1
				document_length = 0

				if docid % 1000 == 0 {
					fmt.printfln("%d documents indexed", docid)
				}
			}

			/*
				if the last token we saw was a <DOCNO> then the next token is the primary key
			*/
			if push_next {
				append(&doc_ids, strings.clone(token))
				push_next = false
			}
			if token == "<DOCNO>" {
				push_next = true
			}

			/*
				Don't index XML tags
			*/
			if token[0] == '<' {
				continue
			}

			/*
				lower case the string
			*/
			lowercase := strings.to_lower(token)

			/*
				truncate any long tokens at 255 characters (so that the length can be stored first and in a single byte)
			*/
			if len(lowercase) > 255 {
				lowercase = lowercase[:255]
			}

			/*
				add the posting to the in-memory index
			*/
			list, ok := vocab[token]

			if !ok {
				// term isn't in the vocab yet
				list = make([dynamic]Posting)
				vocab[token] = list
			}

			if len(list) == 0 || list[len(list)-1].d != docid {
				// if the docno for this occurence has changed then create a new <d,tf> pair
				append(&vocab[token], Posting{docid, 1})
			} else {
				// else increase the tf
				list[len(list)-1].tf += 1
			}

			/*
				Compute the document length
			*/
			document_length += 1
		}
	}

	/*
		If we didn't index any documents then we're done.
	*/
	if docid == -1 {
		os.exit(0)
	}
}
