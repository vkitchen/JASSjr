#!/usr/bin/env julia

# JASSjr_index.jl
# Copyright (c) 2026 Vaughan Kitchen
# Minimalistic BM25 search engine.

if length(ARGS) != 1
    println(stderr, "Usage: $(PROGRAM_FILE) <infile.xml>")
    exit(1)
end

vocab = Dict{String, Vector{UInt32}}() # the in-memory index
doc_ids = String[] # the primary keys
doc_lengths = UInt32[] # hold the length of each document

# A token is either an XML tag '<'..'>' or a sequence of alpha-numerics.
# TREC <DOCNO> primary keys have a hyphen in them
lexer = r"[a-zA-Z0-9][a-zA-Z0-9-]*|<[^>]*>"

docid = -1
document_length = 0
push_next = false # is the next token the primary key?

open(ARGS[1]) do file
    for ln in eachline(file)
        for m in eachmatch(lexer, ln)
            global docid, document_length, push_next
            token = m.match
            # If we see a <DOC> tag then we're at the start of the next document
            if token == "<DOC>"
                # Save the previous document length
                if docid != -1
                    push!(doc_lengths, document_length)
                end
                # Move on to the next document
                docid += 1
                document_length = 0
                if docid % 1000 == 0
                    println("$(docid) documents indexed")
                end
            end
            # if the last token we saw was a <DOCNO> then the next token is the primary key
            if push_next
                push!(doc_ids, token)
                push_next = false
            end
            if token == "<DOCNO>"
                push_next = true
            end
            # Don't index XML tags
            if token[1] == '<'
                continue
            end

            # lower case the string
            token = lowercase(token)

            # truncate any long tokens at 255 charactes (so that the length can be stored first and in a single byte)
            token = first(token, 255)

            # add the posting to the in-memory index
            postings_list = get!(vocab, token, [])
            if length(postings_list) == 0 || postings_list[end-1] != docid
                push!(postings_list, docid, 1)
            else
                postings_list[end] += 1
            end

            # Compute the document length
            document_length += 1
        end
    end
end

# If we didn't index any documents then we're done.
if docid == -1
    exit()
end

# Save the final document length
push!(doc_lengths, document_length)

# tell the user we've got to the end of parsing
println("Indexed $(docid + 1) documents. Serialising...")

# store the primary keys
open("docids.bin", "w") do file
    for doc in doc_ids
        write(file, "$(doc)\n")
    end
end

postings_fp = open("postings.bin", "w")
vocab_fp = open("vocab.bin", "w")

# serialise the in-memory index to disk
for (term, postings) in vocab
    # write the postings list to one file
    whence = position(postings_fp)
    write(postings_fp, postings)

    # write the vocabulary to a second file (one byte length, string, '\0', 4 byte where, 4 byte size)
    write(vocab_fp, UInt8(length(term)))
    write(vocab_fp, term)
    write(vocab_fp, '\0') # string is null terminated
    write(vocab_fp, UInt32(whence))
    write(vocab_fp, UInt32(length(postings) * 4)) # no. of bytes
end

# store the document lengths
open("lengths.bin", "w") do file
    write(file, doc_lengths)
end

# clean up
close(postings_fp)
close(vocab_fp)
