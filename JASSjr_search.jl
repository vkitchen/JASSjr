#!/usr/bin/env julia

# JASSjr_search.jl
# Copyright (c) 2026 Vaughan Kitchen
# Minimalistic BM25 search engine.

using Printf

k1 = 0.9 # BM25 k1 parameter
b = 0.4 # BM25 b parameter

doc_ids = readlines("docids.bin") # Read the primary_keys

# Read the document lengths
doc_lengths = Vector{UInt32}(undef, length(doc_ids))
open("lengths.bin", "r") do file
    read!(file, doc_lengths)
end

# Compute the average document length for BM25
average_length = Float64(sum(doc_lengths)) / length(doc_lengths)

# decode the vocabulary (unsigned byte length, string, '\0', 4 byte signed where, 4 signed byte size)
vocab = Dict{String, Tuple{UInt32, UInt32}}()
open("vocab.bin", "r") do file
    while !eof(file)
        term_len = UInt(read(file, UInt8))
        term = String(read(file, term_len))
        read(file, UInt8) # Null terminated

        whence = read(file, UInt32)
        offset = read(file, UInt32)

        vocab[term] = (whence, offset)
    end
end

# Open the postings list file
postings = Vector{UInt32}(undef, length(doc_ids) * 2)
postings_fh = open("postings.bin", "r")

# Allocate buffers
rsv = Vector{Float64}(undef, length(doc_ids))
# Set up the rsv pointers
rsv_pointers = collect(1:length(doc_ids))

# Search (one query per line)
for query in eachline()
    # Zero the accumulators array
    fill!(rsv, 0)

    terms = split(query)

    # If the first token is a number then assume a TREC query number, and skip it
    query_id = tryparse(Int, terms[1])
    if isnothing(query_id)
        query_id = 0
    else
        popfirst!(terms)
    end

    for term in terms
        offset, size = get(vocab, term, (nothing, nothing))
        postings_length = div(size, 8)
        # Does the term exist in the collection?
        if isnothing(offset)
            continue
        end

        # Seek and read the postings list
        seek(postings_fh, offset)
        postings_view = @view postings[1:postings_length*2]
        read!(postings_fh, postings_view)

        # Compute the IDF component of BM25 as log(N/n).
        idf = log(Float64(length(doc_ids)) / postings_length)

        # Process the postings list by simply adding the BM25 component for this document into the accumulators array
        for (docid, tf) in Iterators.partition(postings_view, 2)
            rsv[docid+1] += idf * ((tf * (k1 + 1)) / (tf + k1 * (1 - b + b * (doc_lengths[docid+1] / average_length))))
        end
    end

    # Sort the results list
    sort!(rsv_pointers, by = i -> (rsv[i], i), rev = true)

    # Print the (at most) top 1000 documents in the results list in TREC eval format which is:
    # query-id Q0 document-id rank score run-name
    for (i, docid) in enumerate(first(rsv_pointers, 1000))
        if iszero(rsv[docid])
            break
        end
        println("$(query_id) Q0 $(doc_ids[docid]) $(i) $(@sprintf("%.4f", rsv[docid])) JASSjr")
    end
end
