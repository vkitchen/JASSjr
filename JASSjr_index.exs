#!/usr/bin/env -S ERL_FLAGS="+B +hms 1073741824" elixir
# +B disables interrupt handler
# +hms sets default heap size (8gb)

# Copyright (c) 2024 Vaughan Kitchen
# Minimalistic BM25 search engine.

defmodule Index do
  defstruct doclength: 0, # length of currently indexing document
  docid: -1, # cache last index into primary keys
  doclengths: [], # hold the length of each document
  docids: [], # the primary keys
  vocab: %{}, # the in-memory index (terms => <tf, docid>)
  pushnext: false # is the next token the primary key?

  # Add the posting to the in-memory index
  def append(index, term) do
    %Index{index | doclength: index.doclength + 1, vocab: Map.update(index.vocab, term, [ 1, index.docid ], fn [ tf | [ doc | tail ]] = docnos ->
      if doc != index.docid do
        # if the docno for this occurence has changed then create a new <d,tf> pair
        [ 1 | [ index.docid | docnos ]]
      else
        # else increase the tf
        [ tf + 1 | [ doc | tail ]]
      end
    end)}
  end
end

defmodule Indexer do
  # One-character lookahead lexical analyser
  def parse(<<>>, index), do: index
  def parse(line, index) do
    # A token is either an XML tag '<'..'>' or a sequence of alpha-numerics.
    # TREC <DOCNO> primary keys have a hyphen in them
    Regex.scan(~r/[a-zA-Z0-9][a-zA-Z0-9-]*|<[^>]*>/, line)
    |> Enum.reduce(index, fn [ token | _ ], acc ->
      # If we see a <DOC> tag then we're at the start of the next document
      cond do
        token == "<DOC>" ->
          # Save the previous document length
          acc = if acc.docid != -1 do
            %Index{acc | doclengths: [ acc.doclength | acc.doclengths]}
          else
            acc
          end

          # Move on to the next document
          acc = %Index{acc | docid: acc.docid + 1, doclength: 0}

          if acc.docid |> rem(1000) == 0 do
            IO.puts("#{acc.docid} documents indexed")
          end

          acc
        # if the last token we saw was a <DOCNO> then the next token is the primary key
        acc.pushnext ->
          # Include the primary key as a term to match the other indexers
          acc = %Index{acc | docids: [ token | acc.docids]}
          acc = Index.append(acc, token)
          %Index{acc | pushnext: false }
        token == "<DOCNO>" ->
          %Index{acc | pushnext: true }
        # Don't index XML tags
        String.first(token) == "<" ->
          acc
        true ->
           Index.append(acc, String.downcase(token)) # lower case the string
      end
    end)
  end

  # serialise the in-memory index to disk
  def serialise(index) do
    # Save the final document length
    index = %Index{index | doclengths: [ index.doclength | index.doclengths]}

    # Tell the user we've got to the end of parsing
    IO.puts("Indexed #{index.docid} documents. Serialising...")

    docids = Enum.reverse(index.docids)

    # store the primary keys
    File.open!("docids.bin", [:write], fn file ->
      Enum.each(docids, fn docid ->
        IO.write(file, "#{docid}\n")
      end)
    end)
    vocab = File.open!("vocab.bin", [:write])
    postings = File.open!("postings.bin", [:write])
    Enum.each(index.vocab, fn {term, posts} ->
      # write the postings list to one file
      posts = Enum.reverse(posts)
      posts = for x <- posts, do: <<x::native-32>>, into: <<>>
      {:ok, where} = :file.position(postings, {:cur, 0})
      IO.binwrite(postings, posts)
      # write the vocabulary to a second file (one byte length, string, '\0', 4 byte where, 4 byte size)
      IO.binwrite(vocab, <<byte_size(term)::8, term::binary, 0::8, where::native-32, byte_size(posts)::native-32>>)
    end)
    # clean up
    :ok = File.close(postings)
    :ok = File.close(vocab)

    # store the document doclengths
    doclengths = Enum.reverse(index.doclengths)
    doclengths = for x <- doclengths, do: <<x::native-32>>, into: <<>>
    File.open!("lengths.bin", [:write], fn file ->
      IO.binwrite(file, doclengths)
    end)
  end

  def start() do
    # Make sure we have one parameter, the filename
    if length(System.argv()) != 1 do
      IO.puts("Usage: ./JASSjr_index.exs <infile.xml>")
      System.halt()
    end

    # Read the file to index
    [ filename | _ ] = System.argv()
    File.stream!(filename)
    |> Enum.reduce(%Index{}, &parse/2)
    |> serialise
  end
end

Indexer.start()
