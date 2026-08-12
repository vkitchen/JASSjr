// Copyright (c) 2024, 2026 Vaughan Kitchen
// Minimalistic BM25 search engine.

const std = @import("std");
const native_endian = @import("builtin").target.cpu.arch.endian();

// Pair definition for a value in a postings list
const Posting = struct { i32, i32 };

// One-character lookahead lexical analyser
const Lexer = struct {
    buffer: []u8,
    current: usize,

    fn init(buffer: []u8) Lexer {
        return Lexer{ .buffer = buffer, .current = 0 };
    }

    // Conform to Zig naming conventions for iterators
    fn next(self: *Lexer) ?[]u8 {
        // Skip over whitespace and punctuation (but not XML tags)
        while (self.current < self.buffer.len and !std.ascii.isAlphanumeric(self.buffer[self.current]) and self.buffer[self.current] != '<')
            self.current += 1;

        // A token is either an XML tag '<'..'>' or a sequence of alpha-numerics.
        const start = self.current;
        if (self.current >= self.buffer.len) {
            return null; // must be at end of line
        } else if (std.ascii.isAlphanumeric(self.buffer[self.current])) {
            // TREC <DOCNO> primary keys have a hyphen in them
            while (self.current < self.buffer.len and (std.ascii.isAlphanumeric(self.buffer[self.current]) or self.buffer[self.current] == '-'))
                self.current += 1;
        } else if (self.buffer[self.current] == '<') {
            self.current += 1;
            while (self.current < self.buffer.len and self.buffer[self.current - 1] != '>')
                self.current += 1;
        }

        // Return the token as a slice. This won't live past the underlying buffer
        return self.buffer[start..self.current];
    }
};

// Simple indexer for TREC WSJ collection
pub fn main(init: std.process.Init) !void {
    var arena = init.arena.allocator();
    var stdout_buffer: [1024]u8 = undefined;
    var stdout_writer = std.Io.File.stdout().writer(init.io, &stdout_buffer);
    const stdout = &stdout_writer.interface;

    const argv = try init.minimal.args.toSlice(arena);

    // Make sure we have one parameter, the filename
    if (argv.len != 2) {
        try stdout.print("Usage: {s} <infile.xml>\n", .{argv[0]});
        try stdout_writer.flush();
        std.process.exit(0);
    }

    var vocab = std.StringHashMap(std.ArrayList(Posting)).init(arena);
    var doc_ids: std.ArrayList([]u8) = .empty;
    var doc_lengths: std.ArrayList(i32) = .empty;

    var doc_id: i32 = -1;
    var document_length: i32 = 0;

    var fh_buffer: [2048]u8 = undefined;
    var fh = try std.Io.Dir.cwd().openFile(init.io, argv[1], .{});
    var stream = fh.reader(init.io, &fh_buffer);

    var push_next = false;
    while (try stream.interface.takeDelimiter('\n')) |line| {
        var lex = Lexer.init(line);
        while (lex.next()) |token| {
            if (std.mem.eql(u8, token, "<DOC>")) {
                // Save the previous document length
                if (doc_id != -1)
                    try doc_lengths.append(arena, document_length);
                // Move on to the next document
                doc_id += 1;
                document_length = 0;
                if (@rem(doc_id, 1000) == 0) {
                    try stdout.print("{d} documents indexed\n", .{doc_id});
                    try stdout_writer.flush();
                }
            }
            // If the last token we saw was a <DOCNO> then the next token is the primary key
            if (push_next) {
                const primary_key = try arena.dupe(u8, token);
                try doc_ids.append(arena, primary_key);
                push_next = false;
            }
            if (std.mem.eql(u8, token, "<DOCNO>")) {
                push_next = true;
            }
            // Don't index XML tags
            if (token[0] == '<')
                continue;

            // Lower case the string
            _ = std.ascii.lowerString(token, token);

            // Truncate any long tokens at 255 characters (so that the length can be stored first and in a single byte)
            const token2 = if (token.len < 255) token else token[0..255];

            // Add the posting to the in-memory index
            const gop = try vocab.getOrPut(token2);
            if (!gop.found_existing) {
                // If the term isn't in the vocab yet
                const term = try arena.dupe(u8, token2);
                gop.key_ptr.* = term;
                gop.value_ptr.* = .empty;
                try gop.value_ptr.append(arena, .{ doc_id, 1 });
            } else {
                if (gop.value_ptr.getLast()[0] != doc_id) {
                    // If the docno for this occurence has changed then create a new <d,tf> pair
                    try gop.value_ptr.append(arena, .{ doc_id, 1 });
                } else {
                    // Else increase the tf
                    gop.value_ptr.items[gop.value_ptr.items.len - 1][1] += 1;
                }
            }

            // Compute the document length
            document_length += 1;
        }
    }

    // Save the final document length
    try doc_lengths.append(arena, document_length);

    // Tell the user we've got to the end of parsing
    try stdout.print("Indexed {d} documents. Serialising...\n", .{doc_id + 1});
    try stdout_writer.flush();

    // Store the primary keys
    var docids_buffer: [1024]u8 = undefined;
    const docids_fh = try std.Io.Dir.cwd().createFile(init.io, "docids.bin", .{});
    var docids_stream = docids_fh.writer(init.io, &docids_buffer);

    for (doc_ids.items) |primary_key| {
        try docids_stream.interface.writeAll(primary_key);
        try docids_stream.interface.writeByte('\n');
    }

    // Serialise the in-memory index to disk
    var postings_buffer: [1024]u8 = undefined;
    const postings_fh = try std.Io.Dir.cwd().createFile(init.io, "postings.bin", .{});
    var postings_stream = postings_fh.writer(init.io, &postings_buffer);

    var vocab_buffer: [1024]u8 = undefined;
    const vocab_fh = try std.Io.Dir.cwd().createFile(init.io, "vocab.bin", .{});
    var vocab_stream = vocab_fh.writer(init.io, &vocab_buffer);

    var where: usize = 0;
    var it = vocab.iterator();
    while (it.next()) |kv| {
        // Write the postings list to one file
        try postings_stream.interface.writeAll(std.mem.sliceAsBytes(kv.value_ptr.items));

        // Write the vocabulary to a second file (one byte length, string, '\0', 4 byte where, 4 byte size)
        try vocab_stream.interface.writeByte(@truncate(kv.key_ptr.len));
        try vocab_stream.interface.writeAll(kv.key_ptr.*);
        try vocab_stream.interface.writeByte(0);
        try vocab_stream.interface.writeInt(u32, @truncate(where), native_endian);
        try vocab_stream.interface.writeInt(u32, @truncate(kv.value_ptr.items.len * 8), native_endian);

        where += kv.value_ptr.items.len * 8;
    }

    // Store the document lengths
    var lengths_buffer: [1024]u8 = undefined;
    const lengths_fh = try std.Io.Dir.cwd().createFile(init.io, "lengths.bin", .{});
    var lengths_stream = lengths_fh.writer(init.io, &lengths_buffer);
    try lengths_stream.interface.writeAll(std.mem.sliceAsBytes(doc_lengths.items));

    // Cleanup
    try lengths_stream.flush();
    lengths_fh.close(init.io);

    try vocab_stream.flush();
    vocab_fh.close(init.io);

    try postings_stream.flush();
    postings_fh.close(init.io);

    try docids_stream.flush();
    docids_fh.close(init.io);
}
