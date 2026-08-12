// Copyright (c) 2024, 2026 Vaughan Kitchen
// Minimalistic BM25 search engine.

const std = @import("std");
const native_endian = @import("builtin").target.cpu.arch.endian();

const k1 = 0.9; // BM25 k1 parameter
const b = 0.4; // BM25 b parameter

fn compare_rsv(rsv: []f64, first: usize, second: usize) bool {
    return if (rsv[first] == rsv[second]) first > second else rsv[first] > rsv[second];
}

// The Zig standard library as of 0.12.0-dev.3639+9cfac4718 doesn't randomise the hash seed
// Due to the hash table implementation writing out and reading back with the same seed value is incredibly slow
// Here we provide a custom hash table context so that we can hash to different values
const StringContext = struct {
    pub fn hash(self: @This(), str: []const u8) u64 {
        _ = self;
        return std.hash.Wyhash.hash(42, str);
    }
    pub fn eql(self: @This(), first: []const u8, second: []const u8) bool {
        _ = self;
        return std.mem.eql(u8, first, second);
    }
};

// Simple search engine ranking on BM25.
pub fn main(init: std.process.Init) !void {
    var arena = init.arena.allocator();
    var stdout_buffer: [1024]u8 = undefined;
    var stdout_writer = std.Io.File.stdout().writer(init.io, &stdout_buffer);
    const stdout = &stdout_writer.interface;

    // Read the document lengths
    const doc_lengths: []u32 = std.mem.bytesAsSlice(u32, try std.Io.Dir.cwd().readFileAllocOptions(init.io, "lengths.bin", arena, std.Io.Limit.unlimited, .@"4", null));

    // Compute the average document length for BM25
    var average_document_length: f64 = 0;
    for (doc_lengths) |val| average_document_length += @floatFromInt(val);
    average_document_length /= @floatFromInt(doc_lengths.len);

    // Read the primary keys
    var fh_buffer: [1024]u8 = undefined;
    var fh = try std.Io.Dir.cwd().openFile(init.io, "docids.bin", .{});
    var stream = fh.reader(init.io, &fh_buffer);

    var primary_keys = try arena.alloc([]u8, doc_lengths.len);

    var key_index: usize = 0;
    while (try stream.interface.takeDelimiter('\n')) |line| {
        const docid = try arena.dupe(u8, line);
        primary_keys[key_index] = docid;
        key_index += 1;
    }

    fh.close(init.io);

    // Build the vocabulary in memory
    fh = try std.Io.Dir.cwd().openFile(init.io, "vocab.bin", .{});
    stream = fh.reader(init.io, &fh_buffer);

    var vocab = std.HashMap([]const u8, struct { u32, u32 }, StringContext, std.hash_map.default_max_load_percentage).init(arena);

    while (true) {
        const len = stream.interface.takeByte() catch |err| switch (err) {
            error.EndOfStream => break,
            else => return err,
        };
        const term = try arena.alloc(u8, len);
        _ = stream.interface.readSliceAll(term) catch unreachable;
        _ = stream.interface.takeByte() catch unreachable;

        const where = stream.interface.takeInt(u32, native_endian) catch unreachable;
        const size = stream.interface.takeInt(u32, native_endian) catch unreachable;

        try vocab.put(term, .{ where, size });
    }

    fh.close(init.io);

    // Open the postings list file
    var postings_buffer: [1024]u8 = undefined;
    const postings = try arena.alloc(struct { u32, u32 }, doc_lengths.len * 2);
    const postings_fh = try std.Io.Dir.cwd().openFile(init.io, "postings.bin", .{});
    var postings_reader = postings_fh.reader(init.io, &postings_buffer);
    defer postings_fh.close(init.io);

    // Allocate buffers
    var rsv = try arena.alloc(f64, doc_lengths.len);

    // Set up the rsv pointers
    var rsv_pointers = try arena.alloc(usize, doc_lengths.len);
    for (0..doc_lengths.len) |i| {
        rsv_pointers[i] = i;
    }

    // Search (one query per line)
    var query_buffer: [1024]u8 = undefined;
    var stdin_reader = std.Io.File.stdin().reader(init.io, &query_buffer);
    const stdin = &stdin_reader.interface;
    while (try stdin.takeDelimiter('\n')) |line| {
        // Zero the accumulator array
        for (0..doc_lengths.len) |i| {
            rsv[i] = 0;
        }

        var it = std.mem.splitAny(u8, line, " \r\n");

        // If the first token is a number then assume a TREC query number, and skip it
        const query_id = std.fmt.parseInt(isize, it.peek().?, 10) catch 0;
        if (query_id != 0) _ = it.next();

        while (it.next()) |term| {
            if (term.len == 0) continue;
            // Does the term exist in the collection?
            if (vocab.get(term)) |pair| {
                // Seek and read the postings list
                try postings_reader.seekTo(pair[0]);
                _ = try postings_reader.interface.readSliceAll(std.mem.sliceAsBytes(postings)[0..pair[1]]);

                // Compute the IDF component of BM25 as log(N/n)
                const idf = @log(@as(f64, @floatFromInt(doc_lengths.len)) / @as(f64, @floatFromInt(pair[1] / 8)));

                // Process the postings list by simply adding the BM25 component for this document into the accumulators array
                for (postings, 0..) |p, i| {
                    if (i == pair[1] / 8) break;
                    const docid = p[0];
                    const tf: f64 = @floatFromInt(p[1]);
                    rsv[docid] += idf * tf * (k1 + 1) / (tf + k1 * (1 - b + b * (@as(f64, @floatFromInt(doc_lengths[docid])) / average_document_length)));
                }
            }
        }

        // Sort the results list
        std.sort.pdq(usize, rsv_pointers, rsv, compare_rsv);

        // Print the (at most) top 1000 documents in the results list in TREC eval format which is:
        // query-id Q0 document-id rank score run-name
        for (rsv_pointers, 0..) |r, i| {
            if (rsv[r] == 0 or i == 1000) break;
            try stdout.print("{d} Q0 {s} {d} {d:.4} JASSjr\n", .{ query_id, primary_keys[r], i + 1, rsv[r] });
        }
        try stdout_writer.flush();
    }
}
