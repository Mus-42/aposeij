const std = @import("std");
const board = @import("board.zig");
const search = @import("search.zig");

const Alloc = std.mem.Allocator;
const Move = board.Move;

pub const TTEntry = struct {
    // lower 16 bits of tt key
    depth: u8,
    score: i16,
    static_eval: i16,
    move: Move,
    bound: Bound, 
};

pub const TTBucket = struct {
    key_low: [3]u16 align(32),
    entries: [3]TTEntry,
};

comptime {
    std.debug.assert(@sizeOf(TTEntry) == 8);
    std.debug.assert(@alignOf(TTEntry) == 2);
    std.debug.assert(@sizeOf(TTBucket) == 32);
    std.debug.assert(@alignOf(TTBucket) == 32);
}

pub const Bound = enum (u2) {
    lower,
    exact,
    upper,
};

const TT_DEFAULT_SIZE_MB = 64;

pub const TTable = struct {
    buckets: []TTBucket,

    stats: struct {
        hits: u64 = 0,
        misses: u64 = 0,
        collisions: u64 = 0,
    } = .{},

    const Self = @This();

    // TODO pass size as paramether (in mb?)

    pub fn init(alloc: Alloc) !Self {
        const size_buckets = (TT_DEFAULT_SIZE_MB << 20) / @sizeOf(TTBucket);

        const tt = try alloc.alloc(TTBucket, size_buckets);

        var self: Self = .{ .buckets = tt };
        self.clear();

        return self;
    }

    pub fn clear(self: *Self) void {
        @memset(std.mem.sliceAsBytes(self.buckets), 0);
        self.stats = .{};
    }

    pub fn resize(self: *Self, alloc: Alloc, tt_size_in_mb: usize) !void {
        const size_buckets = (tt_size_in_mb << 20) / @sizeOf(TTBucket);
        const new_tt = try alloc.alloc(TTBucket, size_buckets);
        errdefer alloc.free(new_tt);
        alloc.free(self.buckets);
        self.buckets = new_tt;
        // TODO copy old buckets into new instead?
        self.clear();
    }

    pub fn deinit(self: *Self, alloc: Alloc) void {
        alloc.free(self.buckets);
    }

    // TODO resize function

    // A:
    // (old_ply - MATE_ABS) - old_ply + new_ply
    // B:
    // (MATE_ABS - old_ply) + old_ply - new_ply

    fn restoreScore(stored_score: i16, ply: u32) i16 {
        var score = stored_score;

        if (search.scoreToMateInPlyAbs(score) != null) {
            if (score > 0) {
                score -= @intCast(ply);
            } else {
                score += @intCast(ply);
            }
        }

        return score;
    }

    fn storeScore(raw_score: i16, ply: u32) i16 {
        var score = raw_score;
        if (search.scoreToMateInPlyAbs(score) != null) {
            if (score > 0) {
                score += @intCast(ply);
            } else {
                score -= @intCast(ply);
            }
        }
        return score;
    }

    fn indicies(self: *const Self, zobrist_key: u64) struct { u32, u16 } {
        // TODO pick a better one?
        const TT_MAGICK = 0xEFECAB0DD6865F75;
        const key: u64 = zobrist_key *% TT_MAGICK;
        const index: u32 = @intCast((key >> 32) % self.buckets.len);
        const lower_key: u16 = @intCast((key >> 48) & 0xFFFF);
        return .{ index, lower_key };
    }

    pub fn probe(self: *Self, zobrist_key: u64, ply: u32) ?TTEntry {
        const tt_index, const lower_key = self.indicies(zobrist_key);
        const tt_bucket = &self.buckets[tt_index];

        var pos: usize = 3;
        inline for (0..3) |i| {
            if (tt_bucket.key_low[i] == lower_key) {
                pos = i;
                break;
            }
        }

        if (pos >= 3) {
            self.stats.misses += 1;
            return null;
        }
        self.stats.hits += 1;
        var entry = tt_bucket.entries[pos];
        entry.score = restoreScore(entry.score, ply);
        return entry;
    }

    pub fn put(self: *Self, zobrist_key: u64, ply: u32, depth: u8, score: i16, static_eval: i16, bound: Bound, move: Move) void {
        const tt_index, const lower_key = self.indicies(zobrist_key);
        const tt_bucket = &self.buckets[tt_index];

        var entry_i: usize = 3;
        inline for (0..3) |i| {
            if (tt_bucket.key_low[i] == lower_key) {
                // if (tt_bucket.entries[i].depth > depth) return;
                self.stats.collisions += 1;
                entry_i = i;
                break;
            }
        }

        if (entry_i >= 3) {
            tt_bucket.entries[1..3].* = tt_bucket.entries[0..2].*;
            tt_bucket.key_low[1..3].* = tt_bucket.key_low[0..2].*;
            entry_i = 0;
        }

        tt_bucket.key_low[entry_i] = lower_key;
        tt_bucket.entries[entry_i] = .{
            .move = move,
            .score = storeScore(score, ply),
            .static_eval = static_eval,
            .depth = depth,
            .bound = bound,
        };
    }
};
