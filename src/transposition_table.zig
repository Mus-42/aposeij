const std = @import("std");
const board = @import("board");
const search = @import("search.zig");

const Alloc = std.mem.Allocator;
const Move = board.Move;

pub const TTEntry = struct {
    // lower 16 bits of tt key
    depth: u8,
    score: i16,
    move: Move,
    bound: Bound, 
};

pub const TTBucket = struct {
    key_low: [4]u16 align(32),
    entries: [4]TTEntry,
};

comptime {
    std.debug.assert(@sizeOf(TTEntry) == 6);
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
    }

    pub fn deinit(self: *Self, alloc: Alloc) void {
        alloc.free(self.buckets);
    }

    // TODO resize function

    fn isMateScore(score: i16) bool {
        const abs_score: i16 = @intCast(@abs(score));
        return search.SCORE_MATE_ABS - abs_score <= search.SCORE_MATE_EPS;
    }

    // A:
    // (old_ply - MATE_ABS) - old_ply + new_ply
    // B:
    // (MATE_ABS - old_ply) + old_ply - new_ply

    fn restoreScore(stored_score: i16, ply: u32) i16 {
        var score = stored_score;
        if (isMateScore(score)) {
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
        if (isMateScore(score)) {
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
        const TT_MAGICK = 0x35C25ADD731CCD89;
        const key: u64 = (zobrist_key *% TT_MAGICK) >> 16;
        const index: u32 = @intCast((key >> 16) % self.buckets.len);
        const lower_key: u16 = @intCast(key & 0xFFFF);
        return .{ index, lower_key };
    }

    pub fn probe(self: *const Self, zobrist_key: u64, ply: u32) ?TTEntry {
        const tt_index, const lower_key = self.indicies(zobrist_key);
        const tt_bucket = &self.buckets[tt_index];

        var pos: usize = 4;
        inline for (0..4) |i| {
            if (tt_bucket.key_low[i] == lower_key) {
                pos = i;
                break;
            }
        }

        if (pos >= 4) return null;
        var entry = tt_bucket.entries[pos];
        entry.score = restoreScore(entry.score, ply);
        return entry;
    }

    pub fn put(self: *Self, zobrist_key: u64, ply: u32, depth: u8, score: i16, bound: Bound, move: Move) void {
        const tt_index, const lower_key = self.indicies(zobrist_key);
        const tt_bucket = &self.buckets[tt_index];

        var entry_i: usize = 4;
        inline for (0..4) |i| {
            if (tt_bucket.key_low[i] == lower_key) {
                entry_i = i;
                break;
            }
        }

        if (entry_i >= 4) {
            tt_bucket.entries[1..4].* = tt_bucket.entries[0..3].*;
            tt_bucket.key_low[1..4].* = tt_bucket.key_low[0..3].*;
            entry_i = 0;
        }

        tt_bucket.key_low[entry_i] = lower_key;
        tt_bucket.entries[entry_i] = .{
            .move = move,
            .score = storeScore(score, ply),
            .depth = depth,
            .bound = bound,
        };
    }
};
