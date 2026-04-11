const std = @import("std");
const board = @import("board");

const Alloc = std.mem.Allocator;
const Move = board.Move;

pub const TTEntry = struct {
    // lower 16 bits of tt key
    key_low: u16 = 0,
    depth: u8 = 0,
    score: i16 = 0,
    move: Move = .NULL,
    bound: Bound = .lower, 
};

pub const TTBucket = struct {
    entries: [4]TTEntry align(32),
};

comptime {
    std.debug.assert(@sizeOf(TTEntry) == 8);
    std.debug.assert(@alignOf(TTEntry) == 2);
    std.debug.assert(@sizeOf(TTBucket) == 32);
    std.debug.assert(@alignOf(TTBucket) == 32);
}

pub const Bound = enum (u2) {
    lower = 0,
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
        @memset(tt, .{ .entries = [1]TTEntry{ .{} } ** 4 });

        return .{ .buckets = tt };
    }

    pub fn deinit(self: *Self, alloc: Alloc) void {
        alloc.free(self.buckets);
    }

    // TODO resize
    
    fn indicies(self: *const Self, zobrist_key: u64) struct { u32, u16 } {
        // TODO pick a better one?
        const TT_MAGICK = 0x35C25ADD731CCD89;
        const key: u64 = (zobrist_key *% TT_MAGICK) >> 8;
        const index: u32 = @intCast(key >> 16 & (self.buckets.len - 1));
        const lower_key: u16 = @intCast(key & 0xFFFF);
        return .{ index, lower_key };
    }

    pub fn probe(self: *const Self, zobrist_key: u64, ply: u32) ?*const TTEntry {
        // TODO correct mate score
        _ = ply; 

        const tt_index, const lower_key = self.indicies(zobrist_key);
        const tt_bucket = &self.buckets[tt_index];

        for (&tt_bucket.entries) |*tt_entry| {
            if (tt_entry.key_low == lower_key)
                return tt_entry;
        }

        return null;
    }

    pub fn put(self: *Self, zobrist_key: u64, ply: u32, depth: u8, score: i16, bound: Bound, move: Move) void {
        // TODO correct mate score
        _ = ply; 

        const tt_index, const lower_key = self.indicies(zobrist_key);
        const tt_bucket = &self.buckets[tt_index];

        var entry_location: ?*TTEntry = null;

        for (&tt_bucket.entries) |*tt_entry| {
            if (tt_entry.key_low == lower_key) {
                if (tt_entry.depth > depth)
                    return;

                entry_location = tt_entry;
                break;
            }
        }

        if (entry_location == null) {
            @memmove(tt_bucket.entries[1..4], tt_bucket.entries[0..3]);
            entry_location = &tt_bucket.entries[0];
        }

        entry_location.?.* = .{
            .move = move,
            .key_low = lower_key,
            //.is_occupied = true,
            .score = score,
            .depth = depth,
            .bound = bound,
        };
    }
};
