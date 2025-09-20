const std = @import("std");
const board = @import("board");

const Alloc = std.mem.Allocator;
const Move = board.Move;

pub const TTEntry = struct {
    key: u64,
    depth: u8,
    score: i16,
    move: Move,
    bound: Bound,
};

pub const Bound = enum {
    lower,
    exact,
    upper,
};

// TODO group entries into buckets?

const TT_DEFAULT_SIZE = 1<<22;

pub const TTable = struct {
    entries: []TTEntry,

    const Self = @This();

    // TODO pass size as paramether (in mb?)

    pub fn init(alloc: Alloc) !Self {
        const tt = try alloc.alloc(TTEntry, TT_DEFAULT_SIZE);

        return .{ .entries = tt };
    }

    pub fn deinit(self: *Self, alloc: Alloc) void {
        alloc.free(self.entries);
    }

    // TODO resize
    
    fn entryIndex(self: *const Self, zobrist_key: u64) u64 {
        return zobrist_key & (self.entries.len - 1);
    }

    pub fn probe(self: *const Self, zobrist_key: u64, ply: u32) ?*const TTEntry {
        const tt_index = self.entryIndex(zobrist_key);
        const tt_entry = &self.entries[tt_index];
        if (tt_entry.key != zobrist_key)
            return null;

        // TODO correct mate score
        _ = ply; 
        return tt_entry;
    }

    pub fn put(self: *Self, zobrist_key: u64, ply: u32, depth: u8, score: i16, bound: Bound, move: Move) void {
        // TODO correct mate score
        _ = ply; 

        const tt_index = self.entryIndex(zobrist_key);
        const tt_entry = &self.entries[tt_index];

        if (tt_entry.key != zobrist_key or tt_entry.depth <= depth) {
            tt_entry.* = .{
                .move = move,
                .score = score,
                .depth = depth,
                .key = zobrist_key,
                .bound = bound,
            };
        }
    }
};
