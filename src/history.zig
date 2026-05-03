const std = @import("std");
const board = @import("board");
const search = @import("search.zig");

const Alloc = std.mem.Allocator;
const Move = board.Move;
const PieceKind = board.PieceKind;
const Board = board.Board;

pub const History = struct {
    pub const Entry = struct {
        quiet_hist: i16 = 0,
        noisy_hist: i16 = 0,
    };

    // TODO experiment with non-butterfly tables
    butterfly_entries: [2 * 64 * 64]Entry,

    const Self = @This();

    pub fn reset(self: *Self) void {
        @memset(&self.butterfly_entries, .{});
    }

    pub fn butterflyIndex(color: board.SideToMove, move: Move) usize {
        return @as(usize, @intFromEnum(color)) << 12 | @as(usize, move.from) << 6 | @as(usize, move.to);
    }

    pub fn getQuiet(self: *const Self, color: board.SideToMove, move: Move) i16 {
        const i = butterflyIndex(color, move);
        return self.butterfly_entries[i].quiet_hist;
    }

    pub fn getNoisy(self: *const Self, color: board.SideToMove, move: Move) i16 {
        const i = butterflyIndex(color, move);
        return self.butterfly_entries[i].noisy_hist;
    }

    pub fn updateQuiet(self: *Self, color: board.SideToMove, move: Move, raw_bonus: i32) void {
        const i = butterflyIndex(color, move);
        const MAX_BONUS = search.MOVESCORE_KILLER;
        const bonus = @max(@min(raw_bonus, MAX_BONUS), -MAX_BONUS);
        const value = &self.butterfly_entries[i].quiet_hist;
        value.* +|= @intCast(bonus - @divTrunc(@as(i32, value.*) * @as(i32, @intCast(@abs(bonus))), MAX_BONUS));
    }

    pub fn updateNoisy(self: *Self, color: board.SideToMove, move: Move, raw_bonus: i32) void {
        const i = butterflyIndex(color, move);
        const MAX_BONUS = search.MOVESCORE_KILLER;
        const bonus = @max(@min(raw_bonus, MAX_BONUS), -MAX_BONUS);
        const value = &self.butterfly_entries[i].noisy_hist;
        value.* +|= @intCast(bonus - @divTrunc(@as(i32, value.*) * @as(i32, @intCast(@abs(bonus))), MAX_BONUS));
    }
};
