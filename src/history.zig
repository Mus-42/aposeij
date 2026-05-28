const std = @import("std");
const board = @import("board.zig");
const search = @import("search.zig");

const Alloc = std.mem.Allocator;
const Move = board.Move;
const PieceKind = board.PieceKind;
const Board = board.Board;

pub const History = struct {
    // TODO experiment with non-butterfly tables
    quiet: [2][64][64]i16,
    noisy: [12][64][64]i16,

    const Self = @This();

    pub fn reset(self: *Self) void {
        @memset(std.mem.sliceAsBytes(&self.quiet), 0);
        @memset(std.mem.sliceAsBytes(&self.noisy), 0);
    }

    pub fn getQuiet(self: *Self, bd: *const board.Board.BoardData, move: Move) *i16 {
        return &self.quiet[@intFromEnum(bd.side_to_move)][move.from][move.to];
    }

    pub fn getNoisy(self: *Self, bd: *const board.Board.BoardData, move: Move) *i16 {
        const piece = bd.getPieceAt(move.from).?;
        return &self.noisy[@intFromEnum(piece)][move.from][move.to];
    }

    pub fn updateQuiet(self: *Self, bd: *const board.Board.BoardData, move: Move, raw_bonus: i32) void {
        const MAX_BONUS = search.MOVESCORE_KILLER;
        const bonus = @max(@min(raw_bonus, MAX_BONUS), -MAX_BONUS);
        const value = self.getQuiet(bd, move);
        value.* +|= @intCast(bonus - @divTrunc(@as(i32, value.*) * @as(i32, @intCast(@abs(bonus))), MAX_BONUS));
    }

    pub fn updateNoisy(self: *Self, bd: *const board.Board.BoardData, move: Move, raw_bonus: i32) void {
        const MAX_BONUS = 0x1000;//search.MOVESCORE_KILLER;
        const bonus = @max(@min(raw_bonus, MAX_BONUS), -MAX_BONUS);
        const value = self.getNoisy(bd, move);
        value.* +|= @intCast(bonus - @divTrunc(@as(i32, value.*) * @as(i32, @intCast(@abs(bonus))), MAX_BONUS));
    }
};
