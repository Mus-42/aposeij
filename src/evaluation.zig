const std = @import("std");
const board = @import("board");

const Alloc = std.mem.Allocator;
const Move = board.Move;
const PieceKind = board.PieceKind;
const Board = board.Board;

pub const PIECE_COST = [6]i16{
    100,
    320,
    330,
    500,
    900,
    0
};

const BONUS_TABLES: [6][64]i8 = .{
    .{
        0,  0,  0,  0,  0,  0,  0,  0,
        50, 50, 50, 50, 50, 50, 50, 50,
        10, 10, 20, 30, 30, 20, 10, 10,
        5,  5, 10, 25, 25, 10,  5,  5,
        0,  0,  0, 20, 20,  0,  0,  0,
        5, -5,-10,  0,  0,-10, -5,  5,
        5, 10, 10,-20,-20, 10, 10,  5,
        0,  0,  0,  0,  0,  0,  0,  0
    },
    .{
        -50,-40,-30,-30,-30,-30,-40,-50,
        -40,-20,  0,  0,  0,  0,-20,-40,
        -30,  0, 10, 15, 15, 10,  0,-30,
        -30,  5, 15, 20, 20, 15,  5,-30,
        -30,  0, 15, 20, 20, 15,  0,-30,
        -30,  5, 10, 15, 15, 10,  5,-30,
        -40,-20,  0,  5,  5,  0,-20,-40,
        -50,-40,-30,-30,-30,-30,-40,-50,
    },
    .{
        -20,-10,-10,-10,-10,-10,-10,-20,
        -10,  0,  0,  0,  0,  0,  0,-10,
        -10,  0,  5, 10, 10,  5,  0,-10,
        -10,  5,  5, 10, 10,  5,  5,-10,
        -10,  0, 10, 10, 10, 10,  0,-10,
        -10, 10, 10, 10, 10, 10, 10,-10,
        -10,  5,  0,  0,  0,  0,  5,-10,
        -20,-10,-10,-10,-10,-10,-10,-20,

    },
    .{
        0,  0,  0,  0,  0,  0,  0,  0,
        5, 10, 10, 10, 10, 10, 10,  5,
        -5,  0,  0,  0,  0,  0,  0, -5,
        -5,  0,  0,  0,  0,  0,  0, -5,
        -5,  0,  0,  0,  0,  0,  0, -5,
        -5,  0,  0,  0,  0,  0,  0, -5,
        -5,  0,  0,  0,  0,  0,  0, -5,
        0,  0,  0,  5,  5,  0,  0,  0
    },
    .{
        -20,-10,-10, -5, -5,-10,-10,-20,
        -10,  0,  0,  0,  0,  0,  0,-10,
        -10,  0,  5,  5,  5,  5,  0,-10,
        -5,  0,  5,  5,  5,  5,  0, -5,
        0,  0,  5,  5,  5,  5,  0, -5,
        -10,  5,  5,  5,  5,  5,  0,-10,
        -10,  0,  5,  0,  0,  0,  0,-10,
        -20,-10,-10, -5, -5,-10,-10,-20
    },
    .{
        -30,-40,-40,-50,-50,-40,-40,-30,
        -30,-40,-40,-50,-50,-40,-40,-30,
        -30,-40,-40,-50,-50,-40,-40,-30,
        -30,-40,-40,-50,-50,-40,-40,-30,
        -20,-30,-30,-40,-40,-30,-30,-20,
        -10,-20,-20,-20,-20,-20,-20,-10,
        20, 20,  0,  0,  0,  0, 20, 20,
        20, 30, 10,  0,  0, 10, 30, 20
    }
};

pub fn captureMoveMaterial(bd: Board.BoardData, move: Move) i16 {
    var material: i16 = 0;
    const from = @intFromEnum(bd.getPieceAt(move.from).?);
    const to_square = if (!move.is_promotion and move.extra.capture == .ep_capture) move.to ^ 8 else move.to;
    const to = @intFromEnum(bd.getPieceAt(to_square).?);

    const cost_from = if (from < 6) PIECE_COST[from] else PIECE_COST[from - 6];
    const cost_to = if (to < 6) PIECE_COST[to] else PIECE_COST[to - 6];

    material -= cost_from;
    material += cost_to;

    return material;
}

pub fn whiteEval(bd: Board.BoardData) i16 {
    var score: i16 = 0;
    // material
    inline for (0..5) |i| {
        score += (@as(i16, @popCount(bd.pieces[i])) - @as(i16, @popCount(bd.pieces[i + 6]))) * PIECE_COST[i];
    }

    inline for (0..6) |i| {
        var piece: u64 = undefined;

        piece = bd.pieces[i];
        while (piece != 0) : (piece &= piece - 1) {
            const pos: u6 = @intCast(@ctz(piece));
            score += BONUS_TABLES[i][board.sideFlipSquare(pos)];
        }

        piece = bd.pieces[i+6];
        while (piece != 0) : (piece &= piece - 1) {
            const pos: u6 = @intCast(@ctz(piece));
            score -= BONUS_TABLES[i][pos];
        }
    }

    return score;
}
