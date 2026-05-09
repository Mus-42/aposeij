const std = @import("std");
const board = @import("board");

const Alloc = std.mem.Allocator;
const Move = board.Move;
const PieceKind = board.PieceKind;
const Board = board.Board;

pub const PIECE_COST = [6][2]i16{
    .{ 85, 95 },
    .{ 320, 290 },
    .{ 330, 310 },
    .{ 480, 510 },
    .{ 1000, 950 },
    .{ 0, 0 },
};

const GAME_PHASE_INCREMENT: [12]i16 = .{ 0, 2, 3, 5, 9, 0, 0, 2, 3, 5, 9, 0 };
const GAME_PHASE_MAX: i16 = (2 + 3 + 5 + 9) * 4;
const GAME_PHASE_ENDGAME_LIM: i16 = GAME_PHASE_MAX * 3 / 4;

const PER_SQUARE_BONUS: [6][2][64]i16 = .{
    .{
        .{
               0,    0,    0,    0,    0,    0,    0,    0,
              40,   80,   90,   80,   80,   90,   80,   40,
             -15,   15,   40,   45,   45,   40,   15,  -15,
             -20,   15,    5,   20,   20,    5,   15,  -20,
             -30,    0,    0,   10,   10,    0,    0,  -30,
             -20,   10,   -5,   -5,   -5,   -5,   10,  -20,
             -30,   15,    0,  -20,  -20,    0,   15,  -30,
               0,    0,    0,    0,    0,    0,    0,    0,
        },
        .{
               0,    0,    0,    0,    0,    0,    0,    0,
             180,  165,  145,  140,  140,  145,  165,  180,
              85,   90,   65,   60,   60,   65,   90,   85,
              20,   20,    5,    0,    0,    5,   20,   20,
               5,    5,  -10,  -10,  -10,  -10,    5,    5,
              -5,    0,  -10,    0,    0,  -10,    0,   -5,
               0,    5,    0,   10,   10,    0,    5,    0,
               0,    0,    0,    0,    0,    0,    0,    0,
        },
    },
    .{
        .{
            -140,  -55,  -70,    5,    5,  -70,  -55, -140,
             -45,  -20,   65,   25,   25,   65,  -20,  -45,
              -5,   65,   80,   70,   70,   80,   65,   -5,
               5,   15,   40,   45,   45,   40,   15,    5,
             -15,   10,   15,   20,   20,   15,   10,  -15,
             -20,    5,   10,   10,   10,   10,    5,  -20,
             -25,  -35,    0,   -5,   -5,    0,  -35,  -25,
             -65,  -20,  -45,  -25,  -25,  -45,  -20,  -65,
        },
        .{
             -80,  -55,  -20,  -30,  -30,  -20,  -55,  -80,
             -40,  -20,  -25,  -10,  -10,  -25,  -20,  -40,
             -35,  -20,    0,    0,    0,    0,  -20,  -35,
             -20,    5,   15,   20,   20,   15,    5,  -20,
             -20,   -5,   15,   20,   20,   15,   -5,  -20,
             -25,  -15,   -5,   10,   10,   -5,  -15,  -25,
             -45,  -25,  -15,   -5,   -5,  -15,  -25,  -45,
             -50,  -55,  -25,  -20,  -20,  -25,  -55,  -50,
        },
    },
    .{
        .{
             -20,    5,  -65,  -35,  -35,  -65,    5,  -20,
             -40,   15,   20,    5,    5,   20,   15,  -40,
             -10,   35,   45,   35,   35,   45,   35,  -10,
              -5,    5,   25,   40,   40,   25,    5,   -5,
              -5,   10,   10,   30,   30,   10,   10,   -5,
               5,   15,   20,   10,   10,   20,   15,    5,
               0,   20,   15,    0,    0,   15,   20,    0,
             -30,  -25,  -15,  -20,  -20,  -15,  -25,  -30,
        },
        .{
             -20,  -20,  -10,  -10,  -10,  -10,  -20,  -20,
             -15,   -5,   -5,  -10,  -10,   -5,   -5,  -15,
               0,   -5,    0,   -5,   -5,    0,   -5,    0,
              -5,    5,   10,   10,   10,   10,    5,   -5,
             -10,    0,   10,   10,   10,   10,    0,  -10,
             -15,   -5,    5,   10,   10,    5,   -5,  -15,
             -25,  -20,  -10,    0,    0,  -10,  -20,  -25,
             -20,  -10,  -20,  -10,  -10,  -20,  -10,  -20,
        },
    },
    .{
        .{
              35,   35,   20,   55,   55,   20,   35,   35,
              35,   25,   60,   70,   70,   60,   25,   35,
               5,   40,   35,   25,   25,   35,   40,    5,
             -25,  -10,   20,   25,   25,   20,  -10,  -25,
             -30,  -10,  -10,    0,    0,  -10,  -10,  -30,
             -40,  -15,  -10,  -10,  -10,  -10,  -15,  -40,
             -60,  -15,   -5,   -5,   -5,   -5,  -15,  -60,
             -25,  -25,    0,   15,   15,    0,  -25,  -25,
        },
        .{
               5,    5,   15,   10,   10,   15,    5,    5,
               5,   10,    5,    0,    0,    5,   10,    5,
               0,    0,    0,    0,    0,    0,    0,    0,
               0,    0,    5,    0,    0,    5,    0,    0,
              -5,   -5,    0,   -5,   -5,    0,   -5,   -5,
             -10,   -5,  -10,   -5,   -5,  -10,   -5,  -10,
              -5,  -10,   -5,   -5,   -5,   -5,  -10,   -5,
             -15,    0,   -5,   -5,   -5,   -5,    0,  -15,
        },
    },
    .{
        .{
               5,   20,   35,   35,   35,   35,   20,    5,
              15,  -10,   25,  -10,  -10,   25,  -10,   15,
              20,   15,   30,   15,   15,   30,   15,   20,
             -15,  -15,    0,  -10,  -10,    0,  -15,  -15,
             -10,  -15,  -10,  -10,  -10,  -10,  -15,  -10,
              -5,    5,   -5,   -5,   -5,   -5,    5,   -5,
             -20,  -10,   10,    5,    5,   10,  -10,  -20,
             -30,  -25,  -20,   -5,   -5,  -20,  -25,  -30,
        },
        .{
               5,   15,   20,   25,   25,   20,   15,    5,
             -10,   25,   25,   45,   45,   25,   25,  -10,
             -10,   10,   20,   45,   45,   20,   10,  -10,
              15,   35,   30,   50,   50,   30,   35,   15,
               0,   30,   25,   35,   35,   25,   30,    0,
             -10,  -10,   15,    5,    5,   15,  -10,  -10,
             -30,  -30,  -30,  -20,  -20,  -30,  -30,  -30,
             -40,  -25,  -30,  -25,  -25,  -30,  -25,  -40,
        },
    },
    .{
        .{
             -30,   10,  -10,  -40,  -40,  -10,   10,  -30,
               0,  -20,  -15,  -10,  -10,  -15,  -20,    0,
             -20,   20,    0,  -20,  -20,    0,   20,  -20,
             -30,  -20,  -20,  -30,  -30,  -20,  -20,  -30,
             -50,  -20,  -40,  -45,  -45,  -40,  -20,  -50,
             -25,  -15,  -30,  -45,  -45,  -30,  -15,  -25,
               0,    5,  -15,  -55,  -55,  -15,    5,    0,
              -5,   30,  -10,  -25,  -25,  -10,   30,   -5,
        },
        .{
             -50,  -20,   -5,  -15,  -15,   -5,  -20,  -50,
              -5,   20,   25,   15,   15,   25,   20,   -5,
              10,   30,   30,   15,   15,   30,   30,   10,
              -5,   20,   25,   25,   25,   25,   20,   -5,
             -15,    0,   20,   25,   25,   20,    0,  -15,
             -15,    0,   10,   20,   20,   10,    0,  -15,
             -25,  -10,    0,   10,   10,    0,  -10,  -25,
             -50,  -30,  -20,  -20,  -20,  -20,  -30,  -50,
        },
    },
};

const BONUS_TABLES: [12][64][2]i16 = blk: {
    @setEvalBranchQuota(40000);

    var bonus: [12][64][2]i16 = @splat(@splat(@splat(0)));

    for (0..6) |piece| {
        for (0..64) |square| {
            bonus[piece + 0][square][0] = PIECE_COST[piece][0] + PER_SQUARE_BONUS[piece][0][board.sideFlipSquare(square)];
            bonus[piece + 0][square][1] = PIECE_COST[piece][1] + PER_SQUARE_BONUS[piece][1][board.sideFlipSquare(square)];
            bonus[piece + 6][square][0] = - PIECE_COST[piece][0] - PER_SQUARE_BONUS[piece][0][square];
            bonus[piece + 6][square][1] = - PIECE_COST[piece][1] - PER_SQUARE_BONUS[piece][1][square];
        }
    }

    break :blk bonus;
};

pub fn whiteEval(bd: Board.BoardData) i16 {
    var score_midgame: i32 = 0;
    var score_endgame: i32 = 0;
    var game_phase: i32 = 0;

    inline for (0..12) |i| {
        var piece = bd.pieces[i];
        while (piece != 0) : (piece &= piece - 1) {
            const pos = @ctz(piece);
            score_midgame += BONUS_TABLES[i][pos][0];
            score_endgame += BONUS_TABLES[i][pos][1];
            game_phase += GAME_PHASE_INCREMENT[i];
        }
    }

    game_phase = @min(game_phase, GAME_PHASE_ENDGAME_LIM);
    return @intCast(@divTrunc(score_midgame * game_phase + score_endgame * (GAME_PHASE_ENDGAME_LIM - game_phase), GAME_PHASE_ENDGAME_LIM));
}

pub fn eval(bd: Board.BoardData) i16 {
    const white_score = whiteEval(bd);
    if (bd.side_to_move == .white) {
        return white_score;
    } else {
        return -white_score;
    }
}

pub fn captureMoveMaterial(bd: Board.BoardData, move: Move) i16 {
    const from = @intFromEnum(bd.getPieceAt(move.from).?);
    const to_square = if (!move.is_promotion and move.extra.capture == .ep_capture) move.to ^ 8 else move.to;
    const to = @intFromEnum(bd.getPieceAt(to_square).?);

    const cost_from = if (from < 6) PIECE_COST[from][0] else PIECE_COST[from - 6][0];
    const cost_to =   if (to < 6) PIECE_COST[to][0] else PIECE_COST[to - 6][0];

    return cost_to * 6 - cost_from;
}
