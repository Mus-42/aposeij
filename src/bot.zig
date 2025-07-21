const std = @import("std");
const board = @import("board");

const Alloc = std.mem.Allocator;
const Move = board.Move;
const PieceKind = board.PieceKind;
const Board = board.Board;

pub const SCORE_MATE_ABS: i32 = 32000;
pub const SCORE_INFINITY: i32 = SCORE_MATE_ABS+1;

pub const Bot = struct {
    alloc: Alloc,

    const Self = @This();

    pub fn init(alloc: Alloc) Self {
        return .{
            .alloc = alloc,
        };
    }

    fn whiteEval(bd: Board.BoardData) i32 {
       var score: i32 = 0;
       score += (@as(i32, @popCount(bd.pieces[@intFromEnum(PieceKind.w_pawn)])) 
            -    @as(i32, @popCount(bd.pieces[@intFromEnum(PieceKind.b_pawn)])))
            * 100;
       score += (@as(i32, @popCount(bd.pieces[@intFromEnum(PieceKind.w_knight)])) 
            -    @as(i32, @popCount(bd.pieces[@intFromEnum(PieceKind.b_knight)])))
            * 270;
       score += (@as(i32, @popCount(bd.pieces[@intFromEnum(PieceKind.w_bishop)])) 
            -    @as(i32, @popCount(bd.pieces[@intFromEnum(PieceKind.b_bishop)])))
            * 310;
       score += (@as(i32, @popCount(bd.pieces[@intFromEnum(PieceKind.w_rook)])) 
            -    @as(i32, @popCount(bd.pieces[@intFromEnum(PieceKind.b_rook)])))
            * 450;
       score += (@as(i32, @popCount(bd.pieces[@intFromEnum(PieceKind.w_queen)])) 
            -    @as(i32, @popCount(bd.pieces[@intFromEnum(PieceKind.b_queen)])))
            * 900;

       return score;
    }

    fn eval(bd: Board.BoardData) i32 {
        const white_score = whiteEval(bd);
        if (bd.side_to_move == .white) {
            return white_score;
        } else {
            return -white_score;
        }
    }

    fn quiescenceSearch(self: *Self, brd: *Board, root_alpha: i32, beta: i32) i32 {
        var moves = board.Moves{};
        board.genMoves(brd.data, &moves);

        if (moves.count() == 0) {
            return -SCORE_MATE_ABS;
        }
        
        var score = -SCORE_INFINITY;
        var alpha = root_alpha;
        for (moves.moves()) |move| {
            if (!move.is_capture) 
                continue;

            brd.makeMove(move);
            const move_score = -self.quiescenceSearch(brd, -beta, -alpha);
            brd.unmakeMove();

            if (move_score > score) {
                score = move_score;
                if (score > alpha) { 
                    alpha = score;
                }
            }
            if (move_score >= beta) {
                return score;
            }
        }

        if (score == -SCORE_INFINITY)
            return eval(brd.data);

        return score;
    }

    fn search(self: *Self, brd: *Board, root_alpha: i32, beta: i32, remaining_depth: u32) i32 {
        if (remaining_depth == 0) return self.quiescenceSearch(brd, root_alpha, beta);

        var moves = board.Moves{};
        board.genMoves(brd.data, &moves);

        if (moves.count() == 0) {
            return -SCORE_MATE_ABS;
        }

        var score = -SCORE_INFINITY;
        var alpha = root_alpha;
        for (moves.moves()) |move| {
            brd.makeMove(move);
            const move_score = -self.search(brd, -beta, -alpha, remaining_depth - 1);
            brd.unmakeMove();

            if (move_score > score) {
                score = move_score;
                if (score > alpha) { 
                    alpha = score;
                }
            }
            if (move_score >= beta) {
                return score;
            }
        }
        return score;
    }

    pub fn bestMove(self: *Self, brd: *Board) Move {
        const DEPTH = 1;

        var score = -SCORE_INFINITY;
        var best_move: Move = Move.NULL;

        var moves = board.Moves{};
        board.genMoves(brd.data, &moves);

        for (moves.moves()) |move| {
            brd.makeMove(move);
            const move_score = -self.search(brd, -SCORE_INFINITY, -score, DEPTH);
            brd.unmakeMove();

            if (move_score > score) {
                score = move_score;
                best_move = move;
            }
        }

        std.debug.assert(best_move != Move.NULL);
        
        return best_move;
    }

    pub fn deinit(self: *Self) void {
        _ = self;
    }
};
