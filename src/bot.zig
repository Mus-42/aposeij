const std = @import("std");
const board = @import("board");

const Alloc = std.mem.Allocator;
const Move = board.Move;
const PieceKind = board.PieceKind;
const Board = board.Board;

pub const MAX_DEPTH = 128;
pub const MAX_THINKING_TIME_NS = 30 * std.time.ns_per_s; 

pub const SCORE_MATE_ABS: i16 = 32000;
pub const SCORE_MATE_EPS: i16 = MAX_DEPTH;
pub const SCORE_INFINITY: i16 = SCORE_MATE_ABS+1;

const PIECE_COST = [6]i16{
    100,
    320,
    330,
    500,
    900,
    0
};

const TT_SIZE = 1<<22;

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

pub const Stats = struct {
    tt_hits: u64 = 0,
    tt_misses: u64 = 0,
    evals: u64 = 0,
    beta_cutoffs: u64 = 0,
    nodes_all: u64 = 0,
    nodes_leaf: u64 = 0,
};

pub const TimeControls = union (enum) {
    infinite,
    time_remaining: struct { ns: u64 },
    to_depth: struct { target: u32 },
};

pub const Bot = struct {
    alloc: Alloc,
    brd: *Board,
    pv_moves: []Move,
    per_ply: [MAX_DEPTH]PerPly = undefined,
    tt: []TTEntry,
    stats: Stats = .{},
    is_time_over: bool = false,
    search_start: std.time.Instant = undefined,
    max_search_time_ns: u64 = undefined,

    const PerPly = struct {
        killers: [2]Move,
    };

    const TTEntry = struct {
        key: u64,
        depth: u8,
        score: i16,
        move: Move,
    };

    const Self = @This();

    pub fn init(alloc: Alloc, brd: *Board) !Self {
        const tt = try alloc.alloc(TTEntry, TT_SIZE);
        const pv_moves = try alloc.alloc(Move, MAX_DEPTH * (MAX_DEPTH + 1) / 2);
        return .{
            .alloc = alloc,
            .brd = brd,
            .pv_moves = pv_moves,
            .tt = tt,
        };
    }

    pub fn deinit(self: *Self) void {
        self.alloc.free(self.tt);
        self.alloc.free(self.pv_moves);
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

    pub fn eval(self: *Self) i16 {
        self.stats.evals += 1;
        const white_score = whiteEval(self.brd.data);
        if (self.brd.data.side_to_move == .white) {
            return white_score;
        } else {
            return -white_score;
        }
    }

    fn clearStats(self: *Self) void {
        self.stats = .{};
    }

    // order:
    // hash
    // pv
    // captures (good)
    // killers
    // quiet
    // captured (bad)
    fn scoreMove(self: *Self, move: Move, ply: u32, tt_move: ?Move) i32 {
        var score: i16 = 0;
        if (tt_move == move) {
            score += 2000;
        }
        // TODO pv move?
        // if (self.per_ply[ply].pv_move == move) {
        //     score += 1000;
        // }

        if (self.per_ply[ply].killers[0] == move or self.per_ply[ply].killers[1] == move) {
            score += 800;
        }
        if (move.is_promotion and move.extra.promotion == .queen) {
            score += 200;
        }
        if (move.is_capture) {
            score += 10;

            const from = @intFromEnum(self.brd.data.getPieceAt(move.from).?);
            const to_square = if (!move.is_promotion and move.extra.capture == .ep_capture) move.to ^ 8 else move.to;
            const to = @intFromEnum(self.brd.data.getPieceAt(to_square).?);

            const cost_from = if (from < 6) PIECE_COST[from] else PIECE_COST[from - 6];
            const cost_to = if (to < 6) PIECE_COST[to] else PIECE_COST[to - 6];

            // TODO weight somehow
            score -= cost_from;
            score += cost_to;
        }

        return score;
    }

    fn orderMoves(self: *Self, moves: []Move, ply: u32, tt_move: ?Move) void {
        const Context = struct {
            moves: []Move,
            scores: []i32,

            pub fn lessThan(ctx: @This(), a: usize, b: usize) bool {
                return ctx.scores[a] > ctx.scores[b];
            }

            pub fn swap(ctx: @This(), a: usize, b: usize) void {
                std.mem.swap(Move, &ctx.moves[a], &ctx.moves[b]);
                std.mem.swap(i32, &ctx.scores[a], &ctx.scores[b]);
            }
        };
        var scores: [board.MAX_MOVES]i32 = undefined;
        for (0..moves.len) |i| {
            scores[i] = self.scoreMove(moves[i], ply, tt_move);
        }
        std.mem.sortUnstableContext(0, moves.len, Context { .moves = moves, .scores = scores[0..moves.len] });
        //std.debug.print("{any}\n", .{scores[0..moves.len]});
    }

    fn quiescenceSearch(self: *Self, root_alpha: i16, beta: i16, ply: u32) i16 {
        const static_eval = self.eval();
        var score = static_eval;
        var alpha = root_alpha;

        if (static_eval >= beta) {
            self.stats.beta_cutoffs += 1;
            return static_eval;
        }
        if (static_eval > alpha) {
            alpha = static_eval;
        }

        var moves = board.Moves{};
        board.genMoves(self.brd.data, &moves);

        const pos_repetitions = self.brd.repetitionsCount();
        if (moves.count() == 0 or pos_repetitions > 2) {
            if (self.brd.isInCheck()) {
                return @as(i16, @intCast(ply)) - SCORE_MATE_ABS;
            }
            return 0;
        }

        moves.filterCapturesOnly();

        if (moves.count() == 0) {
            return self.eval();
        }
        
        self.orderMoves(moves.moves(), ply, null);

        for (moves.moves()) |move| {
            std.debug.assert(move.is_capture);
            self.brd.makeMove(move);
            const move_score = -self.quiescenceSearch(-beta, -alpha, ply+1);
            self.brd.unmakeMove();

            if (move_score >= beta) {
                if (!move.is_capture) {
                    self.per_ply[ply].killers[1] = self.per_ply[ply].killers[0]; 
                    self.per_ply[ply].killers[0] = move; 
                }
                self.stats.beta_cutoffs += 1;
                return move_score;
            }

            if (move_score > score) {
                // TODO update pv?
                score = move_score;
                if (score > alpha) { 
                    alpha = score;
                }
            }
        }

        return score;
    }

    fn perPlyPv(self: *Self, ply: u32) []Move {
        const count = MAX_DEPTH - ply;
        const start = self.pv_moves.len - (count + 1) * count / 2;
        return self.pv_moves[start..start+count];
    }

    fn setNewPvMoveAtPly(self: *Self, move: Move, ply: u32) void {
        const pv = self.perPlyPv(ply);
        const next_pv = self.perPlyPv(ply+1);
        @memcpy(pv[1..], next_pv);
        pv[0] = move;
    }

    fn search(self: *Self, root_alpha: i16, beta: i16, remaining_depth: u32, ply: u32) i16 {
        const zobrist_key = self.brd.data.zobrist_key;
        const tt_index = zobrist_key & (self.tt.len - 1);
        var tt_move: ?Move = null;
       
        self.stats.nodes_all += 1;

        if (self.tt[tt_index].key == zobrist_key) {
            self.stats.tt_hits += 1;
            if (self.tt[tt_index].depth >= remaining_depth and ply != 0) {
                return self.tt[tt_index].score;
            }
            tt_move = self.tt[tt_index].move;
        } else {
            self.stats.tt_misses += 1;
        }

        const pos_repetitions = self.brd.repetitionsCount();
        if (pos_repetitions > 0 and ply > 0) {
            return root_alpha;
        }

        const static_eval = self.quiescenceSearch(root_alpha, beta, ply);
        var score = -SCORE_INFINITY;
        var alpha = root_alpha;

        if (ply > 0) {
            score = static_eval;
            if (static_eval >= beta) {
                self.stats.beta_cutoffs += 1;
                return static_eval;
            }
            if (static_eval > alpha) {
                alpha = static_eval;
            }
        }

        if (remaining_depth == 0) {
            self.stats.nodes_leaf += 1;
            return static_eval;
        }

        var moves = board.Moves{};
        board.genMoves(self.brd.data, &moves);

        if (moves.count() == 0 or pos_repetitions > 2) {
            if (self.brd.isInCheck()) {
                return @as(i16, @intCast(ply)) - SCORE_MATE_ABS;
            }
            return 0;
        }


        self.orderMoves(moves.moves(), ply, tt_move);

        // var score = -SCORE_INFINITY;
        // var alpha = root_alpha;
        var best_move: Move = Move.NULL;
        for (0.., moves.moves()) |i, move| {
            const search_depth = if (remaining_depth > 2 and i > 5) @max(remaining_depth * 2 / 3, 2) - 1 else remaining_depth - 1;

            self.brd.makeMove(move);
            
            // TODO figure out LMR-stuff (for now random values)
            var move_score: i16 = 0;
            if (remaining_depth < 2 or i < 5) {
                move_score = -self.search(-beta, -alpha, remaining_depth - 1, ply + 1);
            } else {
                move_score = -self.search(-(alpha+1), -alpha, search_depth, ply + 1);
                if (move_score > alpha) {
                    move_score = -self.search(-beta, -alpha, search_depth, ply + 1);
                }
            }
            self.brd.unmakeMove();

            if (move_score >= beta) {
                if (!move.is_capture) {
                    self.per_ply[ply].killers[1] = self.per_ply[ply].killers[0]; 
                    self.per_ply[ply].killers[0] = move; 
                }
                self.stats.beta_cutoffs += 1;
                score = move_score;
                break;
            }

            if (move_score > score) {
                score = move_score;
                if (score > alpha) { 
                    alpha = score;
                    best_move = move;
                    self.setNewPvMoveAtPly(move, ply);
                    //self.per_ply[ply].pv_move = move;
                }
            }
        }

        // NOTE:
        // best_move _can_ be null here
        
        // TODO verify that depth < 
        self.tt[tt_index] = .{
            .move = best_move,
            .score = score,
            .depth = @intCast(remaining_depth),
            .key = zobrist_key,
        };

        return score;
    }



    pub fn bestMove(self: *Self, uci_writer: *std.io.Writer, time_controls: TimeControls) Move {
        var depth_target: u32 = MAX_DEPTH;
        var max_thinking_time: u64 = MAX_THINKING_TIME_NS;

        switch (time_controls) {
            .infinite => {},
            .to_depth => |c| depth_target = @min(depth_target, c.target),
            .time_remaining => |c| {
                // TODO
                max_thinking_time = @min(max_thinking_time, c.ns);
            },
        }
        
        self.search_start = std.time.Instant.now() catch unreachable;
        self.is_time_over = false;

        @memset(self.pv_moves, Move.NULL);
    
        var score: i32 = 0;
        for (1..depth_target+1) |d| {
            self.clearStats();
            score = self.search(-SCORE_INFINITY, SCORE_INFINITY, @intCast(d), 0);
            uci_writer.print("info depth {d} score cp {} pv", .{d, score}) catch {};
            for (self.perPlyPv(0)) |move| {
                if (move == Move.NULL) break;
                uci_writer.print(" {s}", .{move.algebraicNotation().toStr()}) catch {};
            }
            uci_writer.print("\n", .{}) catch {};
            uci_writer.print("info string {any}\n", .{self.stats}) catch {};
        }
        const best_move = self.pv_moves[0];
        std.debug.assert(best_move != Move.NULL);


        // TODO
        // if (@abs(score) + SCORE_MATE_EPS > SCORE_MATE_ABS) {
        //     // TODO
        //     uci_writer.print("info score mate {}\n", .{score}) catch {};
        // } else {
        // }

        return best_move;
    }

};
