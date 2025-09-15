const std = @import("std");
const board = @import("board");

const Alloc = std.mem.Allocator;
const Move = board.Move;
const PieceKind = board.PieceKind;
const Board = board.Board;

pub const MAX_DEPTH = 128;
pub const MAX_THINKING_TIME_NS = 30 * std.time.ns_per_s; 
pub const MAX_EXTENSIONS = 8;

pub const SCORE_MATE_ABS: i16 = 32000;
pub const SCORE_MATE_EPS: i16 = MAX_DEPTH;
pub const SCORE_INFINITY: i16 = SCORE_MATE_ABS+1;

pub const TIME_EPS_NS: u64 = 200;

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
    search_start: std.time.Instant = undefined,
    available_time: u64 = 0,

    const PerPly = struct {
        killers: [2]Move,
    };

    const Bound = enum {
        lower,
        exact,
        upper,
    };

    const NodeType = enum {
        Pv,
        NonPv
    };

    const TTEntry = struct {
        key: u64,
        depth: u8,
        score: i16,
        move: Move,
        bound: Bound,
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

    fn preSearchCleanup(self: *Self) void {
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
    }

    fn quiescenceSearch(self: *Self, comptime node_type: NodeType, root_alpha: i16, beta: i16, ply: u32) !i16 {
        if (self.timeRemaining() <= TIME_EPS_NS) {
            return error.TimeExpired;
        }

        const is_pv_node = node_type == .Pv;
        std.debug.assert(is_pv_node or (root_alpha == beta - 1));

        const zobrist_key = self.brd.data.zobrist_key;
        const tt_index = zobrist_key & (self.tt.len - 1);
        const tt_entry = &self.tt[tt_index];
        var tt_move: ?Move = null;

        if (tt_entry.key == zobrist_key) {
            self.stats.tt_hits += 1;
            if (tt_entry.bound == .exact or tt_entry.score >= beta and tt_entry.bound == .lower or tt_entry.score < root_alpha and tt_entry.bound == .upper) {
                return tt_entry.score;
            }

            tt_move = tt_entry.move;
        } else {
            self.stats.tt_misses += 1;
        }

        const static_eval = self.eval();

        // TODO make something about that?
        if (ply == MAX_DEPTH)
            return static_eval;

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

        // technically can be a refutation move
        var best_move: Move = Move.NULL;
        var tt_bound: Bound = .upper;

        for (moves.moves()) |move| {
            std.debug.assert(move.is_capture);
            self.brd.makeMove(move);
            const move_score = -try self.quiescenceSearch(node_type, -beta, -alpha, ply+1);
            self.brd.unmakeMove();

            if (move_score >= beta) {
                self.stats.beta_cutoffs += 1;
                tt_bound = .lower;
                best_move = move;
                return move_score;
            }

            if (move_score > score) {
                // TODO update pv?
                score = move_score;
                best_move = move;
                if (score > alpha) { 
                    alpha = score;
                    tt_bound = .exact;
                }
            }
        }

        if (tt_entry.key != zobrist_key or tt_entry.depth == 0) {
            tt_entry.* = .{
                .move = best_move,
                .score = score,
                .depth = 0,
                .key = zobrist_key,
                .bound = tt_bound,
            };
        }

        return score;
    }

    fn addKiller(self: *Self, ply: u32, move: Move) void {
        const killers = &self.per_ply[ply].killers;
        if (killers[0] != move and killers[1] != move) {
            killers[1] = killers[0]; 
            killers[0] = move; 
        }
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

    fn search(
        self: *Self,
        comptime node_type: NodeType,
        root_alpha: i16,
        beta: i16,
        remaining_depth: u32,
        ply: u32,
        extensions_used: u32,
    ) !i16 {
        if (self.timeRemaining() <= TIME_EPS_NS) {
            return error.TimeExpired;
        }

        const is_pv_node = node_type == .Pv;
        std.debug.assert(is_pv_node or (root_alpha == beta - 1));

        const zobrist_key = self.brd.data.zobrist_key;
        const tt_index = zobrist_key & (self.tt.len - 1);
        const tt_entry = &self.tt[tt_index];
        var tt_move: ?Move = null;

        self.stats.nodes_all += 1;

        @memset(self.perPlyPv(ply), Move.NULL);

        if (tt_entry.key == zobrist_key) {
            self.stats.tt_hits += 1;
            if (tt_entry.depth >= remaining_depth and ply > 0) {
                if (tt_entry.bound == .exact or tt_entry.score >= beta and tt_entry.bound == .lower or tt_entry.score < root_alpha and tt_entry.bound == .upper) {
                    return tt_entry.score;
                }
            }

            tt_move = tt_entry.move;
        } else {
            self.stats.tt_misses += 1;
        }

        const pos_repetitions = self.brd.repetitionsCount();
        if (pos_repetitions > 0 and ply > 0) {
            return 0;
        }

        const static_eval = try self.quiescenceSearch(node_type, root_alpha, beta, ply);

        // TODO make something about that?
        if (ply == MAX_DEPTH)
            return static_eval;

        var score = -SCORE_INFINITY;
        var alpha = root_alpha;

        // TODO fix not-seeing-mate-in-1-move problem with this
        // if (!is_pv_node) {
        //     const margin = @as(i16, @intCast(150 * remaining_depth));
        //     score = static_eval;
        //     if (static_eval >= beta + margin) {
        //         self.stats.beta_cutoffs += 1;
        //         return static_eval;
        //     }
        //     if (static_eval - margin > alpha) {
        //         alpha = static_eval - margin;
        //     }
        // }

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

        // technically can be a refutation move
        var best_move: Move = Move.NULL;
        var tt_bound: Bound = .upper;

        const moves_len = moves.moves().len;
        for (0.., moves.moves()) |i, move| {
            self.brd.makeMove(move);
            // TODO improve on that, still bad
            var search_depth = remaining_depth * 4 / 5 + 1 -| @as(u32, @intCast(i * 8 / moves_len)) / 4;
            search_depth = @max(search_depth, 2);
            search_depth = @min(search_depth, remaining_depth);
            search_depth -= 1;
            // TODO more extensions, limit on total extended depth count
           
            var search_ext: u32 = 0;
            if (self.brd.isInCheck()) {
                search_ext += 1;
            }
            if (move.is_promotion and move.extra.promotion == .queen) {
                search_ext += 1;
            }
            if (moves_len == 1) {
                search_ext += 1;
            }
            search_ext = @min(search_ext + extensions_used, MAX_EXTENSIONS) - extensions_used;
            
            
            // TODO figure out LMR-stuff (for now random values)
            var move_score: i16 = 0;
            if (remaining_depth < 2 or i < 3) {
                move_score = -try self.search(node_type, -beta, -alpha, remaining_depth - 1 + search_ext, ply + 1, extensions_used + search_ext);
            } else {
                move_score = -try self.search(.NonPv, -(alpha+1), -alpha, search_depth + search_ext, ply + 1, extensions_used + search_ext);
                if (move_score > alpha and is_pv_node) {
                    move_score = -try self.search(.Pv, -beta, -alpha, search_depth + search_ext, ply + 1, extensions_used + search_ext);
                }
            }
            self.brd.unmakeMove();

            if (move_score >= beta) {
                if (!move.is_capture) {
                    self.addKiller(ply, move);
                }
                self.stats.beta_cutoffs += 1;
                score = move_score;
                best_move = move;
                tt_bound = .lower;
                break;
            }

            if (move_score > score) {
                score = move_score;

                if (is_pv_node) {
                    self.setNewPvMoveAtPly(move, ply);
                }

                best_move = move;
                if (score > alpha) { 
                    alpha = score;
                    tt_bound = .exact;
                }
            }
        }

        if (tt_entry.key != zobrist_key or tt_entry.depth <= remaining_depth) {
            tt_entry.* = .{
                .move = best_move,
                .score = score,
                .depth = @intCast(remaining_depth),
                .key = zobrist_key,
                .bound = tt_bound,
            };
        }

        return score;
    }

    pub fn timeRemaining(self: *Self) u64 {
        const now = std.time.Instant.now() catch unreachable;
        return self.available_time -| now.since(self.search_start);
    }

    pub fn bestMove(self: *Self, uci_writer: *std.io.Writer, time_controls: TimeControls) Move {
        var depth_target: u32 = MAX_DEPTH;
        var available_time: u64 = MAX_THINKING_TIME_NS;

        switch (time_controls) {
            .infinite => {},
            .to_depth => |c| depth_target = @min(depth_target, c.target),
            .time_remaining => |c| available_time = @min(available_time, c.ns),
        }
        
        self.available_time = available_time;
        self.search_start = std.time.Instant.now() catch unreachable;
    
        var score: i32 = 0;
        var best_move: Move = .NULL;
        var last_iteration_time: u64 = 0;

        for (1..depth_target+1) |d| {
            self.preSearchCleanup();

            const time_remaining = self.timeRemaining();
            // TODO better branching factor estimate?
            if (d != 1 and last_iteration_time * 3 > time_remaining) {
                break;
            }

            const search_iteration_beg = std.time.Instant.now() catch unreachable;
            var is_time_over = false;
            if (self.search(.Pv, -SCORE_INFINITY, SCORE_INFINITY, @intCast(d), 0, 0)) |s| {
                score = s; 
            } else |err| {
                if (err == error.TimeExpired) {
                    is_time_over = true;
                }
            }

            const search_iteration_end = std.time.Instant.now() catch unreachable;
            const iteration_time = search_iteration_end.since(search_iteration_beg);
            last_iteration_time = iteration_time;

            if (d == 1 or !is_time_over) {
                best_move = self.perPlyPv(0)[0];
            }

            uci_writer.print("info depth {d} score cp {} time {} pv", .{d, score, iteration_time / std.time.ns_per_ms}) catch {};
            for (self.perPlyPv(0)) |move| {
                if (move == Move.NULL) break;
                uci_writer.print(" {s}", .{move.algebraicNotation().toStr()}) catch {};
            }
            uci_writer.print("\n", .{}) catch {};
            uci_writer.print("info string {any}\n", .{self.stats}) catch {};
            uci_writer.flush() catch {};

            if (is_time_over) {
                break;
            }
        }
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
