const std = @import("std");
const board = @import("board");
const tt = @import("transposition_table.zig");
const evaluation = @import("evaluation.zig");
const uci = @import("uci.zig");

const Alloc = std.mem.Allocator;
const Move = board.Move;
const PieceKind = board.PieceKind;
const Board = board.Board;

pub const MAX_PLY = 128;
pub const MAX_THINKING_TIME_NS = 30 * std.time.ns_per_min; 
pub const MAX_EXTENSIONS = 8;

pub const QS_TT_DEPTH = 0;

pub const SCORE_MATE_ABS = 32000;
pub const SCORE_MATE_EPS = MAX_PLY;
pub const SCORE_INFINITY = SCORE_MATE_ABS+1;

pub const TIME_EPS_NS: u64 = 200;


// order:
// hash
// pv
// captures (good)
// killers
// quiet
// captured (bad)
pub const MOVESCORE_TT = 0x7FFF;
pub const MOVESCORE_GOOD_CAPTURE = 0x5000;
pub const MOVESCORE_PROMOTION = 0x5100;
pub const MOVESCORE_KILLER = 0x5000;
pub const MOVESCORE_BAD_CAPTURE = -0x4000;

pub fn scoreToMateInPlyAbs(score: i16) ?u16 {
    const mate_ply = SCORE_MATE_ABS - @abs(score);
    if (mate_ply > SCORE_MATE_EPS) return null;
    return mate_ply;
}

pub fn plyToMoves(ply: u16) u16 {
    return (1 + ply) / 2;
}

pub fn scoreToMateInMovesAbs(score: i16) ?u16 {
    const mate_ply = SCORE_MATE_ABS - @abs(score);
    if (mate_ply > SCORE_MATE_EPS) return null;
    return plyToMoves(mate_ply);
}

pub const NodeType = enum {
    Pv,
    NonPv
};

pub const TimeControls = union (enum) {
    infinite,
    nodes: struct { hard_limit: u64 },
    time_remaining: struct { ns: u64 },
    to_depth: struct { target: u32 },
};

pub fn wipEstimateGamePhase(bd: board.Board.BoardData) i16 {
    const VALUES: [5]i16 = .{ 10, 90, 100, 120, 200 };

    var score: i16 = 0;
    inline for (0..5) |i| {
        score += VALUES[i] * (@popCount(bd.pieces[i]) + @popCount(bd.pieces[i + 6]));
    }

    if (score < 900) {
        return 4000;
    } else if (score < 1250) {
        return 3000;
    } else if (score < 1450) {
        return 2000;
    } else if (score < 1600) {
        return 1000;
    }

    return 0;
}

pub fn ButterflyBoard(comptime Counter: type) type {
    return struct {
        // TODO somehow avoid wasting most of the memory on impossible moves
        counters: [2 * 64 * 64]Counter,

        const Self = @This();

        pub fn index(color: board.SideToMove, move: Move) usize {
            return @as(usize, @intFromEnum(color)) << 12 | @as(usize, move.from) << 6 | @as(usize, move.to);
        }

        pub fn setToZero(self: *Self) void {
            @memset(&self.counters, 0);
        }

        pub fn get(self: *const Self, color: board.SideToMove, move: Move) Counter {
            const i = index(color, move);
            return self.counters[i];
        }

        pub fn set(self: *Self, color: board.SideToMove, move: Move, count: Counter) void {
            const i = index(color, move);
            self.counters[i] = count;
        }

        pub fn add(self: *Self, color: board.SideToMove, move: Move, count: Counter) void {
            const i = index(color, move);
            self.counters[i] +|= count;
        }

        pub fn updateHistoryGravity(self: *Self, color: board.SideToMove, move: Move, raw_bonus: i32) void {
            const i = index(color, move);
            const MAX_BONUS = MOVESCORE_KILLER;
            const bonus = @max(@min(raw_bonus, MAX_BONUS), -MAX_BONUS);
            self.counters[i] +|= @intCast(bonus - @divTrunc(@as(i32, self.counters[i]) * @as(i32, @intCast(@abs(bonus))), MAX_BONUS));
        }
    };
}

const PVMoves = struct {
    pv_moves: []Move,
    pv_len: []u16,

    const Self = @This();

    fn init(alloc: Alloc) !Self {
        const pv_moves = try alloc.alloc(Move, MAX_PLY * (MAX_PLY + 1) / 2);
        const pv_len = try alloc.alloc(u16, MAX_PLY);

        return .{ .pv_moves = pv_moves, .pv_len = pv_len };
    }

    fn deinit(self: *Self, alloc: Alloc) void {
        alloc.free(self.pv_moves);
        alloc.free(self.pv_len);
    }

    fn clearAll(self: *Self) void {
        @memset(self.perPlyRaw(0), Move.NULL);
    }

    fn perPlyRaw(self: *Self, ply: u32) []Move {
        const count = MAX_PLY - ply;
        const start = self.pv_moves.len - (count + 1) * count / 2;
        return self.pv_moves[start..start+count];
    }

    fn perPly(self: *Self, ply: u32) []Move {
        return self.perPlyRaw(ply)[0..self.pv_len[ply]];
    }

    fn clearPly(self: *Self, ply: u32) void {
        self.perPlyRaw(ply)[0] = Move.NULL;
        self.pv_len[ply] = 0;
    }

    fn setMoveAtPly(self: *Self, move: Move, ply: u32) void {
        const pv = self.perPlyRaw(ply);
        const next_pv = self.perPly(ply+1);
        @memcpy(pv[1..][0..next_pv.len], next_pv);
        pv[0] = move;
        self.pv_len[ply] = @intCast(1 + next_pv.len);
    }
};

pub const SearchThread = struct {
    alloc: Alloc,
    brd: *Board = undefined,
    tt: tt.TTable,
    per_ply: *[MAX_PLY]PerPly,
    history: *History,
    pv: PVMoves,
    // Stats
    nodes: u64 = 0,
    qsearch_nodes: u64 = 0,

    // Time management
    search_start: std.time.Instant = undefined,
    available_time: u64 = 0,
    is_exiting_search: std.atomic.Value(bool),

    const History = ButterflyBoard(i16);

    const PerPly = struct {
        killers: [2]Move,
    };

    const Self = @This();

    pub fn init(alloc: Alloc) !Self {
        const pv = try PVMoves.init(alloc);
        const history = try alloc.create(History);
        const per_ply = try alloc.create([MAX_PLY]PerPly);
        const ttable = try tt.TTable.init(alloc);

        return .{
            .alloc = alloc,
            .tt = ttable,
            .pv = pv,
            .per_ply = per_ply,
            .history = history,
            .is_exiting_search = .init(false),
        };
    }

    pub fn deinit(self: *Self) void {
        self.tt.deinit(self.alloc);
        self.pv.deinit(self.alloc);
        self.alloc.destroy(self.per_ply);
        self.alloc.destroy(self.history);
    }

    fn evalPosition(self: *Self) i16 {
        const white_score = evaluation.whiteEval(self.brd.data);
        if (self.brd.data.side_to_move == .white) {
            return white_score;
        } else {
            return -white_score;
        }
    }

    fn preSearchCleanup(self: *Self) void {
        self.nodes = 0;
        self.qsearch_nodes = 0;
        self.pv.clearAll();
    }


    fn scoreMove(self: *Self, move: Move, ply: u32, tt_move: ?Move) i16 {
        if (tt_move == move) {
            return MOVESCORE_TT;
        }

        if (move.is_promotion and move.extra.promotion == .queen) {
            return MOVESCORE_PROMOTION;
        }

        if (self.per_ply[ply].killers[0] == move or self.per_ply[ply].killers[1] == move) {
            return MOVESCORE_KILLER;
        }

        if (move.is_capture) {
            // TODO SSE
            const material = evaluation.captureMoveMaterial(self.brd.data, move);

            if (material >= 0) {
                return MOVESCORE_GOOD_CAPTURE + material;
            }

            return MOVESCORE_BAD_CAPTURE + material;
        }

        const move_history = self.history.get(self.brd.data.side_to_move, move);
        return @min(@max(move_history, MOVESCORE_BAD_CAPTURE), MOVESCORE_KILLER);
    }

    fn orderMoves(self: *Self, moves: []Move, scores: []i16, ply: u32, tt_move: ?Move) void {
        const Context = struct {
            bot: *Self,
            moves: []Move,
            scores: []i16,

            pub fn lessThan(ctx: @This(), a: usize, b: usize) bool {
                return ctx.scores[a] > ctx.scores[b];
            }

            pub fn swap(ctx: @This(), a: usize, b: usize) void {
                std.mem.swap(Move, &ctx.moves[a], &ctx.moves[b]);
                std.mem.swap(i16, &ctx.scores[a], &ctx.scores[b]);
            }
        };

        for (0..moves.len) |i| {
            scores[i] = self.scoreMove(moves[i], ply, tt_move);
        }
        std.mem.sortUnstableContext(0, moves.len, Context { .bot = self, .moves = moves, .scores = scores });
        // std.sort.insertionContext(0, moves.len, Context { .bot = self, .moves = moves, .scores = scores });
    }

    fn qsearch(self: *Self, comptime node_type: NodeType, root_alpha: i16, root_beta: i16, ply: u32) i16 {
        const in_check = self.brd.data.is_in_check;

        if (self.is_exiting_search.load(.unordered)) {
            return 0;
        }

        self.pv.clearPly(ply);

        self.qsearch_nodes += 1;

        const is_pv_node = node_type == .Pv;
        std.debug.assert(is_pv_node or (root_alpha == root_beta - 1));

        if (self.brd.isDraw50Moves() or self.brd.repetitionsCount() > 0)
            return 0;

        var alpha = @max(root_alpha, @as(i16, @intCast(ply)) - SCORE_MATE_ABS);
        const beta = @min(root_beta, SCORE_MATE_ABS - @as(i16, @intCast(ply)) - 1);

        if (alpha >= beta) {
            return alpha;
        }

        var eval = self.evalPosition();

        const zobrist_key = self.brd.data.zobrist_key;
        var tt_move: ?Move = null;

        if (self.tt.probe(zobrist_key, ply)) |tt_entry| {
            if (!is_pv_node and (tt_entry.bound == .exact or 
                tt_entry.bound == .lower and tt_entry.score >= beta or
                tt_entry.bound == .upper and tt_entry.score <= alpha)) {
                return tt_entry.score;
            }

            tt_move = tt_entry.move;

            if (tt_entry.bound == .exact or 
                tt_entry.bound == .lower and tt_entry.score >= eval or
                tt_entry.bound == .upper and tt_entry.score <= eval) {
                eval = tt_entry.score;
            }
        }

        // TODO make something about that?
        if (ply >= MAX_PLY)
            return eval;


        if (eval >= beta) {
            return eval;
        }

        eval = @max(alpha, eval);
        
        // TODO more accurate delta pruning
        const delta = 1200;
        if (eval < alpha -| delta) {
            return alpha;
        }

        if (eval > alpha) {
            alpha = eval;
        }


        var moves = board.Moves{};
        board.genMoves(self.brd.data, &moves);
    
        // if (!in_check) {
        //     moves.filterCapturesOnly();
        // }
        moves.filterCapturesOnly();

        const moves_len = moves.count();
        
        var scores_buf: [board.MAX_MOVES]i16 = undefined;
        const scores = scores_buf[0..moves_len];
        self.orderMoves(moves.moves(), scores, ply, tt_move);

        // technically can be a refutation move
        var best_move: Move = Move.NULL;
        var tt_bound: tt.Bound = .upper;
        var moves_played: u32 = 0;

        for (moves.moves()) |move| {
            // TODO lower that when SEE move ordering is implemented
            // (for now fails SPRT)
            // if (!in_check and moves_played >= 4)
            //     break;

            if (self.brd.makeMove(move))
                continue;


            moves_played += 1;

            // TODO do something about draw
            // if (!move.is_capture) {
            //     // TODO
            //     self.brd.unmakeMove();
            //     continue;
            // }


            std.debug.assert(move.is_capture);
            const move_eval = -self.qsearch(node_type, -beta, -alpha, ply+1);
            self.brd.unmakeMove();

            if (self.is_exiting_search.load(.unordered)) {
                break;
            }

            if (move_eval >= beta) {
                tt_bound = .lower;
                best_move = move;
                self.pv.setMoveAtPly(move, ply);
                eval = move_eval;
                // return move_eval;
                break;
            }

            if (move_eval > eval) {
                // TODO update pv?
                eval = move_eval;
                best_move = move;
                self.pv.setMoveAtPly(move, ply);
                if (eval > alpha) { 
                    alpha = eval;
                    tt_bound = .exact;
                }
            }
        }

        // No legal moves
        // TODO smarter condition
        // TODO that doesn't work becasue we skipping quiet moves in qsearch
        if (moves_played == 0) {
            if (in_check) {
                return @as(i16, @intCast(ply)) - SCORE_MATE_ABS;
            }
            return eval;
        }

        if (self.is_exiting_search.load(.unordered)) {
            return 0;
        }
        
        self.tt.put(zobrist_key, ply, QS_TT_DEPTH, eval, tt_bound, best_move);

        return eval;
    }

    fn addKiller(self: *Self, ply: u32, move: Move) void {
        const killers = &self.per_ply[ply].killers;
        if (killers[0] != move and killers[1] != move) {
            killers[1] = killers[0]; 
            killers[0] = move; 
        }
    }


    fn search(
        self: *Self,
        comptime node_type: NodeType,
        root_alpha: i16,
        root_beta: i16,
        remaining_depth: u32,
        ply: u32,
        extensions_used: u32,
        allow_null: bool,
    ) i16 {
        if (ply > 0 and self.is_exiting_search.load(.unordered)) {
            return 0;
        }

        if (self.nodes % 1024 == 0) {
            // TODO better prevent exiting from "depth 1 search" with 0 time left
            if (ply > 0 and self.timeRemaining() <= TIME_EPS_NS) {
                self.is_exiting_search.store(true, .unordered);
                return 0;
            }
        }

        const in_check = self.brd.data.is_in_check;
        const is_pv_node = node_type == .Pv;
        std.debug.assert(is_pv_node or (root_alpha == root_beta - 1));

        self.pv.clearPly(ply);
        self.nodes += 1;

        var alpha = root_alpha;
        var beta = root_beta;

        if (ply > 0) {
            alpha = @max(alpha, @as(i16, @intCast(ply)) - SCORE_MATE_ABS);
            beta = @min(beta, SCORE_MATE_ABS - @as(i16, @intCast(ply)) - 1);
            if (alpha >= beta) {
                return alpha;
            }
        }

        if (ply > 0 and (self.brd.isDraw50Moves() or self.brd.repetitionsCount() > 0))
            return 0;

        const zobrist_key = self.brd.data.zobrist_key;
        var tt_move: ?Move = null;

        if (self.tt.probe(zobrist_key, ply)) |tt_entry| {
            if (!is_pv_node and tt_entry.depth >= remaining_depth and ply > 0) { 
                if (tt_entry.bound == .exact or 
                    tt_entry.bound == .lower and tt_entry.score >= beta or
                    tt_entry.bound == .upper and tt_entry.score <= alpha) {
                    return tt_entry.score;
                }
            }

            tt_move = tt_entry.move;
    
            // TODO ..?
            // if (tt_entry.bound == .exact or 
            //     tt_entry.bound == .lower and tt_entry.score >= eval or
            //     tt_entry.bound == .upper and tt_entry.score <= eval) {
            //     eval = tt_entry.score;
            // }
        }

        const static_eval = self.qsearch(node_type, alpha, beta, ply);
        if (ply > 0 and self.is_exiting_search.load(.unordered)) {
            // TODO?
            return 0;
        }

        if (remaining_depth == 0) {
            return static_eval;
        }

        // TODO make something about that?
        if (ply >= MAX_PLY)
            return static_eval;

        var eval: i16 = -SCORE_INFINITY;

        if (!is_pv_node and !in_check) {
            // RFP
            if (@abs(alpha) < 2000) {
                const margin = @as(i16, @intCast(150 * remaining_depth));
                if (static_eval >= beta + margin ) {
                    return static_eval;
                }
                if (static_eval - margin > alpha) {
                    alpha = static_eval - margin;
                }
            }

            // NMP
            if (allow_null and @abs(beta) < SCORE_MATE_ABS and remaining_depth > 1 and ply > 0) {
                if (!self.brd.makeNullMove()) {
                    const depth_reduction = @min(1 + remaining_depth/2, 4);
                    const nm_score = -self.search(.NonPv, -beta, -beta + 1, remaining_depth - depth_reduction, ply+1, extensions_used, false);
                    self.brd.unmakeNullMove();
                    if (nm_score >= beta) {
                        return nm_score;
                        // TODO more conditions on NMP / double -> enable verification
                        // const nm_score_verif = self.search(.NonPv, beta - 1, beta, remaining_depth - depth_reduction, ply, extensions_used, false);
                        // if (nm_score_verif >= beta) {
                        //     return nm_score;
                        // }
                    }
                }
            }
        }


        var moves = board.Moves{};
        board.genMoves(self.brd.data, &moves);

        const moves_len = moves.count();

        if (moves_len == 0) {
            if (in_check) {
                return @as(i16, @intCast(ply)) - SCORE_MATE_ABS;
            }
            return 0;
        }

        var scores_buf: [board.MAX_MOVES]i16 = undefined;
        const scores = scores_buf[0..moves_len];
        self.orderMoves(moves.moves(), scores, ply, tt_move);

        // technically can be a refutation move
        var best_move: Move = Move.NULL;
        var tt_bound: tt.Bound = .upper;

        var legal_moves: u32 = 0;

        for (moves.moves(), scores) |move, score| {
            if (self.brd.makeMove(move))
                continue;

            const new_in_check = self.brd.data.is_in_check;

            _ = score;
            legal_moves += 1;
            var search_ext: u32 = 0;
            if (new_in_check) {
                search_ext += 1;
            }
            if (move.is_promotion and move.extra.promotion == .queen) {
                search_ext += 1;
            }
            if (moves_len == 1) {
                search_ext += 1;
            }
            search_ext = @min(search_ext + extensions_used, MAX_EXTENSIONS) - extensions_used;
 
            // TODO lmr
            var reduction: u32 = 0;
            // if (legal_moves > 4 and remaining_depth > 1 and !in_check and score < MOVESCORE_KILLER) {
            //     reduction = ???;
            //     reduction -|= @intFromBool(is_pv_node);
            // }
            _ = &reduction;

            // TODO 

            // pvs
            var move_eval: i16 = -SCORE_INFINITY;
            if (remaining_depth < 2 or legal_moves < 3) {
                move_eval = -self.search(node_type, -beta, -alpha, remaining_depth - 1 + search_ext, ply + 1, extensions_used + search_ext, false);
            } else {
                // TODO remove extensions from this those 2
                move_eval = -self.search(.NonPv, -(alpha+1), -alpha, remaining_depth - 1 - reduction + search_ext, ply + 1, extensions_used + search_ext, true);
                if (move_eval > alpha and (is_pv_node or reduction > 0)) {
                    move_eval = -self.search(.Pv, -beta, -alpha, remaining_depth - 1 + search_ext, ply + 1, extensions_used + search_ext, false);
                }
            }

            self.brd.unmakeMove();

            if (ply > 0 and self.is_exiting_search.load(.unordered)) {
                break;
            }

            if (move_eval >= beta) {
                if (!move.is_capture and !move.is_promotion) {
                    self.addKiller(ply, move);
                }
                eval = move_eval;
                best_move = move;
                self.pv.setMoveAtPly(move, ply);

                tt_bound = .lower;
                break;
            }

            if (move_eval > eval) {
                eval = move_eval;
                best_move = move;
                self.pv.setMoveAtPly(move, ply);

                if (eval > alpha) { 
                    alpha = eval;
                    tt_bound = .exact;
                }
            }
        }

        if (legal_moves == 0) {
            std.debug.assert(ply > 0);
            if (in_check) {
                return @as(i16, @intCast(ply)) - SCORE_MATE_ABS;
            }
            return 0;
        }

        if (best_move != Move.NULL and !best_move.is_capture and tt_bound == .lower) {
            const color = self.brd.data.side_to_move;
            const bonus: i16 = @intCast(remaining_depth * remaining_depth);
            self.history.updateHistoryGravity(color, best_move, bonus);
            for (moves.moves()) |move| {
                if (move.is_capture) continue;
                if (move == best_move) break;
                self.history.updateHistoryGravity(color, move, -bonus);
            }
        }

        std.debug.assert(ply != 0 or best_move != Move.NULL);
        std.debug.assert(self.pv.perPly(ply)[0] == best_move);

        // prevent storing to tt 
        if (ply > 0 and self.is_exiting_search.load(.unordered)) {
            // TODO save to TT as lower bound instead
            return 0;
        }

        self.tt.put(zobrist_key, ply, @intCast(remaining_depth), eval, tt_bound, best_move);

        return eval;
    }

    pub fn timeRemaining(self: *Self) u64 {
        const now = std.time.Instant.now() catch unreachable;
        return self.available_time -| now.since(self.search_start);
    }

    pub fn bestMove(self: *Self, brd: *Board, uci_connection: *uci.UciConnection, time_controls: TimeControls) !void {
        self.brd = brd;

        var depth_target: u32 = MAX_PLY;
        var available_time: u64 = MAX_THINKING_TIME_NS;

        switch (time_controls) {
            .infinite => {},
            .nodes => @panic("TODO: go nodes"),
            .to_depth => |c| depth_target = @min(depth_target, @max(c.target, 1)),
            .time_remaining => |c| available_time = @min(available_time, c.ns),
        }
        
        self.available_time = available_time;
        self.search_start = std.time.Instant.now() catch unreachable;
        
        self.history.setToZero();

        var best_move: Move = .NULL;

        var d: u32 = 0;
        while (true) {
            d += 1;

            self.preSearchCleanup();

            const search_iteration_beg = std.time.Instant.now() catch unreachable;
            const score = self.search(.Pv, -SCORE_INFINITY, SCORE_INFINITY, d, 0, 0, false);
            const search_iteration_end = std.time.Instant.now() catch unreachable;
            const iteration_time = search_iteration_end.since(search_iteration_beg);

            const is_canceled = self.is_exiting_search.load(.unordered);

            if (!is_canceled) {
                const info: uci.SearchInfo = .{
                    .depth = d,
                    .time_ms = iteration_time / std.time.ns_per_ms,
                    .nodes = self.nodes,
                    .nps =  @as(u64, @intCast(@as(u96, self.nodes) * std.time.ns_per_s / @max(iteration_time, 1))),
                    .pv = self.pv.perPly(0),
                    .score = score,
                };

                try uci_connection.searchInfo(info); 
            }

            const is_exiting = d >= depth_target or is_canceled;

            // for now don't trust unfinished iterations if ply > 0
            if (!is_exiting or d == 1) {
                best_move = self.pv.perPly(0)[0];
                std.debug.assert(best_move != Move.NULL);
            }

            if (is_exiting) {
                break;
            }
        }

        std.debug.assert(best_move != Move.NULL);
        try uci_connection.bestmove(best_move);
    }

    pub fn searchToMate(self: *Self, brd: *Board, time_limit_ns: u64) !u32 {
        self.brd = brd;

        self.available_time = time_limit_ns;
        self.search_start = std.time.Instant.now() catch unreachable;
        
        self.history.setToZero();

        var depth_limit: u32 = MAX_PLY;
        var found_at: u16 = MAX_PLY;

        var d: u32 = 0;
        while (d < depth_limit) {
            d += 1;

            self.preSearchCleanup();

            const score = self.search(.Pv, -SCORE_INFINITY, SCORE_INFINITY, d, 0, 0, false);
            const is_canceled = self.is_exiting_search.load(.unordered);
            
            if (is_canceled) {
                return error.TimeExpired;
            }
            
            if (scoreToMateInPlyAbs(score)) |mate_at_ply| {
                found_at = @min(found_at, mate_at_ply);
                // TODO smarter depth limit
                depth_limit = @min(depth_limit, mate_at_ply+6);
            }
        }

        return plyToMoves(found_at);
    }
};

fn trimInvalidPvMoves(moves: []const Move) []const Move {
    const len = std.mem.indexOfScalar(Move, moves, Move.NULL) orelse moves.len;
    return moves[0..len];
}

pub const SearchControl = struct {
    // TODO for adding real multithreading turn that into array
    // and do something smart about synchronization
    search_thread_thread: std.Thread,
    search_thread: SearchThread,
    // TODO move that to SearchThread
    brd: Board,
    uci_connection: *uci.UciConnection, 
    time_controls: TimeControls,

    is_searching: bool,
    is_exiting: bool,

    mutex: std.Thread.Mutex,
    cond: std.Thread.Condition,
    search_end_cond: std.Thread.Condition,

    waiting: std.atomic.Value(u32),

    const Self = @This();

    pub fn init(alloc: Alloc, uci_connection: *uci.UciConnection) !*Self {
        const self = try alloc.create(Self);

        self.mutex = .{};
        self.brd = try .init(alloc, .DEFAULT);
        self.is_searching = false;
        self.is_exiting = false;
        self.cond = .{};
        self.search_end_cond = .{};
        self.search_thread = try SearchThread.init(alloc);
        self.waiting = .init(0);
        self.uci_connection = uci_connection;
        self.search_thread_thread = try std.Thread.spawn(.{ .allocator = alloc }, searchThreadEntry, .{ self, &self.search_thread });

        return self;
    }

    pub fn deinit(self: *Self) void {
        self.mutex.lock();
        self.is_exiting = true;
        self.mutex.unlock();
        self.cond.broadcast();
        self.search_thread_thread.join();
        self.search_thread.deinit();
        self.brd.deinit();
    }

    pub fn startSearch(self: *Self, brd: *const Board, time_controls: TimeControls) !void {
        {
            self.mutex.lock();
            defer self.mutex.unlock();

            // TODO copy into each thread
            try self.brd.cloneFrom(brd);
            self.time_controls = time_controls;
            self.search_thread.is_exiting_search.store(false, .release);
        }
        self.cond.broadcast();
    }

    pub fn signalStopSearch(self: *Self) !void {
        self.search_thread.is_exiting_search.store(true, .release);
    }
    
    pub fn waitUntilSearchEnded(self: *Self) !void {
        self.mutex.lock();
        defer self.mutex.unlock();
        
        while (self.is_searching) {
            self.search_end_cond.wait(&self.mutex);
        }
    }

    fn searchThreadEntry(control: *Self, search_thread: *SearchThread) !void {
        control.mutex.lock();
        defer control.mutex.unlock();

        while (true) {
            _ = control.waiting.fetchAdd(1, .release);
            control.cond.wait(&control.mutex);
            _ = control.waiting.fetchSub(1, .release);

            if (control.is_exiting) {
                break;
            }
            control.is_searching = true;
            
            {
                control.mutex.unlock();
                defer control.mutex.lock();

                // TODO run figure out propper way to run that in multiple threads
                try search_thread.bestMove(&control.brd, control.uci_connection, control.time_controls);
            }

            // TODO this would work only for single thread...
            control.is_searching = false;
            control.search_end_cond.broadcast();
        }
    }
};
