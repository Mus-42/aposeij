const std = @import("std");
const board = @import("board");
const tt = @import("transposition_table.zig");
const evaluation = @import("evaluation.zig");
const uci = @import("uci.zig");

const Alloc = std.mem.Allocator;
const Move = board.Move;
const PieceKind = board.PieceKind;
const Board = board.Board;

pub const MAX_DEPTH = 128;
pub const MAX_THINKING_TIME_NS = 30 * std.time.ns_per_min; 
pub const MAX_EXTENSIONS = 8;

pub const QS_TT_DEPTH = 0;

pub const SCORE_MATE_ABS: i16 = 32000;
pub const SCORE_MATE_EPS: i16 = MAX_DEPTH;
pub const SCORE_INFINITY: i16 = SCORE_MATE_ABS+1;

pub const TIME_EPS_NS: u64 = 200;

pub const NodeType = enum {
    Pv,
    NonPv
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

pub fn ButterflyBoard(comptime Counter: type) type {
    return struct {
        // TODO somehow avoid wasting most of the memory on impossible moves
        counters: [2 * 64 * 64]Counter,

        const Self = @This();

        pub fn index(color: board.SideToMove, move: Move) usize {
            return @as(usize, @intFromEnum(color)) << 12 | @as(usize, move.from) << 6 | @as(usize, move.to) << 6;
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
    };
}

pub const SearchThread = struct {
    alloc: Alloc,
    brd: *Board = undefined,
    tt: tt.TTable,
    pv_moves: []Move,
    per_ply: *[MAX_DEPTH]PerPly,
    history: *History,
    stats: Stats = .{},
    search_start: std.time.Instant = undefined,
    available_time: u64 = 0,
    allow_search_cancellation: bool = false,
    // TODO get if from controls
    is_exiting_search: std.atomic.Value(bool),

    const History = ButterflyBoard(i16);

    const PerPly = struct {
        killers: [2]Move,
    };

    const Self = @This();

    pub fn init(alloc: Alloc) !Self {
        const pv_moves = try alloc.alloc(Move, MAX_DEPTH * (MAX_DEPTH + 1) / 2);
        const per_ply = try alloc.create([MAX_DEPTH]PerPly);
        const history = try alloc.create(History);
        const ttable = try tt.TTable.init(alloc);

        return .{
            .alloc = alloc,
            .tt = ttable,
            .pv_moves = pv_moves,
            .per_ply = per_ply,
            .history = history,
            .is_exiting_search = .init(false),
        };
    }

    pub fn deinit(self: *Self) void {
        self.tt.deinit(self.alloc);
        self.alloc.free(self.pv_moves);
        self.alloc.destroy(self.per_ply);
        self.alloc.destroy(self.history);
    }

    pub fn eval(self: *Self) i16 {
        self.stats.evals += 1;
        const white_score = evaluation.whiteEval(self.brd.data);
        if (self.brd.data.side_to_move == .white) {
            return white_score;
        } else {
            return -white_score;
        }
    }

    fn preSearchCleanup(self: *Self) void {
        self.stats = .{};
        @memset(self.perPlyPv(0), Move.NULL);
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

        if (self.perPlyPv(ply)[0] == move) {
            score += 50;
        }

        if (self.per_ply[ply].killers[0] == move or self.per_ply[ply].killers[1] == move) {
            score += 100;
        }

        if (move.is_promotion) {
            if (move.extra.promotion == .queen) {
                score += 100;
            } else {
                score -= 100;
            }
        }

        if (move.is_capture) {
            score += 10 + evaluation.captureMoveMaterial(self.brd.data, move);
        }

        return score;
    }

    fn orderMoves(self: *Self, moves: []Move, ply: u32, tt_move: ?Move) void {
        const Context = struct {
            bot: *Self,
            moves: []Move,
            scores: []i32,

            pub fn lessThan(ctx: @This(), a: usize, b: usize) bool {
                if (ctx.scores[a] != ctx.scores[b]) {
                    return ctx.scores[a] > ctx.scores[b];
                }

                if (!ctx.moves[a].is_capture and !ctx.moves[b].is_capture) {
                    const color = ctx.bot.brd.data.side_to_move;
                    const a_score = ctx.bot.history.get(color, ctx.moves[a]);
                    const b_score = ctx.bot.history.get(color, ctx.moves[b]);
                    return a_score > b_score;
                }

                return false;
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
        std.mem.sortUnstableContext(0, moves.len, Context { .bot = self, .moves = moves, .scores = scores[0..moves.len] });
    }

    fn quiescenceSearch(self: *Self, comptime node_type: NodeType, root_alpha: i16, beta: i16, ply: u32) !i16 {
        if (self.allow_search_cancellation and (self.is_exiting_search.load(.seq_cst) or self.timeRemaining() <= TIME_EPS_NS)) {
            return error.TimeExpired;
        }

        const pos_repetitions = self.brd.repetitionsCount();
        if (pos_repetitions > 0 or self.brd.isDraw50Moves())
            return 0;

        const is_pv_node = node_type == .Pv;
        std.debug.assert(is_pv_node or (root_alpha == beta - 1));

        const zobrist_key = self.brd.data.zobrist_key;
        var tt_move: ?Move = null;

        if (self.tt.probe(zobrist_key, ply)) |tt_entry| {
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

        if (moves.count() == 0) {
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
        var tt_bound: tt.Bound = .upper;

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
        
        self.tt.put(zobrist_key, ply, QS_TT_DEPTH, score, tt_bound, best_move);

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
        if (self.allow_search_cancellation and (self.is_exiting_search.load(.seq_cst) or self.timeRemaining() <= TIME_EPS_NS)) {
            return error.TimeExpired;
        }
        
        const is_pv_node = node_type == .Pv;
        std.debug.assert(is_pv_node or (root_alpha == beta - 1));

        if (is_pv_node) {
            @memset(self.perPlyPv(ply), Move.NULL);
        }

        const pos_repetitions = self.brd.repetitionsCount();
        if ((pos_repetitions > 0 or self.brd.isDraw50Moves()) and ply > 0)
            return 0;

        self.stats.nodes_all += 1;

        const zobrist_key = self.brd.data.zobrist_key;
        var tt_move: ?Move = null;

        if (self.tt.probe(zobrist_key, ply)) |tt_entry| {
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

        const static_eval = try self.quiescenceSearch(node_type, root_alpha, beta, ply);

        // TODO make something about that?
        if (ply == MAX_DEPTH)
            return static_eval;

        var score = -SCORE_INFINITY;
        var alpha = root_alpha;

        if (!is_pv_node and !self.brd.isInCheck() and @abs(alpha) < 2000) {
            const margin = @as(i16, @intCast(150 * remaining_depth));
            if (static_eval >= beta + margin ) {
                self.stats.beta_cutoffs += 1;
                return static_eval;
            }
            if (static_eval - margin > alpha) {
                alpha = static_eval - margin;
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

        // technically can be a refutation move
        var best_move: Move = Move.NULL;
        var tt_bound: tt.Bound = .upper;

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

        if (best_move != Move.NULL and !best_move.is_capture) {
            const color = self.brd.data.side_to_move;
            const bonus: i16 = @intCast(remaining_depth * remaining_depth);
            self.history.add(color, best_move, bonus);
            for (moves.moves()) |move| {
                if (move.is_capture) continue;
                if (move == best_move) break;
                self.history.add(color, move, -bonus);
            }
        }

        self.tt.put(zobrist_key, ply, @intCast(remaining_depth), score, tt_bound, best_move);

        return score;
    }

    pub fn timeRemaining(self: *Self) u64 {
        const now = std.time.Instant.now() catch unreachable;
        return self.available_time -| now.since(self.search_start);
    }

    pub fn bestMove(self: *Self, brd: *Board, uci_connection: *uci.UciConnection, time_controls: TimeControls) !Move {
        self.brd = brd;

        var depth_target: u32 = MAX_DEPTH;
        var available_time: u64 = MAX_THINKING_TIME_NS;

        switch (time_controls) {
            .infinite => {},
            .to_depth => |c| depth_target = @min(depth_target, c.target),
            .time_remaining => |c| available_time = @min(available_time, c.ns),
        }
        
        self.available_time = available_time;
        self.search_start = std.time.Instant.now() catch unreachable;
        
        // make full search at least to depth 1 to have move to play
        self.allow_search_cancellation = false;
        self.history.setToZero();

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
            var is_search_finished = true;
            if (self.search(.Pv, -SCORE_INFINITY, SCORE_INFINITY, @intCast(d), 0, 0)) |s| {
                score = s; 
            } else |err| {
                if (err == error.TimeExpired) {
                    is_search_finished = false;
                }
            }
            std.debug.assert(d != 1 or is_search_finished);

            self.allow_search_cancellation = true;

            const search_iteration_end = std.time.Instant.now() catch unreachable;
            const iteration_time = search_iteration_end.since(search_iteration_beg);
            last_iteration_time = iteration_time;

            if (is_search_finished) {
                best_move = self.perPlyPv(0)[0];
            }

            try uci_connection.searchInfo(.{
                .depth = @intCast(d),
                .time_ms = iteration_time / std.time.ns_per_ms,
                .nodes = self.stats.nodes_all,
                .nps =  @as(u64, @intCast(@as(u96, self.stats.nodes_all) * std.time.ns_per_s / @max(iteration_time, 1))),
                .pv = trimInvalidPvMoves(self.perPlyPv(0)),
                .score = score,
            }); 

            // time expired | cancellation request
            if (!is_search_finished) {
                break;
            }
        }

        std.debug.assert(best_move != Move.NULL);

        return best_move;
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

    // stop_request: bool,
    is_exiting: bool,

    mutex: std.Thread.Mutex,
    cond: std.Thread.Condition,

    const Self = @This();

    pub fn init(alloc: Alloc, uci_connection: *uci.UciConnection) !*Self {
        const self = try alloc.create(Self);
        self.mutex = .{};
        self.brd = try .init(alloc, .DEFAULT);
        self.is_exiting = false;
        self.cond = .{};
        self.search_thread = try SearchThread.init(alloc);
        self.search_thread_thread = try std.Thread.spawn(.{ .allocator = alloc }, searchThreadEntry, .{ self, &self.search_thread });
        self.uci_connection = uci_connection;

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
        self.mutex.lock();
        defer self.mutex.unlock();
        // TODO copy into each thread
        try self.brd.cloneFrom(brd);
        self.time_controls = time_controls;
        self.search_thread.is_exiting_search.store(false, .seq_cst);
        self.cond.broadcast();
    }

    pub fn stopSearch(self: *Self) !void {
        self.search_thread.is_exiting_search.store(true, .seq_cst);
    }

    fn searchThreadEntry(control: *Self, search_thread: *SearchThread) !void {
        control.mutex.lock();
        defer control.mutex.unlock();

        while (true) {
            control.cond.wait(&control.mutex);
            if (control.is_exiting) {
                break;
            }

            control.mutex.unlock();
            defer control.mutex.lock();

            // TODO run figure out propper way to run that
            const best_move = try search_thread.bestMove(&control.brd, control.uci_connection, control.time_controls);
            try control.uci_connection.bestmove(best_move);
        }
    }
};
