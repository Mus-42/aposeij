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

pub const SCORE_MATE_ABS: i16 = 32000;
pub const SCORE_MATE_EPS: i16 = MAX_PLY;
pub const SCORE_INFINITY: i16 = SCORE_MATE_ABS+1;

pub const TIME_EPS_NS: u64 = 200;

pub const NodeType = enum {
    Pv,
    NonPv
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
    per_ply: *[MAX_PLY]PerPly,
    history: *History,

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
        const pv_moves = try alloc.alloc(Move, MAX_PLY * (MAX_PLY + 1) / 2);
        const per_ply = try alloc.create([MAX_PLY]PerPly);
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
        @memset(self.perPlyPv(0), Move.NULL);
        // @memset(self.pv_moves, Move.NULL);
    }

    // order:
    // hash
    // pv
    // captures (good)
    // killers
    // quiet
    // captured (bad)
    const MOVESCORE_TT = 0x7FFF;
    const MOVESCORE_GOOD_CAPTURE = 0x5000;
    const MOVESCORE_PROMOTION = 0x5000;
    const MOVESCORE_KILLER = 0x5000;
    const MOVESCORE_BAD_CAPTURE = -0x4000;

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
    }

    fn quiescenceSearch(self: *Self, comptime node_type: NodeType, root_alpha: i16, root_beta: i16, ply: u32) i16 {
        if (self.is_exiting_search.load(.unordered)) {
            return 0;
        }

        self.qsearch_nodes += 1;

        const is_pv_node = node_type == .Pv;
        std.debug.assert(is_pv_node or (root_alpha == root_beta - 1));

        const pos_repetitions = self.brd.repetitionsCount();
        if (pos_repetitions > 0 or self.brd.isDraw50Moves())
            return 0;

        var alpha = @max(root_alpha, @as(i16, @intCast(ply)) - SCORE_MATE_ABS);
        const beta = @min(root_beta, SCORE_MATE_ABS - @as(i16, @intCast(ply)) - 1);

        if (alpha >= beta) {
            return alpha;
        }

        const zobrist_key = self.brd.data.zobrist_key;
        var tt_move: ?Move = null;

        if (self.tt.probe(zobrist_key, ply)) |tt_entry| {
            if (tt_entry.bound == .exact or 
                tt_entry.bound == .lower and tt_entry.score >= beta or
                tt_entry.bound == .upper and tt_entry.score <= root_alpha) {
                return tt_entry.score;
            }

            tt_move = tt_entry.move;
        }

        const static_eval = self.evalPosition();

        // TODO make something about that?
        if (ply >= MAX_PLY)
            return static_eval;

        var eval = static_eval;

        if (static_eval >= beta) {
            return static_eval;
        }
        if (static_eval > alpha) {
            alpha = static_eval;
        }

        var moves = board.Moves{};
        board.genMoves(self.brd.data, &moves);

        if (moves.count() == 0) {
            if (self.brd.data.is_in_check) {
                return @as(i16, @intCast(ply)) - SCORE_MATE_ABS;
            }
            return 0;
        }

        moves.filterCapturesOnly();

        const moves_len = moves.moves().len;

        if (moves_len == 0) {
            return static_eval;
        }
        
        var scores_buf: [board.MAX_MOVES]i16 = undefined;
        const scores = scores_buf[0..moves_len];
        self.orderMoves(moves.moves(), scores, ply, tt_move);

        // technically can be a refutation move
        var best_move: Move = Move.NULL;
        var tt_bound: tt.Bound = .upper;
        var has_moves = false;

        for (moves.moves()) |move| {
            std.debug.assert(move.is_capture);

            if (self.brd.makeMove(move))
                continue;
            has_moves = true;

            const move_eval = -self.quiescenceSearch(node_type, -beta, -alpha, ply+1);
            self.brd.unmakeMove();

            if (self.is_exiting_search.load(.unordered)) {
                break;
            }

            if (move_eval >= beta) {
                tt_bound = .lower;
                best_move = move;
                eval = move_eval;
                // return move_eval;
                break;
            }

            if (move_eval > eval) {
                // TODO update pv?
                eval = move_eval;
                best_move = move;
                if (eval > alpha) { 
                    alpha = eval;
                    tt_bound = .exact;
                }
            }
        }

        // No legal moves
        // TODO smarter condition
        if (!has_moves) {
            if (self.brd.data.is_in_check) {
                return @as(i16, @intCast(ply)) - SCORE_MATE_ABS;
            }
            return 0;
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

    fn perPlyPv(self: *Self, ply: u32) []Move {
        const count = MAX_PLY - ply;
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
        root_beta: i16,
        remaining_depth: u32,
        ply: u32,
        extensions_used: u32,
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

        const is_pv_node = node_type == .Pv;
        std.debug.assert(is_pv_node or (root_alpha == root_beta - 1));

        // TODO be smarter with that, do less work?
        @memset(self.perPlyPv(ply), Move.NULL);
        // const count = MAX_PLY - ply;
        // const start = self.pv_moves.len - (count + 1) * count / 2;
        // @memset(self.pv_moves[start..], Move.NULL);

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

        const pos_repetitions = self.brd.repetitionsCount();
        if ((pos_repetitions > 0 or self.brd.isDraw50Moves()) and ply > 0)
            return 0;

        const zobrist_key = self.brd.data.zobrist_key;
        var tt_move: ?Move = null;

        if (self.tt.probe(zobrist_key, ply)) |tt_entry| {
            if (!is_pv_node and tt_entry.depth >= remaining_depth and ply > 0) { 
                if (tt_entry.bound == .exact or 
                    tt_entry.bound == .lower and tt_entry.score >= beta or
                    tt_entry.bound == .upper and tt_entry.score <= root_alpha) {
                    return tt_entry.score;
                }
            }

            tt_move = tt_entry.move;
        }

        const static_eval = self.quiescenceSearch(node_type, alpha, beta, ply);
        if (ply > 0 and self.is_exiting_search.load(.unordered)) {
            // TODO?
            return 0;
        }

        // TODO make something about that?
        if (ply >= MAX_PLY)
            return static_eval;

        var eval = -SCORE_INFINITY;

        if (!is_pv_node and !self.brd.data.is_in_check and @abs(alpha) < 2000) {
            const margin = @as(i16, @intCast(150 * remaining_depth));
            if (static_eval >= beta + margin ) {
                return static_eval;
            }
            if (static_eval - margin > alpha) {
                alpha = static_eval - margin;
            }
        }

        if (remaining_depth == 0) {
            return static_eval;
        }

        var moves = board.Moves{};
        board.genMoves(self.brd.data, &moves);

        const moves_len = moves.moves().len;

        if (moves_len == 0) {
            if (self.brd.data.is_in_check) {
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

        for (0.., moves.moves()) |i, move| {
            std.debug.assert(self.perPlyPv(ply)[0] == best_move);

            if (self.brd.makeMove(move))
                continue;

            var search_ext: u32 = 0;
            if (self.brd.data.is_in_check) {
                search_ext += 1;
            }
            if (move.is_promotion and move.extra.promotion == .queen) {
                search_ext += 1;
            }
            if (moves_len == 1) {
                search_ext += 1;
            }
            search_ext = @min(search_ext + extensions_used, MAX_EXTENSIONS) - extensions_used;
            
            var move_eval: i16 = -SCORE_INFINITY;
            if (remaining_depth < 2 or i < 3) {
                move_eval = -self.search(node_type, -beta, -alpha, remaining_depth - 1 + search_ext, ply + 1, extensions_used + search_ext);
            } else {
                move_eval = -self.search(.NonPv, -(alpha+1), -alpha, remaining_depth - 1, ply + 1, extensions_used + search_ext);
                if (move_eval > alpha and is_pv_node) {
                    move_eval = -self.search(.Pv, -beta, -alpha, remaining_depth - 1, ply + 1, extensions_used);
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
                self.setNewPvMoveAtPly(move, ply);

                tt_bound = .lower;
                break;
            }

            if (move_eval > eval) {
                eval = move_eval;
                best_move = move;
                self.setNewPvMoveAtPly(move, ply);

                if (eval > alpha) { 
                    alpha = eval;
                    tt_bound = .exact;
                }
            }
        }

        // Should be the same condition
        // Reachable if and only if has 0 legal moves
        if (eval == -SCORE_INFINITY or best_move == Move.NULL) {
            std.debug.assert(ply > 0);
            if (self.brd.data.is_in_check) {
                return @as(i16, @intCast(ply)) - SCORE_MATE_ABS;
            }
            return 0;
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

        std.debug.assert(ply != 0 or best_move != Move.NULL);
        std.debug.assert(self.perPlyPv(ply)[0] == best_move);

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
            const score = self.search(.Pv, -SCORE_INFINITY, SCORE_INFINITY, d, 0, 0);
            const search_iteration_end = std.time.Instant.now() catch unreachable;
            const iteration_time = search_iteration_end.since(search_iteration_beg);

            const info: uci.SearchInfo = .{
                .depth = d,
                .time_ms = iteration_time / std.time.ns_per_ms,
                .nodes = self.nodes,
                .nps =  @as(u64, @intCast(@as(u96, self.nodes) * std.time.ns_per_s / @max(iteration_time, 1))),
                .pv = trimInvalidPvMoves(self.perPlyPv(0)),
                .score = score,
            };

            try uci_connection.searchInfo(info); 

            // for now don't trust unfinished iterations if ply > 0
            const is_exiting = d >= depth_target or self.is_exiting_search.load(.unordered);

            if (!is_exiting or d == 1) {
                best_move = self.perPlyPv(0)[0];
                std.debug.assert(best_move != Move.NULL);
            }

            if (is_exiting) {
                break;
            }
        }

        std.debug.assert(best_move != Move.NULL);
        try uci_connection.bestmove(best_move);
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
