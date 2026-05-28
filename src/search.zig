const std = @import("std");
const board = @import("board.zig");
const tt = @import("transposition_table.zig");
const evaluation = @import("evaluation.zig");
const uci = @import("uci.zig");
const hist = @import("history.zig");
const logs = @import("search_log.zig");

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

pub const TIME_EPS_NS: u64 = 2000;


// order:
// hash
// pv
// captures (good)
// killers
// quiet
// captured (bad)
pub const MOVESCORE_TT = 0x7FFF;
pub const MOVESCORE_GOOD_CAPTURE = 0x5100;
pub const MOVESCORE_PROMOTION = 0x5200;
pub const MOVESCORE_KILLER = 0x5000;
pub const MOVESCORE_BAD_CAPTURE = -0x5000;

pub fn scoreToMateInPlyAbs(score: i16) ?u16 {
    if (@abs(score) > SCORE_MATE_ABS) return null;
    const mate_ply = SCORE_MATE_ABS - @abs(score);
    if (mate_ply > SCORE_MATE_EPS) return null;
    return mate_ply;
}

pub fn plyToMoves(ply: u16) u16 {
    return (1 + ply) / 2;
}

pub fn scoreToMateInMovesAbs(score: i16) ?u16 {
    if (@abs(score) > SCORE_MATE_ABS) return null;
    const mate_ply = SCORE_MATE_ABS - @abs(score);
    if (mate_ply > SCORE_MATE_EPS) return null;
    return plyToMoves(mate_ply);
}

pub const TimeControls = struct {
    io: std.Io,
    available_time_ns: u64 = MAX_THINKING_TIME_NS,
    depth_limit: u32 = MAX_PLY,
    nodes_limit: u64 = std.math.maxInt(u64),
    search_start: std.Io.Timestamp = undefined,
    is_exiting_search: std.atomic.Value(bool) = .init(false),

    const Self = @This();

    pub fn toDepth(io: std.Io, target: u32) Self {
        return .{ .io = io, .depth_limit = target };
    }

    pub fn toNodes(io: std.Io, target: u64) Self {
        return .{ .io = io, .nodes_limit = target };
    }

    pub fn infinite(io: std.Io) Self {
        return .{ .io = io };
    }

    pub fn toTime(io: std.Io, limit_ns: u64) Self { 
        return .{ .io = io, .available_time_ns = limit_ns };
    }

    pub fn startSearch(self: *Self) void {
        self.is_exiting_search.store(false, .unordered);
        // TODO std.Io.Timestamp.now(io, .awake);
        self.search_start = std.Io.Timestamp.now(self.io, .awake);
    }

    pub fn isStopSet(self: *Self) bool {
        return self.is_exiting_search.load(.unordered);
    }

    pub fn isHardStop(self: *Self, nodes: u64, qnodes: u64) bool {
        if (self.isStopSet())
            return true;
        if (nodes >= self.nodes_limit) {
            self.is_exiting_search.store(true, .unordered);
            return true;
        }
        const total_nodes = nodes + qnodes;
        if (total_nodes & 0x3FF == 0x3FF) {
            const passed = self.search_start.untilNow(self.io, .awake);
            if (self.available_time_ns -| passed.nanoseconds <= TIME_EPS_NS) {
                self.is_exiting_search.store(true, .unordered);
                return true;
            }
        }
        return false;
    }

    pub fn isSoftStop(self: *Self, depth: u32) bool {
        if (self.isStopSet())
            return true;
        if (depth >= self.depth_limit) {
            self.is_exiting_search.store(true, .unordered);
            return true;
        }
        const passed = self.search_start.untilNow(self.io, .awake);
        if (self.available_time_ns -| (@divTrunc(passed.nanoseconds * 5, 3)) <= TIME_EPS_NS) {
            self.is_exiting_search.store(true, .unordered);
            return true;
        }
        return false;
    }
};

const PVMoves = struct {
    root_pv: []Move,
    pv_moves: []Move,
    pv_len: []u16,

    const Self = @This();

    fn init(alloc: Alloc) !Self {
        const root_pv = try alloc.alloc(Move, MAX_PLY);
        errdefer alloc.free(root_pv);
        const pv_moves = try alloc.alloc(Move, MAX_PLY * (MAX_PLY + 1) / 2);
        errdefer alloc.free(pv_moves);
        const pv_len = try alloc.alloc(u16, MAX_PLY);
        errdefer alloc.free(pv_len);

        return .{ .root_pv = root_pv, .pv_moves = pv_moves, .pv_len = pv_len };
    }

    fn deinit(self: *Self, alloc: Alloc) void {
        alloc.free(self.root_pv);
        alloc.free(self.pv_moves);
        alloc.free(self.pv_len);
    }

    fn perPlyRaw(self: *Self, ply: u32) []Move {
        const count = MAX_PLY - ply;
        const start = self.pv_moves.len - (count + 1) * count / 2;
        return self.pv_moves[start..start+count];
    }

    fn perPly(self: *Self, ply: u32) []Move {
        return self.perPlyRaw(ply)[0..self.pv_len[ply]];
    }

    fn clearRoot(self: *Self) void {
        @memset(self.root_pv, Move.NULL);
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

    fn updateRootPv(self: *Self) void {
        const pv = self.perPly(0);
        @memcpy(self.root_pv[0..pv.len], pv);
        @memset(self.root_pv[pv.len..], Move.NULL);
    }
};

pub const SearchThread = struct {
    alloc: Alloc,
    io: std.Io,
    brd: *Board = undefined,
    tt: tt.TTable,
    per_ply: *[MAX_PLY]PerPly,
    history: *hist.History,
    pv: PVMoves,
    // Stats
    nodes: u64 = 0,
    qsearch_nodes: u64 = 0,

    time_controls: TimeControls,
    logger: logs.SearchLogger,

    const PerPly = struct {
        killers: [2]Move,
        moves: board.MoveList,
    };

    const Self = @This();

    pub fn init(alloc: Alloc, io: std.Io) !Self {
        var pv = try PVMoves.init(alloc);
        errdefer pv.deinit(alloc);
        const history = try alloc.create(hist.History);
        errdefer alloc.destroy(history);
        const per_ply = try alloc.create([MAX_PLY]PerPly);
        errdefer alloc.destroy(per_ply);
        var ttable = try tt.TTable.init(alloc);
        errdefer ttable.deinit(alloc);
        var logger = try logs.SearchLogger.init(io, alloc);
        errdefer logger.deinit();

        return .{
            .alloc = alloc,
            .io = io,
            .tt = ttable,
            .pv = pv,
            .per_ply = per_ply,
            .history = history,
            .time_controls = .{ .io = io },
            .logger = logger,
        };
    }

    pub fn deinit(self: *Self) void {
        self.tt.deinit(self.alloc);
        self.pv.deinit(self.alloc);
        self.alloc.destroy(self.per_ply);
        self.alloc.destroy(self.history);
        self.logger.deinit();
    }

    fn evalPosition(self: *Self) i16 {
        return self.brd.data.extractEval();
    }

    fn preSearchCleanup(self: *Self) void {
        self.nodes = 0;
        self.qsearch_nodes = 0;
        self.tt.stats = .{};
    }

    fn scoreMove(self: *Self, move: Move, ply: u32, tt_move: Move) i16 {
        if (tt_move == move) {
            return MOVESCORE_TT;
        }

        const k = &self.per_ply[ply].killers;
        if (k[0] == move or k[1] == move) {
            const score: i16 = MOVESCORE_KILLER;
            return score;
        }

        if (move.is_promotion) {
            const piece = move.extra.promotion.toPiece(.white);
            var score = evaluation.PIECE_COST_ABS[@intFromEnum(piece)][0];
            const hscore = self.history.getNoisy(&self.brd.data, move).*;
            score += hscore;
            return score + MOVESCORE_PROMOTION;
        }

        if (move.is_capture) {
            var score = evaluation.mvv(self.brd.data, move);
            const hscore = self.history.getNoisy(&self.brd.data, move).*;
            const see = self.brd.seeAfterMove(move);
            score += hscore;

            if (see * 32 >= -score) {
                return MOVESCORE_GOOD_CAPTURE + score;
            }

            return MOVESCORE_BAD_CAPTURE + score;
        }

        const move_history = self.history.getQuiet(&self.brd.data, move).*;
        return @min(@max(move_history, MOVESCORE_BAD_CAPTURE), MOVESCORE_KILLER);
    }

    fn scoreMoveQsearch(self: *Self, move: Move, tt_move: Move) i16 {
        if (tt_move == move) {
            return MOVESCORE_TT;
        }

        var score: i16 = 0;

        if (move.is_promotion) {
            const piece = @intFromEnum(move.extra.promotion.toPiece(.white));
            score += evaluation.PIECE_COST_ABS[piece][0];
        }

        if (move.is_capture) {
            score += evaluation.mvv(self.brd.data, move);
        }

        score += self.history.getNoisy(&self.brd.data, move).*;

        return score;
    }

    fn scoreMoves(self: *Self, comptime is_qsearch: bool, moves: *board.MoveList, ply: u32, tt_move: Move) void {
        for (moves.moves(), moves.scores()) |*m, *s| {
            if (is_qsearch) {
                s.* = self.scoreMoveQsearch(m.*, tt_move);
            } else {
                s.* = self.scoreMove(m.*, ply, tt_move);
            }
        }
    }

    fn qsearch(self: *Self, root_alpha: i16, root_beta: i16, ply: u32) i16 {
        if (self.time_controls.isHardStop(self.nodes, self.qsearch_nodes)) {
            return root_alpha;
        }

        const in_check = self.brd.data.is_in_check;
        self.pv.clearPly(ply);

        self.qsearch_nodes += 1;

        const is_pv_node = root_alpha != root_beta - 1;
        if (self.brd.isDraw50Moves() or self.brd.repetitionsCount() > 0)
            return 0;

        var alpha = @max(root_alpha, @as(i16, @intCast(ply)) - SCORE_MATE_ABS);
        const beta = @min(root_beta, SCORE_MATE_ABS - @as(i16, @intCast(ply)) - 1);

        if (alpha >= beta) {
            return alpha;
        }

        var eval = self.evalPosition();

        const zobrist_key = self.brd.data.zobrist_key;
        var tt_move: Move = .NULL;

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
        
        // const pawn_index: u32 = if (self.brd.data.side_to_move == .white) 0 else 6;
        // const can_promote_a_queen = self.brd.data.pieces[pawn_index] & (board.rankMask(0) | board.rankMask(7)) != 0;
        // TODO more accurate delta pruning
        // var delta: i16 = 700;
        const delta = 1200;
        // if (can_promote_a_queen) delta += 700;
        if (eval < alpha -| delta) {
            return alpha;
        }

        if (eval > alpha) {
            alpha = eval;
        }

        const moves = &self.per_ply[ply].moves;
        moves.clear();
        self.brd.movegen.genMoves(true, &self.brd.data, moves);
    
        // moves.filterCapturesOnly();
        self.scoreMoves(true, moves, ply, tt_move);

        // technically can be a refutation move
        var best_move: Move = Move.NULL;
        var tt_bound: tt.Bound = .upper;
        var moves_played: u32 = 0;

        for (0..moves.count()) |_| {
            const move, const score = moves.pickNext();
            _ = score;
            if (!in_check and moves_played >= 4)
                break;

            if (!in_check and moves_played >= 2 and move.is_capture and self.brd.seeAfterMove(move) < 1)
                continue;

            if (self.brd.makeMove(move))
                continue;

            moves_played += 1;

            const move_eval = -self.qsearch(-beta, -alpha, ply+1);
            self.brd.unmakeMove();

            if (self.time_controls.isStopSet()) {
                return alpha;
            }

            if (move_eval >= beta) {
                tt_bound = .lower;
                best_move = move;
                self.pv.setMoveAtPly(move, ply);
                eval = move_eval;
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

        // No legal moves
        // TODO figure it out
        // TODO that doesn't work becasue we skipping quiet moves in qsearch
        // if (moves_played == 0) {
        //     if (in_check) {
        //         return @as(i16, @intCast(ply)) - SCORE_MATE_ABS;
        //     }
        //     return eval;
        // }
        
        self.tt.put(zobrist_key, ply, QS_TT_DEPTH, eval, tt_bound, best_move);

        return eval;
    }

    fn addKiller(self: *Self, ply: u32, move: Move) void {
        const k = &self.per_ply[ply].killers;
        if (k[0] != move and k[1] != move) {
            k[1] = k[0]; 
            k[0] = move; 
        }
    }


    fn search(
        self: *Self,
        root_alpha: i16,
        root_beta: i16,
        remaining_depth: u32,
        ply: u32,
        extensions_used: u32,
        allow_null: bool,
    ) i16 {
        if (ply > 0 and self.time_controls.isHardStop(self.nodes, self.qsearch_nodes)) {
            return root_alpha;
        }

        const in_check = self.brd.data.is_in_check;
        const is_pv_node = root_alpha != root_beta - 1;
        const zobrist_key = self.brd.data.zobrist_key;

        self.pv.clearPly(ply);
        self.nodes += 1;

        var alpha = root_alpha;
        var beta = root_beta;

        self.logger.enterNode(zobrist_key, alpha, beta, false) catch {};
        defer self.logger.exitNode(zobrist_key) catch {};

        if (ply > 0) {
            alpha = @max(alpha, @as(i16, @intCast(ply)) - SCORE_MATE_ABS);
            beta = @min(beta, SCORE_MATE_ABS - @as(i16, @intCast(ply)) - 1);
            if (alpha >= beta) {
                return alpha;
            }
        }

        if (ply > 0 and (self.brd.isDraw50Moves() or self.brd.repetitionsCount() > 0))
            return 0;

        if (remaining_depth == 0 or ply >= MAX_PLY) {
            return self.qsearch(alpha, beta, ply);
        }

        var tt_move: Move = .NULL;
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

        if (ply == 0 and self.pv.root_pv[0] != Move.NULL) {
            tt_move = self.pv.root_pv[0];
        }

        const static_eval = self.qsearch(alpha, beta, ply);
        if (self.time_controls.isStopSet()) {
            return alpha;
        }

        var eval: i16 = -SCORE_INFINITY;

        if (!is_pv_node and !in_check) {
            // RFP
            if (remaining_depth <= 8 and @abs(alpha) < 2000) {
                var margin = @as(i16, @intCast(80 * remaining_depth));
                // TODO double check this. it shouldn't gain but it gains for some reason.
                if (tt_move == Move.NULL) {
                    margin -|= 50;
                }
                if (tt_move.isNoisy()) {
                    margin -|= 50;
                }
                // margin -|= 100;
                margin = @max(margin, 40);

                if (static_eval >= beta + margin) {
                    return static_eval;
                }
                if (static_eval - margin > alpha) {
                    alpha = static_eval - margin;
                }
            }

            // NMP
            if (allow_null and @abs(beta) < SCORE_MATE_ABS - SCORE_MATE_EPS and remaining_depth > 1 and ply > 0) {
                const pieces = &self.brd.data.pieces;
                const is_pawns_only = (pieces[1] | pieces[2] | pieces[3] | pieces[4] | pieces[7] | pieces[8] | pieces[9] | pieces[10]) == 0;
                if (!is_pawns_only and !self.brd.makeNullMove()) {
                    // TODO eval-based reduction
                    const depth_reduction = @min((11 + remaining_depth * 5) / 8, 6);
                    const nm_score = -self.search(-beta, -beta + 1, remaining_depth -| depth_reduction, ply+1, extensions_used, false);
                    self.brd.unmakeNullMove();
                    if (self.time_controls.isStopSet()) {
                        return alpha;
                    }

                    if (nm_score >= beta) {
                        return nm_score;
                        // TODO more conditions on NMP / double -> enable verification
                        // const nm_score_verif = self.search(beta - 1, beta, remaining_depth - depth_reduction, ply, extensions_used, false);
                        // if (nm_score_verif >= beta) {
                        //     return nm_score_verif;
                        // }
                    }
                }
            }
        }

        const moves = &self.per_ply[ply].moves;
        moves.clear();
        self.brd.movegen.genMoves(false, &self.brd.data, moves);

        const moves_len = moves.count();

        if (moves_len == 0) {
            if (in_check) {
                return @as(i16, @intCast(ply)) - SCORE_MATE_ABS;
            }
            return 0;
        }

        self.scoreMoves(false, moves, ply, tt_move);

        // technically can be a refutation move
        var best_move: Move = Move.NULL;
        var tt_bound: tt.Bound = .upper;

        var legal_moves: u32 = 0;

        for (0..moves.count()) |_| {
            const move, const score = moves.pickNext();
            if (self.brd.makeMove(move))
                continue;
            legal_moves += 1;

            const new_in_check = self.brd.data.is_in_check;

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
 
            const search_depth = remaining_depth - 1 + search_ext;

            // TODO fix move ordering and implement actual LMR
            var reduction: u32 = 0;
            if (legal_moves > 2 and remaining_depth > 1 and score < MOVESCORE_KILLER) {
                reduction = @intFromFloat(7.0 + @log(@as(f32, @floatFromInt(remaining_depth))) * @log(@as(f32, @floatFromInt(legal_moves))) * 3.5);

                if (is_pv_node) {
                    reduction -|= 5;
                }

                if (!is_pv_node and tt_move.isNoisy()) {
                    reduction += 3;
                }

                if (in_check or new_in_check) {
                    reduction -|= 5;

                    if (in_check and new_in_check) {
                        reduction -|= 2;
                    }
                }

                reduction /= 8;
                reduction = @min(reduction, search_depth-|1);
            }

            // PVS
            var move_eval: i16 = -SCORE_INFINITY;
            if (remaining_depth < 2 or legal_moves < 3) {
                move_eval = -self.search(-beta, -alpha, search_depth, ply + 1, extensions_used + search_ext, false);
            } else {
                // TODO remove seach extensions from heare when moveordering is better
                move_eval = -self.search(-(alpha+1), -alpha, search_depth - reduction, ply + 1, extensions_used + search_ext, true);
                if (move_eval > alpha and (is_pv_node or reduction > 0)) {
                    move_eval = -self.search(-beta, -alpha, search_depth, ply + 1, extensions_used + search_ext, false);
                }
            }

            self.brd.unmakeMove();

            if (self.time_controls.isStopSet()) {
                return alpha;
            }

            if (move_eval >= beta) {
                if (!move.isNoisy()) {
                    self.addKiller(ply, move);
                }
                eval = move_eval;
                best_move = move;
                self.pv.setMoveAtPly(move, ply);

                tt_bound = .lower;

                const bonus: i16 = @intCast(remaining_depth * remaining_depth);
                const malus: i16 = bonus; // TODO
                if (!best_move.isNoisy()) {
                    self.history.updateQuiet(&self.brd.data, best_move, bonus);
                    for (moves.moves()) |quiet| {
                        if (quiet == best_move) break;
                        if (quiet.isNoisy()) continue;
                        self.history.updateQuiet(&self.brd.data, quiet, -malus);
                    }
                } else {
                    self.history.updateNoisy(&self.brd.data, best_move, bonus);
                }

                for (moves.moves()) |noisy| {
                    if (noisy == best_move) break;
                    if (!noisy.isNoisy()) continue;
                    self.history.updateNoisy(&self.brd.data, noisy, -malus);
                }
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

        std.debug.assert(ply != 0 or best_move != Move.NULL);
        std.debug.assert(self.pv.perPly(ply)[0] == best_move);

        self.tt.put(zobrist_key, ply, @intCast(remaining_depth), eval, tt_bound, best_move);

        return eval;
    }

    pub fn bestMove(self: *Self, brd: *Board, uci_connection: *uci.UciConnection) !void {
        self.brd = brd;
        
        self.history.reset();
        self.pv.clearRoot();
        self.time_controls.startSearch();
        self.preSearchCleanup();

        var best_move: Move = .NULL;

        const key = brd.data.zobrist_key;
        var fen_buf: [board.MAX_FEN_STRING_LENGTH]u8 = undefined;
        const fen = board.writeFen(&fen_buf, brd.data);

        var d: u32 = 0;
        while (true) {
            d += 1;

            self.logger.startNewSearch(fen, key) catch {};

            const score = self.search(-SCORE_INFINITY, SCORE_INFINITY, d, 0, 0, false);
            self.pv.updateRootPv();
            const search_iteration_end = std.Io.Timestamp.now(self.io, .awake);
            const search_time = self.time_controls.search_start.durationTo(search_iteration_end);

            self.logger.finishSearch() catch {};

            const pv_move = self.pv.perPlyRaw(0)[0];
            if (pv_move != Move.NULL) {
                best_move = pv_move;

                const info: uci.SearchInfo = .{
                    .depth = d,
                    .time_ms = @intCast(@as(u96, @intCast(search_time.nanoseconds)) / std.time.ns_per_ms),
                    .nodes = self.nodes,
                    .qs_nodes = self.qsearch_nodes,
                    .nps =  @as(u64, @intCast(@as(u96, self.nodes + self.qsearch_nodes) * std.time.ns_per_s / @max(search_time.nanoseconds, 1))),
                    .pv = self.pv.perPly(0),
                    .score = score,
                };

                try uci_connection.searchInfo(info); 
                // std.debug.print("tt: {any}\n", .{self.tt.stats});
                // std.debug.print("tt h: {d:.4}\n", .{
                //     @as(f64, @floatFromInt(self.tt.stats.hits))
                //     / @as(f64, @floatFromInt(self.tt.stats.misses))
                // });
                // std.debug.print("tt c: {d:.4}\n", .{
                //     @as(f64, @floatFromInt(self.tt.stats.collisions))
                //     / @as(f64, @floatFromInt(self.tt.stats.misses))
                // });
            }

            const is_exiting = self.time_controls.isSoftStop(d);
            if (is_exiting) {
                break;
            }
        }

        std.debug.assert(best_move != Move.NULL);
        try uci_connection.bestmove(best_move);
    }

    pub fn searchToMate(self: *Self, brd: *Board) !u32 {
        self.brd = brd;

        self.time_controls.startSearch();
        self.history.reset();
        self.time_controls.startSearch();
        self.preSearchCleanup();

        var depth_limit: u32 = MAX_PLY;
        var found_at: u16 = MAX_PLY;

        var d: u32 = 0;
        while (d < depth_limit) {
            d += 1;

            const score = self.search(-SCORE_INFINITY, SCORE_INFINITY, d, 0, 0, false);
            const is_canceled = self.time_controls.isStopSet();
            
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
    alloc: Alloc,
    io: std.Io,
    // TODO for adding real multithreading turn that into array
    // and do something smart about synchronization
    search_thread_thread: std.Thread,
    search_thread: SearchThread,
    movegen: *board.Movegen,
    // TODO move that to SearchThread
    brd: Board,
    uci_connection: *uci.UciConnection, 

    is_searching: bool,
    is_exiting: bool,

    mutex: std.Io.Mutex,
    cond: std.Io.Condition,
    search_end_cond: std.Io.Condition,

    waiting: std.atomic.Value(u32),

    const Self = @This();

    pub fn init(alloc: Alloc, io: std.Io, uci_connection: *uci.UciConnection) !*Self {
        const self = try alloc.create(Self);
        errdefer alloc.destroy(self);

        self.io = io;
        self.alloc = alloc;
        self.movegen = try board.Movegen.init(alloc);
        errdefer self.movegen.deinit(alloc);
        errdefer alloc.destroy(self.movegen);
        self.mutex = .init;
        self.brd = try .init(alloc, self.movegen);
        errdefer self.brd.deinit();
        self.is_searching = false;
        self.is_exiting = false;
        self.cond = .init;
        self.search_end_cond = .init;
        self.search_thread = try SearchThread.init(alloc, io);
        self.waiting = .init(0);
        self.uci_connection = uci_connection;
        self.search_thread_thread = try std.Thread.spawn(.{ .allocator = alloc }, searchThreadEntry, .{ self, &self.search_thread });

        return self;
    }

    pub fn deinit(self: *Self) !void {
        try self.mutex.lock(self.io);
        self.is_exiting = true;
        self.mutex.unlock(self.io);
        self.cond.broadcast(self.io);
        self.search_thread_thread.join();
        self.search_thread.deinit();
        self.brd.deinit();
        self.movegen.deinit(self.alloc);
        self.alloc.destroy(self.movegen);
    }

    pub fn startSearch(self: *Self, time_controls: TimeControls) !void {
        {
            try self.mutex.lock(self.io);
            defer self.mutex.unlock(self.io);

            // TODO copy board into each thread
            self.search_thread.time_controls = time_controls;
        }
        self.cond.broadcast(self.io);
    }

    pub fn signalStopSearch(self: *Self) !void {
        self.search_thread.time_controls.is_exiting_search.store(true, .unordered);
    }
    
    pub fn waitUntilSearchEnded(self: *Self) !void {
        try self.mutex.lock(self.io);
        defer self.mutex.unlock(self.io);
        
        while (self.is_searching) {
            try self.search_end_cond.wait(self.io, &self.mutex);
        }
    }

    fn searchThreadEntry(self: *Self, search_thread: *SearchThread) !void {
        try self.mutex.lock(self.io);
        defer self.mutex.unlock(self.io);

        while (true) {
            _ = self.waiting.fetchAdd(1, .release);
            try self.cond.wait(self.io, &self.mutex);
            _ = self.waiting.fetchSub(1, .release);

            if (self.is_exiting) {
                break;
            }
            self.is_searching = true;
            
            {
                self.mutex.unlock(self.io);

                // TODO run figure out propper way to run that in multiple threads
                try search_thread.bestMove(&self.brd, self.uci_connection);
                try self.mutex.lock(self.io);
            }

            // TODO this would work only for single thread...
            self.is_searching = false;
            self.search_end_cond.broadcast(self.io);
        }
    }
};
