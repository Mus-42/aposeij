const std = @import("std");
const board = @import("board");
const search = @import("search.zig");
const evaluation = @import("evaluation.zig");
const uci = @import("uci.zig");
const options = @import("options");

const Alloc = std.mem.Allocator;

var stdin_buf: [uci.MAX_COMMAND_LEN]u8 = undefined;
var stdout_buf: [256]u8 = undefined;

pub fn main() !void {
    // TODO use real allocator here
    var debug_alloc = std.heap.DebugAllocator(.{}).init;
    defer std.debug.assert(debug_alloc.deinit() == .ok);
    const alloc = debug_alloc.allocator();

    var stdin = std.fs.File.stdin().reader(&stdin_buf);
    var stdout = std.fs.File.stdout().writer(&stdout_buf);

    var uci_connection = uci.UciConnection.init(&stdin.interface, &stdout.interface);
    defer uci_connection.deinit(); 

    var brd = try board.Board.init(alloc, .DEFAULT);
    defer brd.deinit();

    var control = try search.SearchControl.init(alloc, &uci_connection);
    defer { 
        control.deinit();
        alloc.destroy(control);
    }

    while (true) {
        const command = uci_connection.readCommand() catch |err| {
            if (err == error.EndOfStream) {
                break;
            }
            return err;
        };

        switch (command.command) {
            .uci => try uci_connection.uciok(),
            .isready => {
                try control.waitUntilSearchEnded();
                try uci_connection.readyok();
            },
            .stop => try control.signalStopSearch(),
            .quit => break,
            .position => try uci_connection.parsePositionArgs(command.arguments, &brd),
            .go => {
                const time_controls = try uci_connection.parseGoArgs(command.arguments, brd.data.side_to_move);
                try control.waitUntilSearchEnded();
                try control.startSearch(&brd, time_controls);
            },
            .ucinewgame => {
                try control.signalStopSearch();
                try control.waitUntilSearchEnded();
            },
            .displaypos => {
                brd.data.debugPrint();
                var buf: [board.MAX_FEN_STRING_LENGTH]u8 = undefined;
                std.debug.print("{s}\n", .{board.writeFen(&buf, brd.data)});
            },
            .perft, .perft_nonbulk => {
                const is_bulk = command.command == .perft;
                const depth = std.fmt.parseInt(u32, command.arguments, 10) catch {
                    std.debug.print("usage: perft [DEPTH]", .{});
                    continue;
                };
                uci_connection.lockStdout();
                defer uci_connection.unlockStdout();
                const beg = try std.time.Instant.now();
                const nodes = if (is_bulk) 
                    try perft_root(true, &brd, depth, uci_connection.stdout)
                else
                    try perft_root(false, &brd, depth, uci_connection.stdout);
                const end = try std.time.Instant.now();
                const duration = @as(f64, @floatFromInt(end.since(beg))) * (1 / @as(f64, std.time.ns_per_s));
                const nps = @as(f64, @floatFromInt(nodes)) / duration;
                try uci_connection.stdout.print("nodes: {}\n", .{nodes});
                try uci_connection.stdout.print("nps: {d}\n", .{nps});
                try uci_connection.stdout.flush();
            },
            .bench => {
                try bench(alloc, uci_connection.stdout);
            },
            else => {},
        }
    }
}

// TODO more fens for bench
// TODO sane way to choose those (for now just random)
const BENCH_FENS: []const []const u8 = &.{
    "r1bqk1nr/pp1n1pbp/2p1p1p1/8/4N3/3P1N2/PPP1BPPP/1RBQK2R w Kkq - 2 8",
    "r3r1k1/pp1qnp1p/3bb3/4pp2/1P2P3/PB2BN1P/5PP1/R2Q1RK1 w - - 0 20",
    "r1bq1rk1/ppp2ppp/2nb1n2/3Pp3/2B1P3/P1N2N2/1P3PPP/R1BQK2R b KQ - 0 10",
    "r2q1rk1/1pp1bpp1/p3p2p/3pP3/3PB3/P5PP/1PP1P3/R1BQ1RK1 b - - 0 13",
    "6k1/8/8/3n4/5R2/8/8/6K1 w - - 25 95",
    "4r3/1p2rk2/1n2p3/1P2R3/2p3P1/5N2/5PP1/4R1K1 b - - 0 47",
    "r2qr1k1/1ppb1p2/4p2p/1Pn1P1p1/2B5/P1Q2N1P/5PP1/3RR1K1 w - - 3 26",
    "r1bqkb1r/ppp1pppp/2np4/3nP3/2PP4/5N2/PP3PPP/RNBQKB1R b KQkq - 0 5",
};

fn writerNoopDrain(_: *std.io.Writer, data: []const []const u8, splat: usize) std.io.Writer.Error!usize {
    if (data.len == 0) return 0;
    var count: usize = data[data.len-1].len * splat;
    for (data[0..data.len-1]) |buf| count += buf.len;
    return count;
}

fn bench(alloc: Alloc, output: *std.Io.Writer) !void {
    var search_thread = try search.SearchThread.init(alloc);
    defer search_thread.deinit();

    var brd = try board.Board.init(alloc, .DEFAULT);
    defer brd.deinit();

    var dummy_reader: std.io.Reader = .failing;
    var dummy_writer: std.io.Writer =  .{
        .vtable = &.{
            .drain = writerNoopDrain,
            .flush = std.io.Writer.noopFlush,
        },
        .buffer = &.{},
    };
    var dummy_connection: uci.UciConnection = .init(&dummy_reader, &dummy_writer);
    defer dummy_connection.deinit();

    const search_time_controls = search.TimeControls { .to_depth = .{ .target = 8 } };

    var nodes: u64 = 0;
    const bench_beg = std.time.Instant.now() catch unreachable;
    for (BENCH_FENS) |fen| {
        const bd = board.readFen(fen) catch unreachable;
        brd.setBoardData(bd);

        search_thread.is_exiting_search.store(false, .release);
        try search_thread.bestMove(&brd, &dummy_connection, search_time_controls);
        nodes += search_thread.nodes;
    }
    const bench_end = std.time.Instant.now() catch unreachable;
    const bench_time = @as(f64, @floatFromInt(bench_end.since(bench_beg))) / std.time.ns_per_s;
    try output.print("{} nodes in {}s\n{} nps\n", .{nodes, bench_time, @as(f64, @floatFromInt(nodes)) / bench_time});
    try output.flush();
}

fn perft_root(comptime is_bulk: bool, brd: *board.Board, remaining_depth: u32, output: *std.Io.Writer) !u64 {
    if (remaining_depth == 0) {
        return 1;
    }

    var moves = board.Moves{};

    board.genMoves(brd.data, &moves);
    const impl = struct {
        fn lessThan(_: void, a: board.Move, b: board.Move) bool {
            return std.mem.lessThan(u8, &a.algebraicNotation().buf, &b.algebraicNotation().buf);
        }
    };
    
    std.mem.sortUnstable(board.Move, moves.p[0..moves.i], {}, impl.lessThan);

    var nodes_count = @as(u64, 0);
    for (moves.moves()) |move| {
        brd.makeMove(move);
        const per_move_count = perft(is_bulk, brd, remaining_depth - 1);
        try output.print("{s} - {d}\n", .{move.algebraicNotation().toStr(), per_move_count});
        nodes_count += per_move_count;
        brd.unmakeMove();
    }

    return nodes_count;
}

fn perft(comptime is_bulk: bool, brd: *board.Board, remaining_depth: u32) u64 {
    if (remaining_depth == 0) {
        return 1;
    }

    var moves = board.Moves{};
    board.genMoves(brd.data, &moves);

    if (is_bulk and remaining_depth == 1) {
        return moves.i;
    }
    

    var nodes_count = @as(u64, 0);
    for (moves.moves()) |move| {
        brd.makeMove(move);
        nodes_count += perft(is_bulk, brd, remaining_depth - 1);
        brd.unmakeMove();
    }

    return nodes_count;
}
