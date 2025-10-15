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

const BENCH_FENS: []const []const u8 = &.{
    "r1bqk1nr/pp1n1pbp/2p1p1p1/8/4N3/3P1N2/PPP1BPPP/1RBQK2R w Kkq - 2 8",
    // TODO more
};

fn bench(alloc: Alloc, output: *std.Io.Writer) !void {
    var search_thread = try search.SearchThread.init(alloc);
    defer search_thread.deinit();

    var brd = try board.Board.init(alloc, .DEFAULT);
    defer brd.deinit();

    for (BENCH_FENS) |fen| {
        const bd = board.readFen(fen) catch unreachable;
        brd.setBoardData(bd);

        // TODO do the search, count nodes
        //search_thread.bestMove(&brd, uci_connection: *UciConnection, time_controls: TimeControls)
    }

    // TODO print results
    _ = output;
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
