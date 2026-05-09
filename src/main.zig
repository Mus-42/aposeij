const std = @import("std");
const board = @import("board");
const search = @import("search.zig");
const evaluation = @import("evaluation.zig");
const uci = @import("uci.zig");
const options = @import("options");

const Alloc = std.mem.Allocator;

var stdin_buf: [uci.MAX_COMMAND_LEN]u8 = undefined;
var stdout_buf: [256]u8 = undefined;

pub fn main(init: std.process.Init) !void {
    const alloc = init.gpa;
    const io = init.io;

    var stdin = std.Io.File.stdin().reader(io, &stdin_buf);
    var stdout = std.Io.File.stdout().writer(io, &stdout_buf);


    // TODO cli option to override
    const strict_mode = std.Io.File.stdin().isTty(io) catch unreachable;

    var uci_connection = uci.UciConnection.init(io, &stdin.interface, &stdout.interface, .{ .strict_mode = strict_mode });
    defer uci_connection.deinit(); 

    var control = try search.SearchControl.init(alloc, io, &uci_connection);
    defer { 
        control.deinit() catch {};
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
            .setoption => {},
            .isready => {
                try control.waitUntilSearchEnded();
                try uci_connection.readyok();
            },
            .stop => try control.signalStopSearch(),
            .quit => break,
            .position => try uci_connection.parsePositionArgs(command.arguments, &control.brd),
            .go => {
                const time_controls = try uci_connection.parseGoArgs(command.arguments, control.brd.data.side_to_move);
                try control.waitUntilSearchEnded();
                try control.startSearch(time_controls);
            },
            .ponderhit => {},
            .ucinewgame => {
                try control.signalStopSearch();
                try control.waitUntilSearchEnded();
            },
            .displaypos => {
                try control.brd.data.debugPrint(uci_connection.stdout);
                var buf: [board.MAX_FEN_STRING_LENGTH]u8 = undefined;
                try uci_connection.stdout.print("{s}\n", .{board.writeFen(&buf, control.brd.data)});
                try uci_connection.stdout.print("key: {x}\n", .{control.brd.data.zobrist_key});
                try uci_connection.stdout.flush();
            },
            .help => {
                try uci_connection.stdout.writeAll(uci.HELP);
                try uci_connection.stdout.flush();
            },
            .perft => {
                const depth = std.fmt.parseInt(u32, command.arguments, 10) catch {
                    try uci_connection.stdout.print("usage: perft [DEPTH]\n", .{});
                    try uci_connection.stdout.flush();
                    continue;
                };
                try uci_connection.lockStdout();
                defer uci_connection.unlockStdout();
                const beg = std.Io.Timestamp.now(io, .awake);
                const nodes = try perft_root(&control.brd, depth, uci_connection.stdout);
                const duration = @as(f64, @floatFromInt(beg.untilNow(io, .awake).nanoseconds)) * (1 / @as(f64, std.time.ns_per_s));
                const nps = @as(f64, @floatFromInt(nodes)) / duration;
                try uci_connection.stdout.print("nodes: {}\n", .{nodes});
                try uci_connection.stdout.print("nps: {d}\n", .{nps});
                try uci_connection.stdout.flush();
            },
            .run_testsuite => {
                var filename = std.mem.trim(u8, command.arguments, &std.ascii.whitespace);
                if (filename.len == 0) {
                    filename = "testfiles/perft.testsuite";
                }
                const bd = control.brd.data;
                try run_testsuite(alloc, io, &control.brd, filename, uci_connection.stdout);
                control.brd.clearHistory();
                control.brd.setBoardData(bd);
            },
            .bench => {
                const bd = control.brd.data;
                try bench(alloc, io, &control.brd, uci_connection.stdout);
                control.brd.clearHistory();
                control.brd.setBoardData(bd);
            },
            .unknown => {
                if (!uci_connection.options.strict_mode) continue;
                try uci_connection.stdout.print("Unknown command `{s}`\n", .{command.command_full_str});
                try uci_connection.stdout.flush();
            },
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

fn writerNoopDrain(_: *std.Io.Writer, data: []const []const u8, splat: usize) std.Io.Writer.Error!usize {
    if (data.len == 0) return 0;
    var count: usize = data[data.len-1].len * splat;
    for (data[0..data.len-1]) |buf| count += buf.len;
    return count;
}

fn bench(alloc: Alloc, io: std.Io, brd: *board.Board, output: *std.Io.Writer) !void {
    var search_thread = try search.SearchThread.init(alloc, io);
    defer search_thread.deinit();

    var dummy_reader: std.Io.Reader = .failing;
    var dummy_writer: std.Io.Writer =  .{
        .vtable = &.{
            .drain = writerNoopDrain,
            .flush = std.Io.Writer.noopFlush,
        },
        .buffer = &.{},
    };
    var dummy_connection: uci.UciConnection = .init(io, &dummy_reader, &dummy_writer, .{});
    defer dummy_connection.deinit();

    const search_time_controls: search.TimeControls = .toDepth(io, 8);

    var nodes: u64 = 0;

    const beg = std.Io.Timestamp.now(io, .awake);
    for (BENCH_FENS) |fen| {
        const bd = board.readFen(fen) catch unreachable;
        brd.setBoardData(bd);
    
        search_thread.time_controls = search_time_controls;
        try search_thread.bestMove(brd, &dummy_connection);
        nodes += search_thread.nodes;
    }
    const duration = @as(f64, @floatFromInt(beg.untilNow(io, .awake).nanoseconds)) * (1 / @as(f64, std.time.ns_per_s));
    try output.print("{} nodes in {}s\n{} nps\n", .{nodes, duration, @as(f64, @floatFromInt(nodes)) / duration});
    try output.flush();
}

fn perft_root(brd: *board.Board, remaining_depth: u32, output: *std.Io.Writer) !u64 {
    if (remaining_depth == 0) {
        return 1;
    }

    var moves = board.MoveList{};
    brd.movegen.genMoves(&brd.data, &moves);
    const impl = struct {
        fn lessThan(_: void, a: board.Move, b: board.Move) bool {
            return std.mem.lessThan(u8, &a.algebraicNotation().buf, &b.algebraicNotation().buf);
        }
    };
    
    std.mem.sortUnstable(board.Move, moves.p[0..moves.i], {}, impl.lessThan);

    var nodes_count = @as(u64, 0);
    for (moves.moves()) |move| {
        if (brd.makeMove(move))
            continue;
        const per_move_count = perft(brd, remaining_depth - 1);
        try output.print("{s} - {d}\n", .{move.algebraicNotation().toStr(), per_move_count});
        nodes_count += per_move_count;
        brd.unmakeMove();
    }

    return nodes_count;
}

fn perft(brd: *board.Board, remaining_depth: u32) u64 {
    if (remaining_depth == 0) {
        return 1;
    }

    var moves = board.MoveList{};
    brd.movegen.genMoves(&brd.data, &moves);

    var nodes_count = @as(u64, 0);
    for (moves.moves()) |move| {
        if (brd.makeMove(move))
            continue;
        nodes_count += perft(brd, remaining_depth - 1);
        brd.unmakeMove();
    }

    return nodes_count;
}

fn run_testsuite(alloc: Alloc, io: std.Io, brd: *board.Board, filename: []const u8, output: *std.Io.Writer) !void {
    var file = std.Io.Dir.cwd().openFile(io, filename, .{}) catch |err| {
        try output.print("failed to open testsuite file {s}: {}\n", .{filename, err});
        try output.flush();
        return;
    };
    defer file.close(io);

    try output.print("Running testsuite {s}\n", .{filename});
    try output.flush();

    const READER_BUF_SIZE = 1024;
    var reader_buf: [READER_BUF_SIZE]u8 = undefined;
    var fen_buf: [board.MAX_FEN_STRING_LENGTH]u8 = undefined;
    var file_reader = file.reader(io, &reader_buf);
    const reader = &file_reader.interface;

    var total: u32 = 0;
    var passed: u32 = 0;
    var skipped: u32 = 0;

    var bd = board.Board.BoardData.DEFAULT;

    var search_thread = try search.SearchThread.init(alloc, io);
    defer search_thread.deinit();

    const movegen = try board.Movegen.init(alloc);
    defer movegen.deinit(alloc);
    defer alloc.destroy(movegen);

    while (true) {
        try reader.rebase(READER_BUF_SIZE);
        const command_raw = reader.takeDelimiterInclusive('\n') catch |err| {
            if (err == error.EndOfStream) break;
            return err;
        };
        const command = std.mem.trim(u8, command_raw, &std.ascii.whitespace);
        var command_parts = std.mem.splitAny(u8, command, &std.ascii.whitespace);
        const name = command_parts.next() orelse continue;

        if (std.mem.eql(u8, name, "fen")) {
            const fen = std.mem.trim(u8, command_parts.rest(), &std.ascii.whitespace);
            bd = board.readFen(fen) catch |err| {
                try output.print("failed to parse fen string `{s}`: `{}`\n", .{fen, err});
                try output.flush();
                skipped += 1;
                continue;
            };
        } else if (std.mem.eql(u8, name, "test_mate")) {
            const fen = board.writeFen(&fen_buf, bd);
            const expected_moves_str = std.mem.trim(u8, command_parts.rest(), &std.ascii.whitespace);
            const expected_moves = try std.fmt.parseInt(u32, expected_moves_str, 10);

            brd.setBoardData(bd);

            const TIME_LIMIT = std.time.ns_per_s * 20;
            search_thread.time_controls = .toTime(io, TIME_LIMIT);
            const mate_in = search_thread.searchToMate(brd) catch |err| {
                switch (err) {
                    error.TimeExpired => {
                        try output.print("failed by time mate in {} for fen {s}\n", .{expected_moves, fen});
                        try output.flush();
                        total += 1;
                        continue;
                    }
                }
            };


            if (expected_moves == mate_in) {
                try output.print("passed mate in {} for fen {s}\n", .{expected_moves, fen});
                try output.flush();
                passed += 1;
            } else {
                try output.print("failed mate in {} for fen {s}. expected mate in {}, found in {}\n", .{expected_moves, fen, expected_moves, mate_in});
                try output.flush();
            }
            total += 1;
        } else if (std.mem.eql(u8, name, "test_perft")) {
            const fen = board.writeFen(&fen_buf, bd);
            var is_passed = true;
            brd.setBoardData(bd);
            while (command_parts.next()) |perft_test| {
                var test_parts = std.mem.splitScalar(u8, perft_test, ':');
                const depth_str = test_parts.next() orelse continue;
                const expected_count_str = test_parts.next() orelse continue;

                const depth = try std.fmt.parseInt(u32, depth_str, 10);
                const expected_count = try std.fmt.parseInt(u32, expected_count_str, 10);

                const count = perft(brd, depth);
                
                if (count != expected_count) {
                    is_passed = false;
                    try output.print("perft for fen {s} failed at depth {d}. expected count {d} but computed {d}\n", .{fen, depth, expected_count, count});
                    try output.flush();
                    break;
                }
            }
            if (is_passed) {
                try output.print("passed perft fen {s}\n", .{fen});
                try output.flush();
                passed += 1;
            }
            total += 1;
        } else {
            try output.print("unknown testsuite command `{s}`\n", .{name});
            try output.flush();
            skipped += 1;
            continue;
        }
    }
    try output.print("passed {d} of {d} ({d:.1}%), {d} tests skipped\n", .{passed, total, @as(f64, @floatFromInt(passed)) / @as(f64, @floatFromInt(total)) * 100, skipped});
    try output.flush();
}
