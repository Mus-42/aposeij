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

const BENCH_FENS =
    \\ rn1qkbnr/ppp2ppp/4p3/3pPb2/3P1P2/8/PPP3PP/RNBQKBNR b KQkq - 0 4
    \\ rn1qkb1r/ppp2ppp/4pn2/3p3b/2PP4/2N2N1P/PP2PPP1/R1BQKB1R w KQkq - 1 6
    \\ rnb1kbnr/pppp1ppp/5q2/8/3Q4/2N5/PPP1PPPP/R1B1KBNR w KQkq - 1 4
    \\ rn1qkbnr/pp2pppp/3p4/2p5/3PP1b1/2P2N2/PP3PPP/RNBQKB1R b KQkq - 0 4
    \\ rnbqkb1r/ppp2ppp/5n2/3ppP2/4P3/3P4/PPP3PP/RNBQKBNR w KQkq - 0 5
    \\ rnbqkbnr/ppp3pp/4pp2/3p4/3P1B2/3BP3/PPP2PPP/RN1QK1NR b KQkq - 1 4
    \\ rnbqkb1r/ppp3pp/4pn2/3p1p2/2PP1B2/4PN2/PP3PPP/RN1QKB1R b KQkq - 0 5
    \\ rnbqkb1r/pppp1ppp/4pn2/8/8/3B1N2/PPPP1PPP/RNBQ1RK1 w kq - 0 5
    \\ r1bqkb1r/pppp1ppp/2n5/4P3/6n1/4PN2/PPP2PPP/RNBQKB1R w KQkq - 1 5
    \\ r1bqk1nr/ppp1bppp/3p4/8/3QP3/2N1B3/PPP2PPP/R3KB1R b KQkq - 3 7
    \\ r1bqkbnr/ppp3pp/4p3/3pP3/8/8/PPP2PPP/RNBQKB1R w KQkq - 0 7
    \\ r1bqkb1r/ppp1pppp/2n2n2/3p4/Q2P4/2P1P3/PP3PPP/RNB1KBNR b KQkq - 0 4
    \\ rnbqkbnr/ppp2ppp/8/3pP3/8/2P5/PP2PPPP/RNBQKBNR b KQkq - 0 3
    \\ r1b1k2r/ppp2ppp/2n5/3qp3/1b2N3/3P1N2/PPPB1PPP/R2QK2R b KQkq - 2 8
    \\ rn2kb1r/pp3ppp/1qp1pn2/3p1b2/2PP1B2/1QN1P3/PP3PPP/R3KBNR w KQkq - 2 7
    \\ r1b1kb1r/ppppq1pp/2n2n2/8/5P2/5N2/PPP1P1PP/RNBQKB1R w KQkq - 0 6
    \\ rn1qkbnr/pp2pppp/8/2pp1b2/8/5NP1/PPPPPPBP/RNBQK2R w KQkq - 2 4
    \\ rn1qkb1r/ppp1pppp/8/3p4/3Pn1b1/4PN2/PPPN1PPP/R1BQKB1R w KQkq - 3 5
    \\ rnbqk2r/ppp2ppp/3b1n2/3Pp3/2B5/2NP4/PPP2PPP/R1BQK1NR b KQkq - 0 5
    \\ rnbqkbnr/pppp1p1p/4p1p1/8/4P3/2N2N2/PPPP1PPP/R1BQKB1R b KQkq - 1 3
    \\ rnbqk1nr/p1pp1ppp/1p2p3/8/1b1PP3/2N5/PPP2PPP/R1BQKBNR w KQkq - 1 4
    \\ rn1qkbnr/ppp1pppp/8/5b2/2B5/2N2Q2/PPPP1PPP/R1B1K1NR b KQkq - 5 5
    \\ rn1qk1nr/pp1bbppp/8/1Bpp4/3P4/5N2/PPP1QPPP/RNB1K2R w KQkq - 4 7
    \\ rnbqkbnr/ppp2ppp/8/3pp3/8/3PB3/PPP1PPPP/RN1QKBNR w KQkq - 0 3
    \\ rn1qkb1r/pb1p1ppp/1p2pn2/2p5/3PP3/2PB1N2/PP2QPPP/RNB1K2R b KQkq - 0 6
    \\ r1bqk2r/p4ppp/2p2n2/n1b1p1N1/8/8/PPPPBPPP/RNBQK2R w KQkq - 2 9
    \\ r1bqk2r/pp1pnpbp/2n1p1p1/8/3NP3/2N1BP2/PPP3PP/R2QKB1R w KQkq - 1 8
    \\ rnbqkbnr/pp2pppp/2p5/3p4/8/4PPP1/PPPP3P/RNBQKBNR b KQkq - 0 3
    \\ rnbqkbnr/pp2p1pp/2pp1p2/8/3P4/3BPN2/PPP2PPP/RNBQK2R b KQkq - 1 4
    \\ r1bqkbnr/ppp1pp1p/2np2p1/8/4PP2/5N2/PPPP2PP/RNBQKB1R w KQkq - 2 4
    \\ rnbqkbnr/ppp3pp/3p1p2/4p3/4P3/3PB3/PPP2PPP/RN1QKBNR w KQkq - 0 4
    \\ rnb1kbnr/ppp3pp/5p2/4p3/1q2P3/2N1BN2/PPP2PPP/R2QKB1R b KQkq - 3 7
    \\
    ;


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

    // var top_nodes: [32]u64 = @splat(0);
    // var top_fens: [32][]const u8 = @splat("");

    var iter = std.mem.splitAny(u8, BENCH_FENS, "\n");
    while (iter.next()) |fen_raw| {
        const fen = std.mem.trim(u8, fen_raw, &std.ascii.whitespace);
        if (fen.len == 0) continue;

        const bd = board.readFen(fen) catch unreachable;
        brd.setBoardData(bd);

        search_thread.time_controls = search_time_controls;
        try search_thread.bestMove(brd, &dummy_connection);

        // for (0..32) |i| {
        //     if (top_nodes[i] < search_thread.nodes) {
        //         top_nodes[i] = search_thread.nodes;
        //         top_fens[i] = fen;
        //         break;
        //     }
        // }

        nodes += search_thread.nodes;
    }

    // for (top_fens, top_nodes) |fen, fen_nodes| {
    //     std.debug.print("{}: {s}\n", .{fen_nodes, fen});
    // }

    const duration = @as(f64, @floatFromInt(beg.untilNow(io, .awake).nanoseconds)) * (1 / @as(f64, std.time.ns_per_s));
    try output.print("{} nodes in {:.2}s {:.0} nps\n", .{nodes, duration, @as(f64, @floatFromInt(nodes)) / duration});
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
