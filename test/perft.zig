const std = @import("std");
const board = @import("board");

pub fn perft(brd: *board.Board, remaining_depth: u32) u64 {
    if (remaining_depth == 0) {
        return 1;
    }

    var moves = board.Moves{};
    board.genMoves(brd.data, &moves, false);

    if (remaining_depth == 1) {
        return moves.i;
    }

    var pos_count = @as(u64, 0);
    for (moves.moves()) |move| {
        brd.makeMove(move);
        pos_count += perft(brd, remaining_depth - 1);
        brd.unmakeMove();
    }

    return pos_count;
}

pub fn main() !void {
    var debug_alloc = std.heap.DebugAllocator(.{}).init;
    defer std.debug.assert(debug_alloc.deinit() == .ok);
    const alloc = debug_alloc.allocator();
    
    // TODO get filename from args & read it instead of embeding
    const data = @embedFile("ethereal_perft_data");
    var fbs = std.io.fixedBufferStream(data);
    const reader = fbs.reader();

    var fen_buf: [board.MAX_FEN_STRING_LENGTH]u8 = undefined;
    var scores_buf: [64]u8 = undefined;

    var total: u32 = 0;
    var passed: u32 = 0;

    while (true) {
        const fen = try reader.readUntilDelimiterOrEof(&fen_buf, '\n') orelse break;
        const scores = try reader.readUntilDelimiterOrEof(&scores_buf, '\n') orelse break;
        var scores_iter = std.mem.splitScalar(u8, scores, ' ');
        var is_passed = true;
        for (1..6+1) |d| {
            const score_raw = scores_iter.next().?;
            if (std.mem.eql(u8, "-", score_raw)) continue;

            const expected_score = try std.fmt.parseInt(u32, score_raw, 10);

            const bd = try board.readFen(fen);

            var b = try board.Board.init(alloc, bd);
            defer b.deinit();

            const score = perft(&b, @intCast(d));
            
            if (score != expected_score) {
                is_passed = false;
                std.debug.print("fen {s} failed at depth {d}. expected score {d} but computed {d}\n", .{fen, d, expected_score, score});
            }
        }
        if (is_passed) {
            passed += 1;
        }
        total += 1;
    }
    std.debug.print("passed {d} of {d} ({d:.1}%)\n", .{passed, total, @as(f64, @floatFromInt(passed)) / @as(f64, @floatFromInt(total)) * 100});
}
