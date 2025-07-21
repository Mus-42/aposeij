const std = @import("std");
const board = @import("board");
const Alloc = std.mem.Allocator;
const Writer = std.io.AnyWriter;


test {
    _ = board;
}

var writer: Writer = undefined;

pub fn perft(brd: *board.Board, remaining_depth: u32) u64 {
    if (remaining_depth == 0) {
        return 1;
    }

    var moves = board.Moves{};
    board.genMoves(brd.data, &moves);

    // comment this out for non-bulk perft
    // if (remaining_depth == 1) {
    //     return moves.i;
    // }

    var pos_count = @as(u64, 0);
    for (moves.moves()) |move| {
        brd.makeMove(move);
        pos_count += perft(brd, remaining_depth - 1);
        brd.unmakeMove();
    }

    return pos_count;
}

pub fn perft_root(brd: *board.Board, remaining_depth: u32) u64 {
    std.debug.assert(remaining_depth > 0);

    var moves = board.Moves{};
    if (@popCount(brd.data.pieces[@intFromEnum(board.PieceKind.w_king)]) != 1) {
        brd.debugDumpMoveHistory();
    }

    board.genMoves(brd.data, &moves);
    const impl = struct {
        fn lessThan(_: void, a: board.Move, b: board.Move) bool {
            return std.mem.lessThan(u8, &a.algebraicNotation().buf, &b.algebraicNotation().buf);
        }
    };
    std.mem.sortUnstable(board.Move, moves.p[0..moves.i], {}, impl.lessThan);

    var pos_count = @as(u64, 0);
    for (moves.moves()) |move| {
        brd.makeMove(move);
        const move_count = perft(brd, remaining_depth - 1);
        writer.print("{s} - {d}\n", .{move.algebraicNotation().toStr(), move_count}) catch unreachable;
        pos_count += move_count;
        brd.unmakeMove();
    }

    return pos_count;
}

var fen_buf: [board.MAX_FEN_STRING_LENGTH]u8 = undefined;

const Args = struct {
    fen: []const u8,
    depth: u32 = 1,
    search_all_depths: bool = false,
    display_root_moves: bool = false,
    display_board: bool = false,
    silent: bool = false,
};

fn printUsage() void {
    std.debug.print("{s}\n", .{
        \\perft [options] [fen string]
        \\
        \\options:
        \\ -d [depth]       search depth
        \\ -a               search all depth 1..=depth
        \\ -m               display root moves
        \\ -b               display board
        \\ -s               silent mode (hide everything except node count)
    });
}

fn readArgs(alloc: Alloc) !Args {
    const args = try std.process.argsAlloc(alloc);
    defer std.process.argsFree(alloc, args);

    if (args.len < 2) {
        printUsage();
        return error.InvalidArgsCount;
    }

    const fen = args[args.len - 1];
    if (fen.len > board.MAX_FEN_STRING_LENGTH) {
        return error.InvalidFenString;
    }
    @memcpy(fen_buf[0..fen.len], fen);

    var ret: Args = .{
        .fen = fen_buf[0..fen.len],
    };

    const options = args[1..args.len-1];
    var i: usize = 0;
    while (i < options.len) {
        if (std.mem.eql(u8, options[i], "-d")) {
            i += 1;
            if (i == options.len) {
                return error.ExpectedDepthValue;
            }
            ret.depth = try std.fmt.parseInt(u32, options[i], 10);
        } else if (std.mem.eql(u8, options[i], "-b")) {
            ret.display_board = true;
        } else if (std.mem.eql(u8, options[i], "-m")) {
            ret.display_root_moves = true;
        } else if (std.mem.eql(u8, options[i], "-a")) {
            ret.search_all_depths = true;
        } else if (std.mem.eql(u8, options[i], "-s")) {
            ret.silent = true;
        }
        i += 1;
    }

    return ret;
}

pub fn main() !void {
    var debug_alloc = std.heap.DebugAllocator(.{}).init;
    defer std.debug.assert(debug_alloc.deinit() == .ok);
    const alloc = debug_alloc.allocator();

    const args = try readArgs(alloc);

    var buf_writer = std.io.bufferedWriter(std.io.getStdOut().writer());
    writer = buf_writer.writer().any();

    const bd = try board.readFen(args.fen);
    if (args.display_board and !args.silent) {
        bd.debugPrint();
        std.debug.print("{s}\n", .{board.writeFen(&fen_buf, bd)});
    }

    var b = try board.Board.init(alloc, bd);
    defer b.deinit();

    const min_depth = if (args.search_all_depths) 1 else args.depth; 

    for (min_depth..args.depth+1) |d| {
        const beg = try std.time.Instant.now();
        const score = if (args.display_root_moves)
            perft_root(&b, @intCast(d)) else perft(&b, @intCast(d));
        const end = try std.time.Instant.now();
        const duration = @as(f64, @floatFromInt(end.since(beg))) * (1 / @as(f64, std.time.ns_per_s));
        const nps = @as(f64, @floatFromInt(score)) / duration;
        if (args.silent) {
            try writer.print("{d}\n", .{score});
        } else {
            try writer.print("{d} in {d:.9} nps ({d:.9}s)\n", .{score, nps, duration});
        }
    }
    try buf_writer.flush();
}
