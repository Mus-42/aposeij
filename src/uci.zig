const std = @import("std");
const board = @import("board");
const bot = @import("bot.zig");
const options = @import("options");

const Alloc = std.mem.Allocator;


const HELP = 
    \\TODO: write help here
    \\
    ;

const UCI_ID = 
    \\id name
    ++ " " ++ options.aposeij_version_string ++
    \\id author Mus_42
    \\
    ;

var stdin_buf: [4096]u8 = undefined;
var stdout_buf: [256]u8 = undefined;

pub fn main() !void {
    // TODO use real allocator here
    var debug_alloc = std.heap.DebugAllocator(.{}).init;
    defer std.debug.assert(debug_alloc.deinit() == .ok);
    const alloc = debug_alloc.allocator();


    var stdin = std.fs.File.stdin().reader(&stdin_buf);
    var stdout = std.fs.File.stdout().writer(&stdout_buf);
    
    //const log = try std.fs.cwd().createFile("log.txt", .{});
    const startpos = try board.readFen(board.DEFAULT_FEN_STRING);

    var brd = try board.Board.init(alloc, startpos);
    defer brd.deinit();

    var b = try bot.Bot.init(alloc, &brd);
    defer b.deinit();

    while (true) {
        const command_full = stdin.interface.takeDelimiterInclusive('\n') catch |err| {
            if (err == error.EndOfStream) {
                break;
            }
            return err;
        };
            
        var command_parts = std.mem.splitScalar(u8, std.mem.trim(u8, command_full, &std.ascii.whitespace), ' ');
        const command = command_parts.next() orelse continue;

        if (command.len == 0) continue;

        if (std.mem.eql(u8, command, "uci")) {
            try stdout.interface.writeAll(UCI_ID);
            try stdout.interface.writeAll("uciok\n");
        } else if (std.mem.eql(u8, command, "quit")) {
            break;
        } else if (std.mem.eql(u8, command, "setoption")) {
            // TODO
        } else if (std.mem.eql(u8, command, "position")) {
            const pos_type = command_parts.next() orelse continue;
            if (std.mem.eql(u8, pos_type, "startpos")) {
                brd.setBoardData(startpos);
            } else if (std.mem.eql(u8, pos_type, "fen")) {
                const beg = std.mem.indexOf(u8, command_full, "fen").? + 3;
                const end = std.mem.indexOf(u8, command_full, "moves") orelse command_full.len;
                const fen = std.mem.trim(u8, command_full[beg..end], &std.ascii.whitespace);
                const data = board.readFen(fen) catch continue;
                brd.setBoardData(data);
            } else continue;
            
            while (command_parts.peek()) |part| {
                if (std.mem.eql(u8, part, "moves")) break;
                _ = command_parts.next();
            }

            const maybe_moves = command_parts.next() orelse continue;
            if (!std.mem.eql(u8, maybe_moves, "moves")) continue;

            while (command_parts.next()) |move| {
                var moves = board.Moves{};
                board.genMoves(brd.data, &moves);
                var found = false;
                for (moves.moves()) |m| {
                    if (std.mem.eql(u8, m.algebraicNotation().toStr(), move)) {
                        brd.makeMove(m);
                        found = true;
                        break;
                    }
                }
                if (!found) {
                    break;
                }
            }
        } else if (std.mem.eql(u8, command, "ucinewgame")) {
            // TODO reset bot & board in smart way?
        } else if (std.mem.eql(u8, command, "isready")) {
            try stdout.interface.writeAll("readyok\n");
        } else if (std.mem.eql(u8, command, "go")) {
            var go_depth: ?u32 = null;
            var wtime: ?u32 = null;
            var btime: ?u32 = null;
            var winc: ?u32 = null;
            var binc: ?u32 = null;
            var movetime: ?u32 = null;
            var infinite = false;

            while (command_parts.next()) |cmd| {
                if (std.mem.eql(u8, cmd, "infinite")) {
                    infinite = true;
                } else if (std.mem.eql(u8, cmd, "depth")) {
                    const depth_str = command_parts.next() orelse break;
                    go_depth = std.fmt.parseInt(u32, depth_str, 10) catch continue;
                } else if (std.mem.eql(u8, cmd, "wtime")) {
                    const wtime_str = command_parts.next() orelse break;
                    wtime = std.fmt.parseInt(u32, wtime_str, 10) catch continue;
                } else if (std.mem.eql(u8, cmd, "btime")) {
                    const btime_str = command_parts.next() orelse break;
                    btime = std.fmt.parseInt(u32, btime_str, 10) catch continue;
                } else if (std.mem.eql(u8, cmd, "winc")) {
                    const winc_str = command_parts.next() orelse break;
                    winc = std.fmt.parseInt(u32, winc_str, 10) catch continue;
                } else if (std.mem.eql(u8, cmd, "binc")) {
                    const binc_str = command_parts.next() orelse break;
                    binc = std.fmt.parseInt(u32, binc_str, 10) catch continue;
                } else if (std.mem.eql(u8, cmd, "movetime")) {
                    const movetime_str = command_parts.next() orelse break;
                    movetime = std.fmt.parseInt(u32, movetime_str, 10) catch continue;
                } else break;
            }

            var time_controls: bot.TimeControls = .{ .to_depth = .{ .target = 8 } };

            if (infinite) {
                time_controls = .infinite;
            } else if (go_depth) |depth| {
                time_controls = .{ .to_depth = .{ .target = depth } };
            } else if (movetime) |time| {
                time_controls = .{ .time_remaining = .{ .ns = std.time.ns_per_ms * @as(u64, time) } };
            } else {
                const side_to_move = brd.data.side_to_move;
                if (side_to_move == .white and wtime != null) {
                    const time_ns = std.time.ns_per_ms * @as(u64, wtime.?) / 20 + std.time.ns_per_ms * @as(u64, winc orelse 0) / 2;
                    time_controls = .{ .time_remaining = .{ .ns = time_ns } };
                }
                if (side_to_move == .black and btime != null) {
                    const time_ns = std.time.ns_per_ms * @as(u64, btime.?) / 20 + std.time.ns_per_ms * @as(u64, binc orelse 0) / 2;
                    time_controls = .{ .time_remaining = .{ .ns = time_ns } };
                }
            }

            const best_move = b.bestMove(&stdout.interface, time_controls);
            try stdout.interface.print("bestmove {s}\n", .{best_move.algebraicNotation().toStr()});
        } else if (std.mem.eql(u8, command, "ponderhit")) {
            continue;
        } else if (std.mem.eql(u8, command, "stop")) {
            // TODO
            continue;
        } else if (std.mem.eql(u8, command, "evalpos")) {
            const score_static = bot.Bot.whiteEval(brd.data);
            std.debug.print("score (static): {d} (in cp, for white)\n", .{score_static});
        } else if (std.mem.eql(u8, command, "displaypos")) {
            brd.data.debugPrint();
            var fen_buf: [board.MAX_FEN_STRING_LENGTH]u8 = undefined;
            std.debug.print("{s}\n", .{board.writeFen(&fen_buf, brd.data)});
        } else if (std.mem.eql(u8, command, "help")) {
            try stdout.interface.writeAll(HELP);
        } else {
            try stdout.interface.print("unknown command `{s}` found. type `help` to list all available commands\n", .{command});
        }

        try stdout.interface.flush();
    }

    try stdout.interface.flush();
}
