const std = @import("std");
const board = @import("board");
const bot = @import("bot.zig");

const Alloc = std.mem.Allocator;


const HELP = 
    \\TODO: write help here
    \\
    ;

const UCI_ID = 
    \\id name Aposeij
    \\id author Mus_42
    \\
    ;

var command_buf: [1024]u8 = undefined;

pub fn main() !void {
    // TODO use real allocator here
    var debug_alloc = std.heap.DebugAllocator(.{}).init;
    defer std.debug.assert(debug_alloc.deinit() == .ok);
    const alloc = debug_alloc.allocator();

    const stdin = std.io.getStdIn();
    const stdout = std.io.getStdOut();

    const reader = stdin.reader();
    const writer = stdout.writer();
    
    //const log = try std.fs.cwd().createFile("log.txt", .{});
    const startpos = try board.readFen(board.DEFAULT_FEN_STRING);

    var brd = try board.Board.init(alloc, startpos);
    defer brd.deinit();

    var b = bot.Bot.init(alloc);
    defer b.deinit();

    while (true) {
        const command_full = try reader.readUntilDelimiterOrEof(&command_buf, '\n') orelse break;
        // try log.writeAll(command_full);
        // try log.writeAll("\n");
            
        var command_parts = std.mem.splitScalar(u8, std.mem.trim(u8, command_full, &std.ascii.whitespace), ' ');
        const command = command_parts.next() orelse continue;

        if (command.len == 0) continue;

        if (std.mem.eql(u8, command, "uci")) {
            try writer.writeAll(UCI_ID);
            try writer.writeAll("uciok\n");
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
            brd.clearHistory();
        } else if (std.mem.eql(u8, command, "ucinewgame")) {
            // TODO reset bot & board in smart way?
        } else if (std.mem.eql(u8, command, "isready")) {
            try writer.writeAll("readyok\n");
        } else if (std.mem.eql(u8, command, "go")) {
            // TODO
            const best_move = b.bestMove(&brd);
            try writer.print("bestmove {s}\n", .{best_move.algebraicNotation().toStr()});
        } else if (std.mem.eql(u8, command, "ponderhit")) {
            continue;
        } else if (std.mem.eql(u8, command, "stop")) {
            // TODO
            continue;
        } else if (std.mem.eql(u8, command, "displaypos")) {
            brd.data.debugPrint();
            std.debug.print("{s}\n", .{board.writeFen(&command_buf, brd.data)});
        } else if (std.mem.eql(u8, command, "help")) {
            try writer.writeAll(HELP);
        } else {
            try writer.print("unknown command `{s}` found. type `help` to list all available commands\n", .{command});
        }
    }
}
