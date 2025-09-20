const std = @import("std");
const board = @import("board");
const search = @import("search.zig");
const evaluation = @import("evaluation.zig");
const uci = @import("uci.zig");
const options = @import("options");

const Alloc = std.mem.Allocator;

var stdin_buf: [4096]u8 = undefined;
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
            .isready => try uci_connection.readyok(),
            .stop => try control.stopSearch(),
            .quit => break,
            .position => try uci_connection.parsePositionArgs(command.arguments, &brd),
            .go => {
                const time_controls = try uci_connection.parseGoArgs(command.arguments, brd.data.side_to_move);
                try control.startSearch(&brd, time_controls);
            },
            else => {},
        }
    }
}
