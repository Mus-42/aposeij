const std = @import("std");
const board = @import("board");
const search = @import("search.zig");
const evaluation = @import("evaluation.zig");
const options = @import("options");

const Alloc = std.mem.Allocator;
const Move = board.Move;
const Board = board.Board;

pub const HELP = 
    \\TODO: write help here
    \\
    ;

pub const UCI_ID = 
    \\id name
    ++ " " ++ options.aposeij_version_string ++
    \\id author Mus_42
    \\
    ;

pub const UciCommand = enum {
    uci,
    isready,
    stop,
    quit,
    position,
    go,

    ucinewgame,
    setoption,
    ponderhit,
    // custom,
    displaypos,
    help,
};

pub const GuiToEngine = struct {
    command: UciCommand,
    arguments: []const u8,
};

pub const SearchInfo = struct {
    depth: u32,
    time_ms: u64,
    nodes: u64,
    nps: u64,
    pv: []const Move,
    score: i32,
};

pub const MAX_COMMAND_LEN = 4096;

pub const UciConnection = struct {
    stdin: *std.Io.Reader, 
    stdout: *std.Io.Writer,
    stdout_mutex: std.Thread.Mutex = .{},

    const Self = @This();

    pub fn init(stdin: *std.Io.Reader, stdout: *std.Io.Writer) Self {
        // there are some _very_ long commands that should fully fit into buffer (like `position`)
        std.debug.assert(stdin.buffer.len == MAX_COMMAND_LEN);

        return .{
            .stdin = stdin,
            .stdout = stdout,
        };
    }

    pub fn deinit(self: *Self) void {
        _ = self;
    }

    /// reads stdin until find valid uci command
    pub fn readCommand(self: *Self) !GuiToEngine {
        while (true) {
            try self.stdin.rebase(MAX_COMMAND_LEN);
            const command_full = try self.stdin.takeDelimiterInclusive('\n');
            const command_string = std.mem.trim(u8, command_full, &std.ascii.whitespace);
            if (command_string.len == 0) 
                continue;
            var command_parts = std.mem.splitAny(u8, command_string, &std.ascii.whitespace);
            const name = command_parts.next() orelse continue;
            const command = std.meta.stringToEnum(UciCommand, name) orelse continue;
            const arguments = command_parts.rest();
            return .{ .command = command, .arguments = arguments };
        }
    }

    // TODO return an error in case invalid command given?

    pub fn parsePositionArgs(self: *const Self, args: []const u8, brd: *Board) !void {
        _ = self;

        var command_parts = std.mem.splitAny(u8, args, &std.ascii.whitespace);
        const pos_type = command_parts.next() orelse return;
        if (std.mem.eql(u8, pos_type, "startpos")) {
            brd.setBoardData(.DEFAULT);
        } else if (std.mem.eql(u8, pos_type, "fen")) {
            const beg = std.mem.indexOf(u8, args, "fen").? + 3;
            const end = std.mem.indexOf(u8, args, "moves") orelse args.len;
            const fen = std.mem.trim(u8, args[beg..end], &std.ascii.whitespace);
            const data = board.readFen(fen) catch return;
            brd.setBoardData(data);
        } else return;

        while (command_parts.peek()) |part| {
            if (std.mem.eql(u8, part, "moves")) break;
            _ = command_parts.next();
        }

        const maybe_moves = command_parts.next() orelse return;
        if (!std.mem.eql(u8, maybe_moves, "moves")) return;

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
    }

    pub fn parseGoArgs(self: *const Self, args: []const u8, side_to_move: board.SideToMove) !search.TimeControls {
        _ = self;

        var command_parts = std.mem.splitAny(u8, args, &std.ascii.whitespace);

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

        var time_controls: search.TimeControls = .{ .to_depth = .{ .target = 8 } };

        if (infinite) {
            time_controls = .infinite;
        } else if (go_depth) |depth| {
            time_controls = .{ .to_depth = .{ .target = depth } };
        } else if (movetime) |time| {
            time_controls = .{ .time_remaining = .{ .ns = std.time.ns_per_ms * @as(u64, time) } };
        } else {
            if (side_to_move == .white and wtime != null) {
                const time_ns = std.time.ns_per_ms * @as(u64, wtime.?) / 20 + std.time.ns_per_ms * @as(u64, winc orelse 0) / 2;
                time_controls = .{ .time_remaining = .{ .ns = time_ns } };
            }
            if (side_to_move == .black and btime != null) {
                const time_ns = std.time.ns_per_ms * @as(u64, btime.?) / 20 + std.time.ns_per_ms * @as(u64, binc orelse 0) / 2;
                time_controls = .{ .time_remaining = .{ .ns = time_ns } };
            }
        }

        return time_controls;
    }

    // Answers

    pub fn bestmove(self: *Self, move: Move) !void {
        self.stdout_mutex.lock();
        defer self.stdout_mutex.unlock();

        try self.stdout.print("bestmove {s}\n", .{move.algebraicNotation().toStr()});
        try self.stdout.flush();
    }

    pub fn readyok(self: *Self) !void {
        self.stdout_mutex.lock();
        defer self.stdout_mutex.unlock();

        try self.stdout.writeAll("readyok\n");
        try self.stdout.flush();
    }

    pub fn uciok(self: *Self) !void {
        self.stdout_mutex.lock();
        defer self.stdout_mutex.unlock();

        try self.stdout.writeAll(UCI_ID);
        try self.stdout.writeAll("uciok\n");
        try self.stdout.flush();
    }

    pub fn searchInfo(self: *Self, info: SearchInfo) !void {
        self.stdout_mutex.lock();
        defer self.stdout_mutex.unlock();

        try self.stdout.print("info depth {[depth]} nodes {[nodes]} nps {[nps]} time {[time]} ", .{
            .depth = info.depth,
            .nodes = info.nodes,
            .nps = info.nps,
            .time = info.time_ms,
        });

        // TODO score mate
        try self.stdout.print("score cp {d}", .{info.score});

        if (info.pv.len > 0) {
            try self.stdout.writeAll(" pv");
            for (info.pv) |move| {
                try self.stdout.print(" {s}", .{move.algebraicNotation().toStr()});
            }
        }
        try self.stdout.writeAll("\n");

        try self.stdout.flush();
    }
};
