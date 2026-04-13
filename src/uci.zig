const std = @import("std");
const board = @import("board");
const search = @import("search.zig");
const evaluation = @import("evaluation.zig");
const options = @import("options");

const Alloc = std.mem.Allocator;
const Move = board.Move;
const Board = board.Board;

pub const HELP = 
    \\TODO write something about engine / UCI
    \\
    \\Commands:
    \\  uci                                                         establish uci conneciton
    \\  isready                                                     block connection and wait for engine to complete previous commands
    \\  quit                                                        quit
    \\  stop                                                        stop search
    \\  position [fen <FENSTRING> | startpos] [moves <MOVES...>]    set position
    \\  go depth DEPTH                                              search DEPTH ply deep
    \\  go infinite                                                 search util stop recived
    \\  go movetime TIME                                            search for TIME ms
    \\  go nodes NODES                                              search exactly NODES nodes
    \\  go [TODO options]                                           TODO message
    \\  ucinewgame                                                  clear cache
    \\  setoption                                                   NOOP
    \\  ponderhit                                                   NOOP
    \\  help                                                        print this message
    \\
    \\Custom/Debug commands:
    \\  displaypos                                                  debug print postion, fen string, position hash
    \\  perft DEPTH                                                 run perft test DEPTH moves deep
    \\  bench                                                       run small benchmark to estimate engine speed on current machine
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
    setoption,
    isready,
    stop,
    quit,
    position,
    go,
    ponderhit,
    ucinewgame,
    // custom,
    displaypos,
    help,
    perft,
    bench,
    
    unknown,
};

pub const GuiToEngine = struct {
    command: UciCommand,
    command_full_str: []const u8,
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

pub const MAX_COMMAND_LEN = 8192;

pub const UciOptions = struct {
    strict_mode: bool = false,
};

pub const UciConnection = struct {
    stdin: *std.Io.Reader, 
    stdout: *std.Io.Writer,
    stdout_mutex: std.Thread.Mutex = .{},
    options: UciOptions,

    const Self = @This();

    pub fn init(stdin: *std.Io.Reader, stdout: *std.Io.Writer, opt: UciOptions) Self {
        return .{
            .stdin = stdin,
            .stdout = stdout,
            .options = opt,
        };
    }

    pub fn deinit(self: *Self) void {
        _ = self;
    }

    /// reads stdin until find valid uci command
    pub fn readCommand(self: *Self) !GuiToEngine {
        // there are some _very_ long commands that should fully fit into buffer (like `position`)
        std.debug.assert(self.stdin.buffer.len == MAX_COMMAND_LEN);

        while (true) {
            try self.stdin.rebase(MAX_COMMAND_LEN);
            const command_full = try self.stdin.takeDelimiterInclusive('\n');
            const command_string = std.mem.trim(u8, command_full, &std.ascii.whitespace);
            if (command_string.len == 0) 
                continue;
            var command_parts = std.mem.splitAny(u8, command_string, &std.ascii.whitespace);
            const name = command_parts.next() orelse continue;
            const command = std.meta.stringToEnum(UciCommand, name) orelse .unknown;
            const arguments = command_parts.rest();
            return .{ .command = command, .command_full_str = command_string, .arguments = arguments };
        }
    }

    // TODO return an error in case invalid command given?

    pub fn reportError(self: *const Self, comptime msg: []const u8, args: anytype) !void {
        if (!self.options.strict_mode) return;
        try self.stdout.print("error: " ++ msg ++ "\n", args);
        try self.stdout.flush();
    }

    pub fn parsePositionArgs(self: *const Self, args: []const u8, brd: *Board) !void {
        var command_parts = std.mem.splitAny(u8, args, &std.ascii.whitespace);
        const pos_type = command_parts.next()
            orelse return self.reportError("missing `position` command args", .{});

        if (std.mem.eql(u8, pos_type, "startpos")) {
            brd.setBoardData(.DEFAULT);
        } else if (std.mem.eql(u8, pos_type, "fen")) {
            const beg = std.mem.indexOf(u8, args, "fen").? + 3;
            const end = std.mem.indexOf(u8, args, "moves") orelse args.len;
            const fen = std.mem.trim(u8, args[beg..end], &std.ascii.whitespace);
            const data = board.readFen(fen) catch |err|
                return self.reportError("invalid fen string `{s}`: {s}", .{fen, @errorName(err)});
            brd.setBoardData(data);
        } else return;

        while (command_parts.peek()) |part| {
            if (std.mem.eql(u8, part, "moves")) break;
            // TODO check what we are skipping in strict mode?
            _ = command_parts.next();
        }

        // TODO checks in strict mode?
        const maybe_moves = command_parts.next() orelse return;
        if (!std.mem.eql(u8, maybe_moves, "moves")) return;

        while (command_parts.next()) |move| {
            var moves = board.Moves{};
            board.genMoves(brd.data, &moves);
            var found = false;
            for (moves.moves()) |m| {
                if (std.mem.eql(u8, m.algebraicNotation().toStr(), move)) {
                    if (brd.makeMove(m))
                        break;
                    found = true;
                    break;
                }
            }
            if (!found) {
                return self.reportError("invalid move `{s}` provided", .{move});
            }
        }
        
        // TODO check sanity (2 kings, not a mate, etc.)
    }

    

    pub fn parseGoArgs(self: *const Self, args_str: []const u8, side_to_move: board.SideToMove) !search.TimeControls {

        const GoArgs = struct {
            depth: ?u32 = null,
            wtime: ?u32 = null,
            btime: ?u32 = null,
            winc: ?u32 = null,
            binc: ?u32 = null,
            movetime: ?u32 = null,
            nodes: ?u64 = null,
            infinite: bool = false,
        };

        // TODO this is dumm, rewrite later into something more generic?
        var args: GoArgs = .{};
        var command_parts = std.mem.splitAny(u8, args_str, &std.ascii.whitespace);
        parts_loop: while (command_parts.next()) |cmd| {
            inline for (std.meta.fields(GoArgs)) |field| {
                if (std.mem.eql(u8, cmd, field.name)) {
                    switch (field.type) {
                        bool => @field(args, field.name) = true,
                        ?u32 => {
                            const val_str = command_parts.next() orelse {
                                try self.reportError("go: invalid depth value", .{});
                                if (self.options.strict_mode) {
                                    // TODO instead of that return special "SkipCommand" error?
                                    break;
                                }
                                continue :parts_loop;
                            };
                            @field(args, field.name) = std.fmt.parseInt(u32, val_str, 10) catch {
                                try self.reportError("go: failed to parse `{s}` as u32 value for {s}", .{val_str, field.name});
                                if (self.options.strict_mode) {
                                    break;
                                }
                                continue :parts_loop;
                            };
                        },
                        ?u64 => {
                            const val_str = command_parts.next() orelse {
                                try self.reportError("go: invalid depth value", .{});
                                if (self.options.strict_mode) {
                                    break;
                                }
                                continue :parts_loop;
                            };
                            @field(args, field.name) = std.fmt.parseInt(u64, val_str, 10) catch {
                                try self.reportError("go: failed to parse `{s}` as u32 value for {s}", .{val_str, field.name});
                                if (self.options.strict_mode) {
                                    break;
                                }
                                continue :parts_loop;
                            };
                        },
                        else => std.debug.panic("TODO: parsing for {}\n", .{field.type}),
                    }
                    continue :parts_loop;
                }
            }
            try self.reportError("go: unknown part `{s}` provided, skipping remaining command parts", .{cmd});
            break;
        }


        // TODO better way to check that
        // TODO movestogo

        var time_controls: search.TimeControls = .{ .to_depth = .{ .target = 8 } };

        if (args.infinite) {
            time_controls = .infinite;
            args.infinite = false;
        } else if (args.depth) |depth| {
            time_controls = .{ .to_depth = .{ .target = depth } };
            args.depth = null;
        } else if (args.nodes) |nodes| {
            time_controls = .{ .nodes = .{ .hard_limit = nodes } };
            args.nodes = null;
        } else if (args.movetime) |time| {
            time_controls = .{ .time_remaining = .{ .ns = std.time.ns_per_ms * @as(u64, time) } };
            args.movetime = null;
        } else {
            // TODO move that to other function
            if (side_to_move == .white and args.wtime != null) {
                const time_ns = std.time.ns_per_ms * @as(u64, args.wtime.?) / 20 + std.time.ns_per_ms * @as(u64, args.winc orelse 0) / 2;
                time_controls = .{ .time_remaining = .{ .ns = time_ns } };
                args.wtime = null;
            }
            if (side_to_move == .black and args.btime != null) {
                const time_ns = std.time.ns_per_ms * @as(u64, args.btime.?) / 20 + std.time.ns_per_ms * @as(u64, args.binc orelse 0) / 2;
                time_controls = .{ .time_remaining = .{ .ns = time_ns } };
                args.btime = null;
            }
        }

        if (self.options.strict_mode) {
            inline for (std.meta.fields(GoArgs)) |field| {
                switch (field.type) {
                    bool => if (@field(args, field.name)) {
                        try self.reportError("go: unused arg: `{s}`", .{field.name});
                    },
                    ?u32, ?u64 => if (@field(args, field.name) != null) {
                        try self.reportError("go: unused arg: `{s}`", .{field.name});
                    },
                    else => unreachable,
                }
            }
        }

        return time_controls;
    }

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
        try self.writeSearchInfo(info);
        try self.stdout.flush();
    }

    pub fn lockStdout(self: *Self) void {
        self.stdout_mutex.lock();
    }

    pub fn unlockStdout(self: *Self) void {
        self.stdout_mutex.unlock();
    }

    fn writeSearchInfo(self: *Self, info: SearchInfo) !void {
        try self.stdout.print("info depth {[depth]:2} nodes {[nodes]:8} nps {[nps]:8} time {[time]:6} ", .{
            .depth = info.depth,
            .nodes = info.nodes,
            .nps = info.nps,
            .time = info.time_ms,
        });
        
        const mate_ply = search.SCORE_MATE_ABS - @abs(info.score);
        if (mate_ply <= search.SCORE_MATE_EPS) {
            const mate_dist = (1 + mate_ply) / 2;
            const is_winning = info.score > 0;
            // std.debug.assert(is_winning == (mate_ply % 2 == 1));
            const sign: u8 = if (is_winning) '+' else '-';
            var buf: [8]u8 = undefined;
            var stream = std.Io.fixedBufferStream(&buf);
            try stream.writer().print("{c}{d}", .{sign, mate_dist});
            const mate_string = buf[0..stream.pos];
            try self.stdout.print("score mate {s:6}", .{mate_string});
            // try self.stdout.print("score mate {d:6}", .{mate_ply});
        } else {
            try self.stdout.print("score cp {d:8}", .{info.score});
        }

        if (info.pv.len > 0) {
            try self.stdout.writeAll(" pv");
            for (info.pv) |move| {
                try self.stdout.print(" {s}", .{move.algebraicNotation().toStr()});
            }
        }
        try self.stdout.writeAll("\n");
    }
};
