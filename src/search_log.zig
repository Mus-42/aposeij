const std = @import("std");
const options = @import("options");
const board = @import("board.zig");

const Move = board.Move;
const Alloc = std.mem.Allocator;

const DummySearchLogger = struct {
    const Self = @This();

    pub fn init(io: std.Io, alloc: Alloc) !Self {
        _ = io;
        _ = alloc;

        return .{};
    }

    pub fn deinit(self: *Self) void {
        _ = self;
    }

    pub fn startNewSearch(self: *Self, position_fen: []const u8, key: u64) !void {
        _ = self;
        _ = position_fen;
        _ = key;
    }

    pub fn finishSearch(self: *Self) !void {
        _ = self;
    }

    pub fn enterNode(self: *Self, key: u64, alpha: i16, beta: i16, is_qsearch: bool) !void {
        _ = self;
        _ = key;
        _ = alpha;
        _ = beta;
        _ = is_qsearch;
    }

    pub fn exitNode(self: *Self, key: u64) !void {
        _ = self;
        _ = key;
    }
};

const MAX_DEPTH = 256; // just in case

pub const SEARCH_LOG_FILE_MAGIC: [16]u8 = "aposeij_slogfile".*;

const LogEntryKind = enum (u16) {
    file_end    = 1,
    node_enter  = 2,
    node_leave  = 3,
};

const LogHeader = extern struct {
    magic: [16]u8 = SEARCH_LOG_FILE_MAGIC,
    startpos_key: u64 = 0,
    startpos_fen: [board.MAX_FEN_STRING_LENGTH+1]u8 = @splat(0),
};

const LogEntryEnd = extern struct {
    kind: LogEntryKind = .file_end,
};

const LogEntryNodeEnter = extern struct {
    kind: LogEntryKind = .node_enter,
    key: u64,
    is_qsearch: bool,
    alpha: i16,
    beta: i16,
};

const LogEntryNodeLeave = extern struct {
    kind: LogEntryKind = .node_leave,
    key: u64,
};


const RealSearchLogger = struct {
    io: std.Io,
    alloc: Alloc,

    log_id: u32 = 0,
    file: ?std.Io.File = null,
    writer: ?std.Io.File.Writer = null,

    ply: u32 = 0,
    node_stack: []NodeInfo,
    io_buf: []u8,

    const Self = @This();

    const NodeInfo = struct {
        key: u64,
    };

    pub fn init(io: std.Io, alloc: Alloc) !Self {
        const io_buf = try alloc.alloc(u8, 4096);
        errdefer alloc.free(io_buf);

        const node_stack = try alloc.alloc(NodeInfo, MAX_DEPTH);
        errdefer alloc.free(node_stack);

        return .{ 
            .io = io,
            .alloc = alloc,
            .io_buf = io_buf,
            .node_stack = node_stack,
        };
    }

    pub fn deinit(self: *Self) void {
        self.flushAndCloseFile();
        self.alloc.free(self.node_stack);
        self.alloc.free(self.io_buf);
    }

    pub fn flushAndCloseFile(self: *Self) void {
        if (self.writer) |*writer| writer.flush() catch {};
        self.writer = null;
        if (self.file) |file| file.close(self.io);
        self.file = null;
    }

    pub fn startNewSearch(self: *Self, position_fen: []const u8, key: u64) !void {
        std.debug.assert(self.file == null);
        std.debug.assert(self.writer == null);
        errdefer self.flushAndCloseFile();

        // TODO cross platform way
        const pid: u32 = @bitCast(std.os.linux.getpid());

        var filename: [64]u8 = undefined;
        var writer = std.Io.Writer.fixed(&filename);
        try writer.print("logs/{:08}-{:06}.slog", .{pid, self.log_id});

        self.log_id += 1;
        self.ply = 0;
        self.file = try std.Io.Dir.cwd().createFile(self.io, filename[0..writer.end], .{});
        self.writer = self.file.?.writer(self.io, self.io_buf);

        var header: LogHeader = .{};
        header.startpos_key = key;
        @memcpy(header.startpos_fen[0..position_fen.len], position_fen);

        try self.writer.?.interface.writeStruct(header, .little);
    }

    pub fn finishSearch(self: *Self) !void {
        defer self.flushAndCloseFile();
        if (self.writer == null) return; 
        try self.writer.?.interface.writeStruct(LogEntryEnd{}, .little);
    }

    pub fn enterNode(self: *Self, key: u64, alpha: i16, beta: i16, is_qsearch: bool) !void {
        errdefer self.flushAndCloseFile();
        if (self.writer == null) return; 

        const enter_node: LogEntryNodeEnter = .{
            .key = key,
            .alpha = alpha,
            .beta = beta,
            .is_qsearch = is_qsearch,
        };
        try self.writer.?.interface.writeStruct(enter_node, .little);
    }

    pub fn exitNode(self: *Self, key: u64) !void {
        const exit_node: LogEntryNodeLeave = .{
            .key = key,
        };
        try self.writer.?.interface.writeStruct(exit_node, .little);
    }
};

pub const SearchLogger = DummySearchLogger;
// pub const SearchLogger = RealSearchLogger;


// todo
pub const SearchLogEntry = union (enum) {
    enter_node: struct { key: u64, a: i16, b: i16 },
    leave_node: struct { key: u64 },
};

pub const SearchLogRecord = struct {
    entries: []SearchLogEntry,

    const Self = @This();

    pub fn deinit(self: *Self, alloc: Alloc) void {
        alloc.free(self.entries);
        self.entries = undefined;
    }
};

pub fn readSearchRecord(io: std.Io, alloc: Alloc, filename: []const u8) !SearchLogRecord {
    const file = try std.Io.Dir.cwd().openFile(io, filename, .{});
    defer file.close(io);

    var buf: [256]u8 = undefined;
    var reader = file.reader(io, &buf);

    const header = try reader.interface.takeStruct(LogHeader, .little);
    if (!std.mem.eql(u8, &header.magic, &SEARCH_LOG_FILE_MAGIC)) {
        return error.InvalidMagic;
    }

    const fen_str_len = std.mem.findScalar(u8, &header.startpos_fen, 0).?;
    const fen = header.startpos_fen[0..fen_str_len];
    std.debug.print("key: {X:016}, fen: `{s}`\n", .{header.startpos_key, fen});

    var entries: std.ArrayList(SearchLogEntry) = .empty;
    defer entries.deinit(alloc);

    while (true) {
        const raw_kind = try reader.interface.peekInt(u16, .little);
        const kind: LogEntryKind = @enumFromInt(raw_kind);

        switch (kind) {
            .file_end => break,
            .node_enter => {
                const entry = try reader.interface.takeStruct(LogEntryNodeEnter, .little);

                try entries.append(alloc, .{ .enter_node = .{
                    .key = entry.key,
                    .a = entry.alpha,
                    .b = entry.beta,
                }});
            },
            .node_leave => {
                const entry = try reader.interface.takeStruct(LogEntryNodeLeave, .little);

                try entries.append(alloc, .{ .leave_node = .{
                    .key = entry.key
                }});
            },
        }
    }

    return .{ .entries = try entries.toOwnedSlice(alloc) };
}
