const std = @import("std");
const builtin = @import("builtin");
const Alloc = std.mem.Allocator;

pub const PieceKind = enum(u4) {
    w_pawn,
    w_knight,
    w_bishop,
    w_rook,
    w_queen,
    w_king,

    b_pawn,
    b_knight,
    b_bishop,
    b_rook,
    b_queen,
    b_king,
};

pub const SideToMove = enum(u1) {
    white,
    black,

    pub fn flip(self: *@This()) void {
        self.* = if (self.* == .white) .black else .white;
    }
};

pub const Square = u6;

pub fn debugRenderBitboard(bitboard: u64) void {
    for (0..8) |i| {
        var rank: u8 = @truncate(bitboard >> @intCast(56 - i * 8));
        for (0..8) |_| {
            std.debug.print("{d} ", .{rank & 1});
            rank >>= 1;
        }
        std.debug.print("\n", .{});
    }
}

pub fn rankMask(rank: u3) u64 {
    return @as(u64, 0xFF) << (@as(u6, @intCast(rank)) * 8);
}

pub fn fileMask(file: u3) u64 {
    return @as(u64, 0x0101010101010101) << file;
}


pub fn bitboardUp(b: u64) u64 {
    var new_b = b;
    new_b &= ~rankMask(7);
    new_b <<= 8;
    return new_b;
}

pub fn bitboardDown(b: u64) u64 {
    var new_b = b;
    new_b &= ~rankMask(0);
    new_b >>= 8;
    return new_b;
}

pub fn bitboardRight(b: u64) u64 {
    var new_b = b;
    new_b &= ~fileMask(7);
    new_b <<= 1;
    return new_b;
}

pub fn bitboardLeft(b: u64) u64 {
    var new_b = b;
    new_b &= ~fileMask(0);
    new_b >>= 1;
    return new_b;
}

fn sideFlipBitboard(b: u64) u64 {
    return @byteSwap(b);
}

test sideFlipBitboard {
    const A: u64 =
        0b00001100 << (7 * 8) |
        0b00101100 << (6 * 8) |
        0b01001100 << (5 * 8) |
        0b10001001 << (4 * 8) |
        0b00101100 << (3 * 8) |
        0b00001000 << (2 * 8) |
        0b01001101 << (1 * 8) |
        0b00011000 << (0 * 8);
    const B: u64 =
        0b00001100 << (0 * 8) |
        0b00101100 << (1 * 8) |
        0b01001100 << (2 * 8) |
        0b10001001 << (3 * 8) |
        0b00101100 << (4 * 8) |
        0b00001000 << (5 * 8) |
        0b01001101 << (6 * 8) |
        0b00011000 << (7 * 8);

    try std.testing.expectEqual(A, sideFlipBitboard(B));
}

pub const CastlingRights = packed struct (u4) {
    white_kingside: bool =  true,
    white_queenside: bool = true,
    black_kingside: bool = true,
    black_queenside: bool = true,

    const Self = @This();

    pub fn updateAfterMove(self: *Self, move: Move) void {
        const WHITE_QUEENSIDE_MASK = @as(u64, 1 << 4  | 1 << 0);
        const WHITE_KINGSIDE_MASK  = @as(u64, 1 << 4  | 1 << 7);
        const BLACK_QUEENSIDE_MASK = @as(u64, 1 << 60 | 1 << 56);
        const BLACK_KINGSIDE_MASK  = @as(u64, 1 << 60 | 1 << 63);
        const move_mask = @as(u64, 1) << move.from | @as(u64, 1) << move.to;
        self.white_queenside = self.white_queenside and WHITE_QUEENSIDE_MASK & move_mask == 0;
        self.white_kingside  = self.white_kingside  and WHITE_KINGSIDE_MASK  & move_mask == 0;
        self.black_queenside = self.black_queenside and BLACK_QUEENSIDE_MASK & move_mask == 0;
        self.black_kingside  = self.black_kingside  and BLACK_KINGSIDE_MASK  & move_mask == 0;
    }
};

// actually less then max possible in chess but way less than you may want to rewind
const MAX_HISTORY_LEN = 1024; 

pub const Board = struct {
    pub const BoardData = struct {
        pieces: [12]u64,
        halfmoves: u12 = 0,
        halfmoves50: u6 = 0,
        castling_rights: CastlingRights = .{},
        side_to_move: SideToMove = .white,
        en_passant_target: ?Square = null,
        zobrist_key: u64 = 0,

        pub const EMPTY: BoardData = .{ .pieces = @splat(0) };
        pub const DEFAULT = readFen(DEFAULT_FEN_STRING) catch unreachable;

        pub fn getPieceAt(self: BoardData, square: Square) ?PieceKind {
            inline for (0..12) |i| {
                if (self.pieces[i] & @as(u64, 1) << square != 0) {
                    return @enumFromInt(i);
                }
            }
            return null;
        }

        pub fn popPieceAt(self: *BoardData, square: Square) ?PieceKind {
            inline for (0..12) |i| {
                if (self.pieces[i] & @as(u64, 1) << square != 0) {
                    self.pieces[i] &= ~(@as(u64, 1) << square);
                    return @enumFromInt(i);
                }
            }
            return null;
        }

        pub fn clearPieceAt(self: *BoardData, square: Square) void {
            inline for (0..12) |i| {
                self.pieces[i] &= ~(@as(u64, 1) << square);
            }
        }

        pub fn setPieceAt(self: *BoardData, square: Square, piece: PieceKind) void {
            self.clearPieceAt(square);
            self.pieces[@intFromEnum(piece)] |= @as(u64, 1) << square;
        }

        pub fn debugPrint(self: BoardData) void {
            for (0..8) |ri| {
                const r = 7 - ri;
                for (0..8) |f| {
                    if (self.getPieceAt(@intCast(r * 8 + f))) |p| {
                        std.debug.print("{c} ", .{FEN_PIECE_TO_CHAR[@intFromEnum(p)]});
                    } else {
                        std.debug.print(". ", .{});
                    }
                }
                std.debug.print("\n", .{});
            }
        }
    };

    alloc: Alloc,
    data: BoardData,
    history: History,

    pub const HistoryEntry = struct {
        data: BoardData,
        move: Move,
    };

    pub const History = std.ArrayListUnmanaged(HistoryEntry);
    pub const Self = @This();

    pub fn init(alloc: Alloc, data: BoardData) !Self {
        const history = try History.initCapacity(alloc, MAX_HISTORY_LEN);
        return .{
            .data = data,
            .alloc = alloc,
            .history = history,
        };
    }
    
    

    pub fn makeMove(self: *Self, move: Move) void {
        self.history.appendAssumeCapacity(.{ .data = self.data, .move = move });
        
        self.data.castling_rights.updateAfterMove(move);
        self.data.en_passant_target = null;

        if (move.is_promotion) {
            const piece = self.data.popPieceAt(move.from);
            const old_piece = self.data.popPieceAt(move.to);
            const new_piece = move.extra.promotion.toPiece(self.data.side_to_move);

            std.debug.assert(piece == .w_pawn or piece == .b_pawn);
            std.debug.assert((old_piece != null) == move.is_capture);

            self.data.setPieceAt(move.to, new_piece);
        } else if (!move.is_capture and move.extra.quiet != .none) {
            const king_rank_mask = move.from & 0x38;
            const rook_from, const rook_to =  if (move.extra.quiet == .king_castle) 
                .{ 7 | king_rank_mask, 5 | king_rank_mask } 
                else 
                .{ 0 | king_rank_mask, 3 | king_rank_mask };

            const king = self.data.popPieceAt(move.from).?;
            const rook = self.data.popPieceAt(rook_from).?;
            std.debug.assert(king == .w_king or king == .b_king);
            std.debug.assert(rook == .w_rook or rook == .b_rook);
            self.data.setPieceAt(move.to, king);
            self.data.setPieceAt(rook_to, rook);
        } else if (move.is_capture and move.extra.capture == .ep_capture) {
            const piece = self.data.popPieceAt(move.from).?;
            const old_piece = self.data.popPieceAt(move.to ^ 8);
            std.debug.assert(old_piece != null);
            self.data.setPieceAt(move.to, piece);
        } else {
            const piece = self.data.popPieceAt(move.from).?;
            const old_piece = self.data.popPieceAt(move.to);

            std.debug.assert((old_piece != null) == move.is_capture);

            if ((piece == .b_pawn or piece == .w_pawn) and move.isLooksLikePawn2SquareMove()) {
                self.data.en_passant_target = @intCast((@as(u7, move.from) + @as(u7, move.to)) >> 1);
            }

            self.data.setPieceAt(move.to, piece);
        }

        self.data.side_to_move.flip();
    }

    pub fn unmakeMove(self: *Self) void {
        self.data = self.history.pop().?.data;
    }

    pub fn setBoardData(self: *Self, bd: BoardData) void {
        self.clearHistory();
        self.data = bd;
    }
    
    pub fn clearHistory(self: *Self) void {
        self.history.clearRetainingCapacity();
    }

    pub fn deinit(self: *Self) void {
        self.history.deinit(self.alloc);
    }

    pub fn debugDumpMoveHistory(self: *const Self) void {
        for (self.history.items) |entry| {
            std.debug.print("{s} ", .{entry.move.algebraicNotation().toStr()});
        }
        std.debug.print("\n", .{});
    }
};

pub const DEFAULT_FEN_STRING = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";

const FEN_PIECE_TO_CHAR: [12]u8 = "PNBRQKpnbrqk".*;

const FEN_CASTLING_TO_CHAR: [4]u8 = "KQkq".*;

const SQUARE_TO_STRING: [64][2]u8 = .{
    "a1".*, "b1".*, "c1".*, "d1".*, "e1".*, "f1".*, "g1".*, "h1".*,
    "a2".*, "b2".*, "c2".*, "d2".*, "e2".*, "f2".*, "g2".*, "h2".*,
    "a3".*, "b3".*, "c3".*, "d3".*, "e3".*, "f3".*, "g3".*, "h3".*,
    "a4".*, "b4".*, "c4".*, "d4".*, "e4".*, "f4".*, "g4".*, "h4".*,
    "a5".*, "b5".*, "c5".*, "d5".*, "e5".*, "f5".*, "g5".*, "h5".*,
    "a6".*, "b6".*, "c6".*, "d6".*, "e6".*, "f6".*, "g6".*, "h6".*,
    "a7".*, "b7".*, "c7".*, "d7".*, "e7".*, "f7".*, "g7".*, "h7".*,
    "a8".*, "b8".*, "c8".*, "d8".*, "e8".*, "f8".*, "g8".*, "h8".*,
};

pub const ToSquareError = error{ InvalidSquareStringLength, InvalidSquareStringCharacter };

pub fn stringToSquare(s: []const u8) ToSquareError!Square {
    if (s.len != 2) return error.InvalidSquareStringLength;
    if (s[0] < 'a' or 'h' < s[0]) return error.InvalidSquareStringCharacter;
    if (s[1] < '1' or '8' < s[1]) return error.InvalidSquareStringCharacter;
    const square = (s[1] - '1') * 8 + s[0] - 'a';
    return @intCast(square);
}

test stringToSquare {
    try std.testing.expectEqual(0, stringToSquare("a1"));
    try std.testing.expectEqual(9, stringToSquare("b2"));
    try std.testing.expectEqual(61, stringToSquare("f8"));

    try std.testing.expectError(error.InvalidSquareStringCharacter, stringToSquare("aa"));
    try std.testing.expectError(error.InvalidSquareStringCharacter, stringToSquare("42"));
    try std.testing.expectError(error.InvalidSquareStringLength, stringToSquare("a1 "));
}

pub const FenReadError = error{ IncompleteFenData, InvalidFenCharacter } || ToSquareError;

pub fn readFen(s: []const u8) FenReadError!Board.BoardData {
    var board = Board.BoardData.EMPTY;
    var i: usize = 0;

    for (0..8) |ri| {
        const r = 7 - ri;
        var f: usize = 0;
        while (f != 8 and i < s.len) : (i += 1) {
            if ('1' <= s[i] and s[i] <= '8') {
                f += s[i] - '0';
                continue;
            }
            const piece_value = std.mem.indexOfScalar(u8, &FEN_PIECE_TO_CHAR, s[i]) orelse return error.InvalidFenCharacter;
            const piece: PieceKind = @enumFromInt(piece_value);
            board.setPieceAt(@intCast(r * 8 + f), piece);
            f += 1;
        }
        if (r > 0) {
            if (s[i] != '/') return error.InvalidFenCharacter;
            i += 1;
        }
    }

    while (i < s.len and std.ascii.isWhitespace(s[i])) i += 1;
    if (i >= s.len) return error.IncompleteFenData;

    if (s[i] == 'w') {
        board.side_to_move = .white;
    } else if (s[i] == 'b') {
        board.side_to_move = .black;
    } else return error.InvalidFenCharacter;
    i += 1;

    while (i < s.len and std.ascii.isWhitespace(s[i])) i += 1;
    if (i >= s.len) return error.IncompleteFenData;

    var castling_rights: u4 = 0;
    if (s[i] == '-') {
        i += 1;
    } else {
        while (i < s.len and !std.ascii.isWhitespace(s[i])) : (i += 1) {
            const n = std.mem.indexOfScalar(u8, &FEN_CASTLING_TO_CHAR, s[i]) orelse return error.InvalidFenCharacter;
            castling_rights |= @as(u4, 1) << @intCast(n);
        }
    }
    board.castling_rights = @bitCast(castling_rights);

    while (i < s.len and std.ascii.isWhitespace(s[i])) i += 1;
    if (i >= s.len) return error.IncompleteFenData;

    if (s[i] == '-') {
        board.en_passant_target = null;
        i += 1;
    } else {
        if (i + 1 >= s.len) return error.IncompleteFenData;
        board.en_passant_target = try stringToSquare(s[i .. i + 2]);
        i += 2;
    }

    while (i < s.len and std.ascii.isWhitespace(s[i])) i += 1;
    if (i >= s.len) return error.IncompleteFenData;

    const h50 = i;
    while (i < s.len and !std.ascii.isWhitespace(s[i])) i += 1;
    board.halfmoves50 = std.fmt.parseUnsigned(u6, s[h50..i], 10) catch return error.IncompleteFenData;

    while (i < s.len and std.ascii.isWhitespace(s[i])) i += 1;
    if (i >= s.len) return error.IncompleteFenData;

    const h = i;
    while (i < s.len and !std.ascii.isWhitespace(s[i])) i += 1;
    board.halfmoves = std.fmt.parseUnsigned(u6, s[h..i], 10) catch return error.IncompleteFenData;

    while (i < s.len and std.ascii.isWhitespace(s[i])) i += 1;

    if (i != s.len) return error.InvalidFenCharacter;

    return board;
}

/// Maximum string lenght that can be produced by `writeFen`
/// 64 squares + 7 delim. + 1 active color + 4 castling + 2 en passant
/// + 2 fifty move clock + 4 full move number + 5 spaces = 89
pub const MAX_FEN_STRING_LENGTH = 89;

pub fn writeFen(buffer: []u8, board: Board.BoardData) []const u8 {
    std.debug.assert(buffer.len >= MAX_FEN_STRING_LENGTH);

    var i: usize = 0;

    // pieces
    for (0..8) |ri| {
        const r = 7 - ri;
        var emty_count: u8 = 0;
        for (0..8) |f| {
            const piece = board.getPieceAt(@intCast(r * 8 + f)) orelse {
                emty_count += 1;
                continue;
            };
            if (emty_count != 0) {
                buffer[i] = '0' + emty_count;
                i += 1;
                emty_count = 0;
            }
            buffer[i] = FEN_PIECE_TO_CHAR[@intFromEnum(piece)];
            i += 1;
        }
        if (emty_count != 0) {
            buffer[i] = '0' + emty_count;
            i += 1;
        }
        if (r != 0) {
            buffer[i] = '/';
            i += 1;
        }
    }

    buffer[i] = ' ';
    i += 1;
    buffer[i] = if (board.side_to_move == .white) 'w' else 'b';
    i += 1;
    buffer[i] = ' ';
    i += 1;

    const castling_rights: u4 = @bitCast(board.castling_rights);
    if (castling_rights == 0) {
        buffer[i] = '-';
        i += 1;
    } else {
        inline for (0..4) |r| {
            if (castling_rights >> r & 1 != 0) {
                buffer[i] = FEN_CASTLING_TO_CHAR[r];
                i += 1;
            }
        }
    }

    const en_passant = if (board.en_passant_target) |square| &SQUARE_TO_STRING[square] else "-";

    const v = std.fmt.bufPrint(buffer[i..], " {s} {d} {d}", .{ en_passant, board.halfmoves50, board.halfmoves }) catch unreachable;
    i += v.len;

    return buffer[0..i];
}

test writeFen {
    var buf: [MAX_FEN_STRING_LENGTH]u8 = undefined;
    try std.testing.expectEqualStrings(DEFAULT_FEN_STRING, writeFen(&buf, Board.BoardData.DEFAULT));
}

test "board representation" {
    var buf: [MAX_FEN_STRING_LENGTH]u8 = undefined;

    const FENS = [_][]const u8{
        "rnbqkbnr/pp1ppppp/8/2p5/4P3/8/PPPP1PPP/RNBQKBNR w KQkq c6 0 2",
        "rnbqkbnr/pp1ppppp/8/2p5/4P3/5N2/PPPP1PPP/RNBQKB1R b KQkq - 1 2",
    };

    for (FENS) |fen| {
        const board = try readFen(fen);
        try std.testing.expectEqualStrings(fen, writeFen(&buf, board));
    }
}

pub const Move = packed struct (u16) {
    const QuietSpetial = enum(u2) {
        none,
        king_castle,
        queen_castle,
    };

    const CaptureSpetial = enum(u2) {
        none,
        ep_capture,
    };

    const PromotionTarget = enum(u2) {
        knight,
        bishop,
        rook,
        queen,

        fn toPiece(self: PromotionTarget, side: SideToMove) PieceKind {
            const kind = @as(u4, @intFromEnum(self)) + 1;
            const side_shift = (@as(u4, 0) -% @intFromEnum(side)) & 6;
            return @enumFromInt(kind + side_shift);
        }
    };

    const MoveExtra = packed union {
        quiet: QuietSpetial,
        capture: CaptureSpetial,
        promotion: PromotionTarget,
    };

    const MoveStrBuf = struct {
        buf: [5]u8,

        pub fn toStr(self: *const MoveStrBuf) []const u8 {
            return std.mem.trim(u8, &self.buf, &.{' '});
        }
    };

    from: Square,
    to: Square,
    is_promotion: bool = false,
    is_capture: bool = false,
    // extra priorities: promotion > capture > quiet
    extra: MoveExtra = undefined, 

    const Self = @This();

    pub const NULL: Self = .{ .from = 0, .to = 0, .extra = .{ .quiet = .none }};

    pub fn isLooksLikePawn2SquareMove(self: Self) bool {
        const move_mask = @as(u64, 1) << self.to | @as(u64, 1) << self.from;
        const PAWN_LINES_MASK_W = rankMask(1) | rankMask(3);
        const PAWN_LINES_MASK_B = rankMask(4) | rankMask(6);
        return PAWN_LINES_MASK_W & move_mask == move_mask or PAWN_LINES_MASK_B & move_mask == move_mask;
    }
    
    pub fn algebraicNotation(self: Self) MoveStrBuf {
        var ret: [5]u8 = undefined;

        ret[0..2].* = SQUARE_TO_STRING[self.from];
        ret[2..4].* = SQUARE_TO_STRING[self.to];
        ret[4] = if (self.is_promotion) switch (self.extra.promotion) {
            .knight => 'n',
            .bishop => 'b',
            .rook   => 'r',
            .queen  => 'q',
        } else ' ';

        return MoveStrBuf { .buf = ret };
    }
};

pub const MAX_MOVES = 220;

const MoveGenState = struct {
    all: u64,
    white: u64,
    black: u64,
    pieces: [12]u64,
    castling: CastlingRights,
    ep_target: ?Square,
};

// TODO instead of generating attacks for all squares and then checking single one it's better to generate & check for single square?
const AttackGenState = struct {
    attacks: [6]u64,
    attacks_all: u64,
    invalidation_mask: u64,

    // Knights -> change only if knight dissapears
    // sliding pieces & king -> change if past attack square OR piece changed

    const Self = @This();

    fn init(state: MoveGenState) Self {
        var self: Self = .{
            .attacks = @splat(0),
            .attacks_all = 0,
            .invalidation_mask = 0,
        };

        self.attacks[0] = genPieceAttacks(blackPawnAttacks, state.pieces[@intFromEnum(PieceKind.b_pawn)], state.black, state.white);
        self.attacks[1] = genPieceAttacks(knightMoves, state.pieces[@intFromEnum(PieceKind.b_knight)],    state.black, state.white);
        self.attacks[2] = genPieceAttacks(bishopMoves, state.pieces[@intFromEnum(PieceKind.b_bishop)],    state.black, state.white);
        self.attacks[3] = genPieceAttacks(rookMoves,   state.pieces[@intFromEnum(PieceKind.b_rook)],      state.black, state.white);
        self.attacks[4] = genPieceAttacks(queenMoves,  state.pieces[@intFromEnum(PieceKind.b_queen)],     state.black, state.white);
        self.attacks[5] = genPieceAttacks(kingMoves,   state.pieces[@intFromEnum(PieceKind.b_king)],      state.black, state.white);
        
        var all: u64 = 0;
        for (self.attacks) |attack| {
            all |= attack;
        }
        self.attacks_all = all;

        self.invalidation_mask = 
            self.attacks[@intFromEnum(PieceKind.w_bishop)] |
            self.attacks[@intFromEnum(PieceKind.w_rook)] |
            self.attacks[@intFromEnum(PieceKind.w_queen)];

        return self;
    }

    fn afterMoveAttacksBitboard(self: *const Self, state: MoveGenState, from: u6, to: u6, comptime is_ep: bool) u64 {
        const white_moved, const black_captured = if (is_ep) 
            .{ @as(u64, 1) << from | @as(u64, 1) << to, @as(u64, 1) << (to ^ 8) }
            else 
            .{ @as(u64, 1) << from | @as(u64, 1) << to, @as(u64, 1) << to };
       
        const move_diff = white_moved | black_captured;

        if (move_diff & (self.invalidation_mask | state.black) == 0)
            return self.attacks_all;
        
        const pawns   = state.pieces[@intFromEnum(PieceKind.b_pawn)];
        const knights = state.pieces[@intFromEnum(PieceKind.b_knight)];
        const bishops = state.pieces[@intFromEnum(PieceKind.b_bishop)];
        const rooks   = state.pieces[@intFromEnum(PieceKind.b_rook)];
        const queens  = state.pieces[@intFromEnum(PieceKind.b_queen)];

        const bishop_attacks = self.attacks[@intFromEnum(PieceKind.w_bishop)];
        const rook_attacks   = self.attacks[@intFromEnum(PieceKind.w_rook)];
        const queen_attacks  = self.attacks[@intFromEnum(PieceKind.w_queen)];

        var attacks: u64 = 0;

        if (black_captured & pawns != 0) {
            attacks |= genPieceAttacks(blackPawnAttacks, pawns & ~black_captured, state.black & ~black_captured, state.white ^ white_moved);
        } else {
            attacks |= self.attacks[@intFromEnum(PieceKind.w_pawn)];
        }

        if (black_captured & knights != 0) {
            attacks |= genPieceAttacks(knightMoves, knights & ~black_captured, state.black & ~black_captured, state.white ^ white_moved);
        } else {
            attacks |= self.attacks[@intFromEnum(PieceKind.w_knight)];
        }

        if (move_diff & (bishops | bishop_attacks) != 0) {
            attacks |= genPieceAttacks(bishopMoves, bishops & ~black_captured, state.black & ~black_captured, state.white ^ white_moved);
        } else {
            attacks |= bishop_attacks;
        }

        if (move_diff & (rooks | rook_attacks) != 0) {
            attacks |= genPieceAttacks(rookMoves, rooks & ~black_captured, state.black & ~black_captured, state.white ^ white_moved);
        } else {
            attacks |= rook_attacks;
        }

        if (move_diff & (queens | queen_attacks) != 0) {
            attacks |= genPieceAttacks(queenMoves, queens & ~black_captured, state.black & ~black_captured, state.white ^ white_moved);
        } else {
            attacks |= queen_attacks;
        }

        attacks |= self.attacks[@intFromEnum(PieceKind.w_king)];

        return attacks;
    }
};

pub const Moves = struct {
    p: [MAX_MOVES]Move = undefined,
    i: usize = 0,

    const Self = @This();

    pub fn clear(self: *Self) void {
        self.i = 0;
    }

    pub fn count(self: *const Self) usize {
        return self.i;
    }

    pub fn add(self: *Self, move: Move) void {
        self.p[self.i] = move;
        self.i += 1;
    }

    pub fn moves(self: *const Self) []const Move {
        return self.p[0..self.i];
    }
};

pub fn genMoves(board: Board.BoardData, moves: *Moves) void {
    var state = computeMovegenState(board);
    if (board.side_to_move != .white) {
        state = sideFlipState(state);
    }

    genWhiteMoves(state, moves);

    if (board.side_to_move != .white) {
        for (0..moves.i) |i| {
            moves.p[i] = sideFlipMove(moves.p[i]);
        }
    }
}



fn genWhiteMoves(state: MoveGenState, moves: *Moves) void {
    const attacks_gen = AttackGenState.init(state);

    genWhitePawnMoves(state, attacks_gen, moves, state.pieces[@intFromEnum(PieceKind.w_pawn)]);
    genPieceMoves(knightMoves, state, attacks_gen, moves, state.pieces[@intFromEnum(PieceKind.w_knight)]);
    genPieceMoves(bishopMoves, state, attacks_gen, moves, state.pieces[@intFromEnum(PieceKind.w_bishop)]);
    genPieceMoves(rookMoves,   state, attacks_gen, moves, state.pieces[@intFromEnum(PieceKind.w_rook)]);
    genPieceMoves(queenMoves,  state, attacks_gen, moves, state.pieces[@intFromEnum(PieceKind.w_queen)]);

    const kings = state.pieces[@intFromEnum(PieceKind.w_king)];
    std.debug.assert(@popCount(kings) == 1);
    const king_pos: Square = @intCast(@ctz(kings));
    var to_all = kingMoves(king_pos, state.white, state.black) & ~state.white;
    while (to_all != 0) : (to_all &= to_all - 1) {
        const to: Square = @intCast(@ctz(to_all));
        const black_attacks = attacks_gen.afterMoveAttacksBitboard(state, king_pos, to, false);
        if (black_attacks >> to & 1 != 0) {
            continue;
        }
        moves.add(.{
            .from = king_pos,
            .to = to,
            .is_capture = (state.black >> to & 1) != 0,
            .extra = .{ .capture = .none },
        });
    }
    
    to_all = whiteKingCastling(king_pos, state.white, state.black, attacks_gen.attacks_all, state.castling.white_kingside, state.castling.white_queenside);
    while (to_all != 0) : (to_all &= to_all - 1) {
        const to: Square = @intCast(@ctz(to_all));
        const extra: Move.QuietSpetial = if (to == 2) .queen_castle else .king_castle;
        moves.add(.{
            .from = king_pos,
            .to = to,
            .extra = .{ .quiet = extra },
        });
    }
}

fn genWhitePawnMoves(state: MoveGenState, attack_gen: AttackGenState, moves: *Moves, piece_bitboard: u64) void {
    var pieces = piece_bitboard;
    const ep_targets = if (state.ep_target) |target| @as(u64, 1) << target else 0;
    while (pieces != 0) : (pieces &= pieces - 1) {
        const from: Square = @intCast(@ctz(pieces));
        var to_all = whitePawnMoves(from, state.white, state.black, ep_targets) & ~state.white;
        while (to_all != 0) : (to_all &= to_all - 1) {
            const to: Square = @intCast(@ctz(to_all));
            
            if (ep_targets >> to & 1 != 0) {
                const black_attacks = attack_gen.afterMoveAttacksBitboard(state, from, to, true);

                if (state.pieces[@intFromEnum(PieceKind.w_king)] & black_attacks != 0) {
                    continue;
                }

                moves.add(.{
                    .from = from,
                    .to = to,
                    .is_capture = true,
                    .extra = .{ .capture = .ep_capture },
                });
                continue;
            }

            const black_attacks = attack_gen.afterMoveAttacksBitboard(state, from, to, false);

            if (state.pieces[@intFromEnum(PieceKind.w_king)] & black_attacks != 0) {
                continue;
            }
            
            if (to >> 3 == 0 or to >> 3 == 7) {
                const PROMOTION_TARGETS = [4]Move.PromotionTarget{ .knight, .bishop, .rook, .queen };
                inline for (PROMOTION_TARGETS) |target| {
                    moves.add(.{
                        .from = from,
                        .to = to,
                        .is_promotion = true,
                        .is_capture = (state.black >> to & 1) != 0,
                        .extra = .{ .promotion = target },
                    });
                }
                continue;
            }


            moves.add(.{
                .from = from,
                .to = to,
                .is_capture = (state.black >> to & 1) != 0,
                .extra = .{ .capture = .none },
            });
        }
    }
}

fn genPieceMoves(comptime movesFn: anytype, state: MoveGenState, attack_gen: AttackGenState, moves: *Moves, piece_bitboard: u64) void {
    var pieces = piece_bitboard;
    while (pieces != 0) : (pieces &= pieces - 1) {
        const from: Square = @intCast(@ctz(pieces));
        var to_all = movesFn(from, state.white, state.black) & ~state.white;
        while (to_all != 0) : (to_all &= to_all - 1) {
            const to: Square = @intCast(@ctz(to_all));
            const black_attacks = attack_gen.afterMoveAttacksBitboard(state, from, to, false);

            // king under attack
            if (state.pieces[@intFromEnum(PieceKind.w_king)] & black_attacks != 0) {
                continue;
            }

            moves.add(.{
                .from = from,
                .to = to,
                .is_capture = (state.black >> to & 1) != 0,
                .extra = .{ .capture = .none },
            });
        }
    }
}


// TODO could be smarter with sliding piece attacks
// TODO it's not obvious that "white" and "black" actually mean side to move and other side instead of colors. rename it 
fn genPieceAttacks(comptime movesFn: anytype, piece_bitboard: u64, white_all: u64, black_all: u64) u64 {
    var pieces = piece_bitboard;
    var attacks_all: u64 = 0;
    while (pieces != 0) : (pieces &= pieces - 1) {
        const from: Square = @intCast(@ctz(pieces));
        // be careful with a pawn attacks/moves
        const to_all = movesFn(from, white_all, black_all);
        attacks_all |= to_all;
    }
    return attacks_all;
}


fn computeMovegenState(board: Board.BoardData) MoveGenState {
    var white: u64 = 0;
    inline for (0..6) |i| {
        white |= board.pieces[i];
    }

    var black: u64 = 0;
    inline for (6..12) |i| {
        black |= board.pieces[i];
    }

    return .{
        .all = white | black,
        .white = white,
        .black = black,
        .pieces = board.pieces,
        .castling = board.castling_rights,
        .ep_target = board.en_passant_target,
    };
}

pub fn sideFlipMove(move: Move) Move {
    return .{
        .from = sideFlipSquare(move.from),
        .to = sideFlipSquare(move.to),
        .is_capture = move.is_capture,
        .is_promotion = move.is_promotion,
        .extra = move.extra,
    };
}

pub fn sideFlipSquare(square: Square) Square {
    return square ^ 0b111000;
}

pub fn sideFlipCastling(castling: CastlingRights) CastlingRights {
    return .{
        .white_kingside  = castling.black_kingside,
        .white_queenside = castling.black_queenside,
        .black_kingside  = castling.white_kingside,
        .black_queenside = castling.white_queenside,
    };
}

fn sideFlipState(state: MoveGenState) MoveGenState {
    var pieces: [12]u64 = undefined;
    inline for (0..12) |i| {
        const new_i = if (i < 6) i+6 else i-6;
        pieces[new_i] = sideFlipBitboard(state.pieces[i]);
    }
    
    const white = sideFlipBitboard(state.black);
    const black = sideFlipBitboard(state.white);

    return .{
        .pieces = pieces,
        .all = white | black,
        .white = white,
        .black = black,
        .castling = sideFlipCastling(state.castling),
        .ep_target = if (state.ep_target) |target| sideFlipSquare(target) else null,
    };
}


fn whitePawnMoves(piece_pos: Square, white_pieces: u64, black_pieces: u64, ep_targets: u64) u64 {
    const all_pieces = white_pieces | black_pieces;
    const piece = @as(u64, 1) << piece_pos;
    const up_raw = bitboardUp(piece);
    const up = up_raw & ~all_pieces;
    const doubleup = bitboardUp(up & rankMask(2)) & ~all_pieces;

    const captures = (bitboardLeft(up_raw) | bitboardRight(up_raw)) & (all_pieces | ep_targets);
    const moves = up | doubleup;

    return moves | captures;
}

fn blackPawnAttacks(piece_pos: Square, white_pieces: u64, black_pieces: u64) u64 {
    _ = white_pieces;
    _ = black_pieces;
    const piece = @as(u64, 1) << piece_pos;
    const down_raw = bitboardDown(piece);

    const captures = bitboardLeft(down_raw) | bitboardRight(down_raw);
    return captures;
}

fn knightMoves(piece_pos: Square, white_pieces: u64, black_pieces: u64) u64 {
    _ = white_pieces;
    _ = black_pieces;

    // TODO generate moves bitboard and then move it to the position?
    const pos: u64 = @as(u64, 1) << piece_pos;
    const pos_2u = bitboardUp(bitboardUp(pos));
    const pos_2d = bitboardDown(bitboardDown(pos));
    const pos_2l = bitboardLeft(bitboardLeft(pos));
    const pos_2r = bitboardRight(bitboardRight(pos));
    var moves: u64 = 0;
    moves |= bitboardLeft(pos_2u);
    moves |= bitboardRight(pos_2u);
    moves |= bitboardLeft(pos_2d);
    moves |= bitboardRight(pos_2d);
    moves |= bitboardUp(pos_2l);
    moves |= bitboardDown(pos_2l);
    moves |= bitboardUp(pos_2r);
    moves |= bitboardDown(pos_2r);
    return moves;
}

fn bishopMoves(piece_pos: Square, white_pieces: u64, black_pieces: u64) u64 {
    const all_pieces = white_pieces | black_pieces;
    var moves: u64 = 0;
    var pos_a: u64 = @as(u64, 1) << piece_pos;
    var pos_b: u64 = @as(u64, 1) << piece_pos;
    var pos_c: u64 = @as(u64, 1) << piece_pos;
    var pos_d: u64 = @as(u64, 1) << piece_pos;
    inline for (0..7) |_| {
        pos_a = bitboardUp(bitboardLeft(pos_a));
        pos_b = bitboardUp(bitboardRight(pos_b));
        pos_c = bitboardDown(bitboardLeft(pos_c));
        pos_d = bitboardDown(bitboardRight(pos_d));

        moves |= pos_a | pos_b | pos_c | pos_d;

        pos_a &= ~all_pieces;
        pos_b &= ~all_pieces;
        pos_c &= ~all_pieces;
        pos_d &= ~all_pieces;
    }
    return moves;
}

fn rookMoves(piece_pos: Square, white_pieces: u64, black_pieces: u64) u64 {
    const all_pieces = white_pieces | black_pieces;
    var moves: u64 = 0;
    var pos_u: u64 = @as(u64, 1) << piece_pos;
    var pos_d: u64 = @as(u64, 1) << piece_pos;
    var pos_l: u64 = @as(u64, 1) << piece_pos;
    var pos_r: u64 = @as(u64, 1) << piece_pos;
    inline for (0..7) |_| {
        pos_u = bitboardUp(pos_u);
        pos_d = bitboardDown(pos_d);
        pos_l = bitboardLeft(pos_l);
        pos_r = bitboardRight(pos_r);

        moves |= pos_u | pos_d | pos_l | pos_r;

        pos_u &= ~all_pieces;
        pos_d &= ~all_pieces;
        pos_l &= ~all_pieces;
        pos_r &= ~all_pieces;
    }
    return moves;
}

fn queenMoves(piece_pos: Square, white_pieces: u64, black_pieces: u64) u64 {
    return rookMoves(piece_pos, white_pieces, black_pieces) | bishopMoves(piece_pos, white_pieces, black_pieces);
}

fn kingMoves(piece_pos: Square, white_pieces: u64, black_pieces: u64) u64 {
    _ = white_pieces;
    _ = black_pieces;
    const pos: u64 = @as(u64, 1) << piece_pos;
    const vertical = bitboardUp(pos) | pos | bitboardDown(pos);
    const moves = bitboardLeft(vertical) | vertical | bitboardRight(vertical);
    return moves;
}

fn whiteKingCastling(piece_pos: Square, white_pieces: u64, black_pieces: u64, black_attacks: u64, can_kingside: bool, can_queenside: bool) u64 {
    if (piece_pos != 4) return 0;

    const all_pieces = white_pieces | black_pieces;

    const KINGSIDE_ATTACKS_MASK  = 0b01110000;
    const QUEENSIDE_ATTACKS_MASK = 0b00011100;
    const KINGSIDE_SPACE_MASK    = 0b01100000;
    const QUEENSIDE_SPACE_MASK   = 0b00001110;

    var moves: u64 = 0;
    if (black_attacks & KINGSIDE_ATTACKS_MASK == 0 and all_pieces & KINGSIDE_SPACE_MASK == 0 and can_kingside) {
        moves |= 1 << 6;
    }
    if (black_attacks & QUEENSIDE_ATTACKS_MASK == 0 and all_pieces & QUEENSIDE_SPACE_MASK == 0 and can_queenside) {
        moves |= 1 << 2;
    }

    return moves;
}
