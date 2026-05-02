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

pub fn sideFlipSquare(square: Square) Square {
    return square ^ 0b111000;
}

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
    white_kingside: bool  = true,
    white_queenside: bool = true,
    black_kingside: bool  = true,
    black_queenside: bool = true,

    const Self = @This();

    const NO_RIGHTS: Self = .{
        .white_kingside  = false,
        .white_queenside = false,
        .black_kingside  = false,
        .black_queenside = false,
    };

    const WHITE_QUEENSIDE_MASK = @as(u64, 1 << 4  | 1 << 0);
    const WHITE_KINGSIDE_MASK  = @as(u64, 1 << 4  | 1 << 7);
    const BLACK_QUEENSIDE_MASK = @as(u64, 1 << 60 | 1 << 56);
    const BLACK_KINGSIDE_MASK  = @as(u64, 1 << 60 | 1 << 63);

    pub fn updateAfterMove(self: *Self, move: Move) void {
        const move_mask = @as(u64, 1) << move.from | @as(u64, 1) << move.to;

        self.white_queenside = self.white_queenside and WHITE_QUEENSIDE_MASK & move_mask == 0;
        self.white_kingside  = self.white_kingside  and WHITE_KINGSIDE_MASK  & move_mask == 0;
        self.black_queenside = self.black_queenside and BLACK_QUEENSIDE_MASK & move_mask == 0;
        self.black_kingside  = self.black_kingside  and BLACK_KINGSIDE_MASK  & move_mask == 0;
    }

    pub fn sideFlip(self: Self) Self {
        return .{
            .white_kingside  = self.black_kingside,
            .white_queenside = self.black_queenside,
            .black_kingside  = self.white_kingside,
            .black_queenside = self.white_queenside,
        };
    }
};

// actually less then max possible in chess but way less than you may want to rewind
const MAX_HISTORY_LEN = 1024; 

const MovegenComptimeLoopups = struct {
    // magic boards stuff
    bishop_blockers_mask: [64]u64,
    rook_blockers_mask: [64]u64,
    bishop_magic: [64]u64,
    rook_magic: [64]u64,
    bishop_shift: [64]u6,
    rook_shift: [64]u6,
    bishop_index: [64]usize,
    rook_index: [64]usize,
    // precomputed moves
    knight_moves: [64]u64,
    king_moves: [64]u64,
};

const LOOKUPS: MovegenComptimeLoopups = blk: {
    @setEvalBranchQuota(50000);

    var magic: MovegenComptimeLoopups = .{
        .bishop_blockers_mask = undefined,
        .rook_blockers_mask = undefined,
        .rook_magic = undefined,
        .bishop_magic = undefined,
        .rook_shift = undefined,
        .bishop_shift = undefined,
        .rook_index = undefined,
        .bishop_index = undefined,
        .knight_moves = undefined,
        .king_moves = undefined,
    };

    const exclude_ranks = ~rankMask(0) & ~rankMask(7);
    const exclude_files = ~fileMask(0) & ~fileMask(7);

    for (0..64) |i| {
        const pos: Square = @intCast(i);
        const rank: u3 = @intCast(pos >> 3);
        const file: u3 = @intCast(pos & 7);
        magic.knight_moves[i] = knightAttacksSlow(pos);
        magic.king_moves[i] = kingAttacksSlow(pos);
        const exclude_pos = ~(@as(u64, 1) << pos);
        magic.bishop_blockers_mask[i] = bishopAttacksSlow(pos, 0) & exclude_ranks & exclude_files & exclude_pos;
        magic.rook_blockers_mask[i] = ((rankMask(rank) & exclude_files) | (fileMask(file) & exclude_ranks)) & exclude_pos;
    }
    
    break :blk magic;
};

fn blockersBoardN(blockers: u64, index: u64) u64 {
    var res: u64 = 0;
    var block = blockers;
    while (block != 0) {
        res |= (index & 1) << @ctz(block);
        index >>= 1;
        block &= block - 1;
    }
    return res;
}

pub const Movegen = struct {
    state: State = .{},
    is_dirty: bool = true,
    
    const State = struct {
        white: u64 = 0,
        black: u64 = 0,
        pieces: [12]u64 = @splat(0),
        castling: CastlingRights = .{},
        ep_target: ?Square = null,
    };

    const Self = @This();

    pub fn init(alloc: Alloc) !*Self {
        const self = try alloc.create(Self);
        errdefer alloc.destroy(self);
        self.* = .{};

        return self;
    }

    pub fn deinit(self: *Self, alloc: Alloc) void {
        _ = self;
        _ = alloc;
    }

    pub fn setDirty(self: *Self) void {
        self.is_dirty = true;
    }

    // is side-to-move king in check?
    pub fn isKingInCheck(self: *const Self) bool {
        const kings = self.state.pieces[@intFromEnum(PieceKind.w_king)];
        return self.isAnySquareUnderAttack(kings);
    }

    // TODO split into phases (like caputres / quiets ...?)

    pub fn genMoves(self: *Self, board: *const Board.BoardData, movelist: *MoveList) void {
        self.computeState(board);
        self.acutallyGenWiteMoves(movelist);

        if (board.side_to_move != .white) {
            for (movelist.moves()) |*move| {
                move.sideFlip();
            }
        }
    }

    fn acutallyGenWiteMoves(self: *const Self, movelist: *MoveList) void {
        const white = self.state.white;
        inline for (0..6) |i| {
            const kind: PieceKind = @enumFromInt(i);
            var pieces = self.state.pieces[i];
            while (pieces != 0) : (pieces &= pieces - 1) {
                const from: Square = @intCast(@ctz(pieces));
                var to_all = self.getMovesBitboard(kind, from) & ~white;
                while (to_all != 0) : (to_all &= to_all - 1) {
                    const to: Square = @intCast(@ctz(to_all));
                    self.emitMoves(kind, from, to, movelist);
                }
            }
        }
    }

    inline fn emitMoves(self: *const Self, comptime kind: PieceKind, from: Square, to: Square, movelist: *MoveList) void {
        const black = self.state.black;

        if (kind == .w_pawn) {
            // ep capture
            if (self.state.ep_target == to) {
                @branchHint(.unlikely);
                movelist.add(.{
                    .from = from,
                    .to = to,
                    .is_capture = true,
                    .extra = .{ .capture = .ep_capture },
                });
                return;
            }

            // promotion
            if (to >> 3 == 7) {
                @branchHint(.unlikely);
                const PROMOTION_TARGETS = [4]Move.PromotionTarget{ .knight, .bishop, .rook, .queen };
                inline for (PROMOTION_TARGETS) |target| {
                    movelist.add(.{
                        .from = from,
                        .to = to,
                        .is_promotion = true,
                        .is_capture = (black >> to & 1) != 0,
                        .extra = .{ .promotion = target },
                    });
                }
                return;
            }
        }

        // castle
        if (kind == .w_king and from == 4 and (to == 2 or to == 6)) {
            @branchHint(.unlikely);
            const extra: Move.QuietSpetial = if (to == 2) .queen_castle else .king_castle;
            movelist.add(.{
                .from = from,
                .to = to,
                .extra = .{ .quiet = extra },
            });
            return;
        }

        // regular move / regular capture
        movelist.add( .{
            .from = from,
            .to = to,
            .is_capture = (black >> to & 1) != 0,
            .extra = .{ .capture = .none },
        });
    }
        
    // white piece -> all moves, black piece -> only attacks (isAttackedBy)
    fn getMovesBitboard(self: *const Self, comptime kind: PieceKind, pos: Square) u64 {
        const white = self.state.white;
        const black = self.state.black;
        const blockers = white | black;

        switch (kind) {
            .w_pawn => {
                const ep = if (self.state.ep_target) |target| @as(u64, 1) << target else 0;
                return whitePawnMoves(pos, white, black, ep);
            },
            .b_pawn => return blackPawnAttacksRev(pos),
            .w_knight, .b_knight => return LOOKUPS.knight_moves[pos],
            .w_bishop, .b_bishop => return bishopAttacksSlow(pos, blockers),
            .w_rook, .b_rook => return rookAttacksSlow(pos, blockers),
            .w_queen, .b_queen => return queenAttacksSlow(pos, blockers),
            .w_king => return LOOKUPS.king_moves[pos] | self.whiteKingCastling(),
            .b_king => return LOOKUPS.king_moves[pos],
        }
    }

    fn isAnySquareUnderAttack(self: *const Self, squares: u64) bool {
        inline for (6..12) |i| {
            const kind: PieceKind = @enumFromInt(i);
            var pos = squares;
            while (pos != 0) : (pos &= pos - 1) {
                const piece_pos: Square = @intCast(@ctz(pos));
                if (self.getMovesBitboard(kind, piece_pos) & self.state.pieces[i] != 0) {
                    return true;
                }
            }
        }
        return false;
    }

    fn whiteKingCastling(self: *const Self) u64 {
        const state = &self.state;
        if (!state.castling.white_kingside and !state.castling.white_queenside) 
            return 0;

        std.debug.assert(@ctz(state.pieces[@intFromEnum(PieceKind.w_king)]) == 4);
        std.debug.assert(@popCount(state.pieces[@intFromEnum(PieceKind.w_king)]) == 1);

        const all_pieces = state.white | state.black;

        const can_kingside = state.castling.white_kingside;
        const can_queenside = state.castling.white_queenside;

        const KINGSIDE_ATTACKS_MASK  = 0b01110000;
        const QUEENSIDE_ATTACKS_MASK = 0b00011100;
        const KINGSIDE_SPACE_MASK    = 0b01100000;
        const QUEENSIDE_SPACE_MASK   = 0b00001110;

        var moves: u64 = 0;
        if (all_pieces & KINGSIDE_SPACE_MASK == 0 and can_kingside) {
            if (!self.isAnySquareUnderAttack(KINGSIDE_ATTACKS_MASK)) {
                moves |= 1 << 6;
            }
        }
        if (all_pieces & QUEENSIDE_SPACE_MASK == 0 and can_queenside) {
            if (!self.isAnySquareUnderAttack(QUEENSIDE_ATTACKS_MASK)) {
                moves |= 1 << 2;
            }
        }

        return moves;
    }

    fn sideFlipState(self: *Self) void {
        const state = &self.state;
        std.mem.swap([6]u64, state.pieces[0..6], state.pieces[6..12]);

        inline for (0..12) |i| {
            state.pieces[i] = sideFlipBitboard(state.pieces[i]);
        }

        const white = sideFlipBitboard(state.black);
        const black = sideFlipBitboard(state.white);

        state.white = white;
        state.black = black;

        state.castling = state.castling.sideFlip();

        if (state.ep_target) |*target| {
            target.* = sideFlipSquare(target.*);
        }
    }

    fn computeState(self: *Self, board: *const Board.BoardData) void {
        if (self.is_dirty) {
            self.is_dirty = false;

            var white: u64 = 0;
            inline for (0..6) |i| {
                white |= board.pieces[i];
            }

            var black: u64 = 0;
            inline for (6..12) |i| {
                black |= board.pieces[i];
            }


            self.state = .{
                .white = white,
                .black = black,
                .pieces = board.pieces,
                .castling = board.castling_rights,
                .ep_target = board.en_passant_target,
            };

            if (board.side_to_move != .white) {
                self.sideFlipState();
            }
        }

        std.debug.assert(@popCount(self.state.pieces[@intFromEnum(PieceKind.w_king)]) == 1);
        std.debug.assert(@popCount(self.state.pieces[@intFromEnum(PieceKind.b_king)]) == 1);
        
        // TODO figure out why is this fails but movegen passes testsuite anyways???
        // std.debug.assert(board.is_in_check == self.isKingInCheck());
        if (board.is_in_check) {
            self.state.castling = .NO_RIGHTS;
        } else {
            self.state.castling = board.castling_rights;
            if (board.side_to_move != .white) {
                self.state.castling = self.state.castling.sideFlip();
            }
        }

    }
};

pub const Board = struct {
    pub const BoardData = struct {
        pieces: [12]u64,
        fullmoves: u12 = 0,
        halfmoves50: u8 = 0,
        castling_rights: CastlingRights = .{},
        side_to_move: SideToMove = .white,
        en_passant_target: ?Square = null,
        is_in_check: bool = false,
        zobrist_key: u64 = 0,

        pub const EMPTY: BoardData = .{ .pieces = @splat(0) };
        pub const DEFAULT: BoardData = blk: {
            var brd: BoardData = .{
                .pieces = .{
                    0x000000000000ff00,
                    0x0000000000000042,
                    0x0000000000000024,
                    0x0000000000000081,
                    0x0000000000000008,
                    0x0000000000000010,
                    0x00ff000000000000,
                    0x4200000000000000,
                    0x2400000000000000,
                    0x8100000000000000,
                    0x0800000000000000,
                    0x1000000000000000,
                },
            };
            brd.recomputeZobristKey();
            break :blk brd;
        };

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

        pub fn debugPrint(self: BoardData, sink: *std.io.Writer) !void {
            for (0..8) |ri| {
                const r = 7 - ri;
                for (0..8) |f| {
                    if (self.getPieceAt(@intCast(r * 8 + f))) |p| {
                        try sink.print("{c} ", .{FEN_PIECE_TO_CHAR[@intFromEnum(p)]});
                    } else {
                        try sink.print(". ", .{});
                    }
                }
                try sink.print("\n", .{});
            }
        }

        pub fn recomputeZobristKey(self: *BoardData) void {
            var key: u64 = 0;

            for (0..12) |i| {
                var piece = self.pieces[i];
                while (piece != 0) : (piece &= piece - 1) {
                    const pos = @ctz(piece);
                    key ^= zobrist_hash.PIECE_SQUARES[i << 6 | pos];
                }
            }

            key ^= zobrist_hash.CASTLING_RIGHTS[@as(u4, @bitCast(self.castling_rights))];
            
            if (self.side_to_move == .black) {
                key ^= zobrist_hash.SIDE_TO_MOVE;
            }

            if (self.en_passant_target) |square| {
                key ^= zobrist_hash.EP_FILE[square & 7];
            }

            self.zobrist_key = key;
        }
    };

    alloc: Alloc,
    data: BoardData,
    history: History,
    movegen: *Movegen,

    pub const HistoryEntry = struct {
        data: BoardData,
        move: Move,
    };

    pub const History = std.ArrayListUnmanaged(HistoryEntry);
    pub const Self = @This();

    pub fn init(alloc: Alloc, movegen: *Movegen) !Self {
        var history = try History.initCapacity(alloc, MAX_HISTORY_LEN);
        errdefer history.deinit(alloc);

        return .{
            .data = .DEFAULT,
            .alloc = alloc,
            .history = history,
            .movegen = movegen,
        };
    }

    pub fn deinit(self: *Self) void {
        self.history.deinit(self.alloc);
    }

    pub fn cloneFrom(self: *Self, other: *const Self) !void {
        self.history.clearRetainingCapacity();
        self.history.appendSliceAssumeCapacity(other.history.items);
        self.data = other.data;
        self.movegen.setDirty();
    }

    pub fn makeNullMove(self: *Self) bool {
        self.history.appendAssumeCapacity(.{ .data = self.data, .move = Move.NULL });

        self.data.en_passant_target = null;

        // TODO pointless???
        self.movegen.setDirty();
        self.movegen.computeState(&self.data);

        const is_opponent_king_in_check = self.movegen.isKingInCheck();
        // illegal move
        if (is_opponent_king_in_check) {
            self.unmakeMove();
            return true;
        }

        if (self.data.side_to_move == .black) {
            self.data.halfmoves50 += 1;
        }


        self.data.side_to_move.flip();
        std.debug.assert(!self.movegen.is_dirty);
        self.movegen.sideFlipState();
        self.data.is_in_check = self.movegen.isKingInCheck();
        // TODO don't compute from scratch each single time
        self.data.recomputeZobristKey();
        // self.data.castling_rights.updateAfterMove(move);

        return false;
    }

    pub fn unmakeNullMove(self: *Self) void {
        // TODO: safety checks?
        self.unmakeMove();
    }


    pub fn makeMove(self: *Self, move: Move) bool {
        self.history.appendAssumeCapacity(.{ .data = self.data, .move = move });
        
        self.data.en_passant_target = null;

        var reset_50_moves_clock = false;

        if (move.is_promotion) {
            const piece = self.data.popPieceAt(move.from);
            const old_piece = self.data.popPieceAt(move.to);
            const new_piece = move.extra.promotion.toPiece(self.data.side_to_move);

            reset_50_moves_clock = true;

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

            reset_50_moves_clock = true;

            std.debug.assert(old_piece != null);
            self.data.setPieceAt(move.to, piece);
        } else {
            const piece = self.data.popPieceAt(move.from).?;
            const old_piece = self.data.popPieceAt(move.to);

            std.debug.assert((old_piece != null) == move.is_capture);

            const is_pawn = piece == .b_pawn or piece == .w_pawn;

            if (is_pawn and move.isLooksLikePawn2SquareMove()) {
                self.data.en_passant_target = @intCast((@as(u7, move.from) + @as(u7, move.to)) >> 1);
            }

            reset_50_moves_clock = is_pawn or move.is_capture;

            self.data.setPieceAt(move.to, piece);
        }

        self.data.castling_rights.updateAfterMove(move);

        self.movegen.setDirty();
        self.movegen.computeState(&self.data);

        const is_opponent_king_in_check = self.movegen.isKingInCheck();
        // illegal move
        if (is_opponent_king_in_check) {
            self.unmakeMove();
            return true;
        }

        if (self.data.side_to_move == .black) {
            self.data.halfmoves50 += 1;
        }

        if (reset_50_moves_clock) {
            self.data.halfmoves50 = 0;
        }

        self.data.side_to_move.flip();
        std.debug.assert(!self.movegen.is_dirty);
        self.movegen.sideFlipState();
        self.data.is_in_check = self.movegen.isKingInCheck();
        // TODO don't compute from scratch each single time
        self.data.recomputeZobristKey();

        return false;
    }

    pub fn unmakeMove(self: *Self) void {
        self.movegen.setDirty();
        self.data = self.history.pop().?.data;
    }

    pub fn repetitionsCount(self: *const Self) u32 {
        const key = self.data.zobrist_key;
        var repetitions: u32 = 0;
        for (self.history.items) |entry| {
            if (entry.data.zobrist_key == key) {
                repetitions += 1;
            }
        }
        return repetitions;
    }


    pub fn isDraw50Moves(self: *const Self) bool {
        return self.data.halfmoves50 >= 50;
    }

    pub fn setBoardData(self: *Self, bd: BoardData) void {
        self.clearHistory();
        self.data = bd;
        self.data.recomputeZobristKey();
        self.movegen.setDirty();
        self.movegen.computeState(&self.data);
        self.data.is_in_check = self.movegen.isKingInCheck();
    }
    
    pub fn clearHistory(self: *Self) void {
        self.history.clearRetainingCapacity();
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
    if (s.len <= 8) return error.IncompleteFenData;

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
    const halfmoves = std.fmt.parseUnsigned(u8, s[h50..i], 10) catch return error.IncompleteFenData;
    board.halfmoves50 = halfmoves;

    while (i < s.len and std.ascii.isWhitespace(s[i])) i += 1;
    if (i >= s.len) return error.IncompleteFenData;

    const h = i;
    while (i < s.len and !std.ascii.isWhitespace(s[i])) i += 1;
    board.fullmoves = std.fmt.parseUnsigned(u12, s[h..i], 10) catch return error.IncompleteFenData;

    while (i < s.len and std.ascii.isWhitespace(s[i])) i += 1;

    if (i != s.len) return error.InvalidFenCharacter;

    // TODO
    // board.is_in_check = board.computeIsInCheck();
    // board.recomputeZobristKey();

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

    const v = std.fmt.bufPrint(buffer[i..], " {s} {d} {d}", .{ en_passant, board.halfmoves50, board.fullmoves }) catch unreachable;
    i += v.len;

    return buffer[0..i];
}

const zobrist_hash = struct {
    const XorshiftRng = struct {
        state: u64,

        fn next(self: *XorshiftRng) u64 {
            var x = self.state;

            x ^= x << 13;
            x ^= x >> 17;
            x ^= x << 5;

            self.state = x;
            return x;
        }
    };

    fn generateVectors(seed: u64, comptime size: usize) [size]u64 {
        // @setEvalBranchQuota(1000000);
        @setEvalBranchQuota(10000);

        var rng: XorshiftRng = .{ .state = seed };
        // var rng: std.Random.DefaultPrng = .init(seed);
        var ret: [size]u64 = undefined;
        for (&ret) |*v| {
            const a = rng.next();
            const b = rng.next();
            const c = rng.next();
            v.* = a & b & c;
            std.debug.assert(v.* != 0);
        }
        
        // for (0..ret.len-1) |i| {
        //     for (i+1..ret.len) |j| {
        //         std.debug.assert(ret[i] != ret[j]);
        //     }
        // }

        return ret;
    }

    const SEED = 4242;
    const PIECES_COUNT = 12 * 64;
    const ZOBRIST_VECTORS = generateVectors(SEED, PIECES_COUNT + 1 + 24);

    const PIECE_SQUARES: [PIECES_COUNT]u64 = ZOBRIST_VECTORS[0..PIECES_COUNT].*;
    const CASTLING_RIGHTS: [16]u64 = ZOBRIST_VECTORS[PIECES_COUNT..PIECES_COUNT+16].*;
    const EP_FILE: [8]u64 = ZOBRIST_VECTORS[PIECES_COUNT+16..PIECES_COUNT+24].*;
    const SIDE_TO_MOVE: u64 = ZOBRIST_VECTORS[PIECES_COUNT+24];
};


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

    pub const NULL: Self = @bitCast(@as(u16, 0));
    // pub const NULL: Self = .{ .from = 0, .to = 0, .is_promotion = false, .is_capture = false, .extra = .{ .quiet = .none }};

    pub fn isLooksLikePawn2SquareMove(self: Self) bool {
        const move_mask = @as(u64, 1) << self.to | @as(u64, 1) << self.from;
        const PAWN_LINES_MASK_W = rankMask(1) | rankMask(3);
        const PAWN_LINES_MASK_B = rankMask(4) | rankMask(6);
        return PAWN_LINES_MASK_W & move_mask == move_mask or PAWN_LINES_MASK_B & move_mask == move_mask;
    }
    
    pub fn algebraicNotation(self: Self) MoveStrBuf {
        if (self == NULL) return .{ .buf = "null ".* };

        var ret: [5]u8 = undefined;

        ret[0..2].* = SQUARE_TO_STRING[self.from];
        ret[2..4].* = SQUARE_TO_STRING[self.to];
        ret[4] = if (self.is_promotion) switch (self.extra.promotion) {
            .knight => 'n',
            .bishop => 'b',
            .rook   => 'r',
            .queen  => 'q',
        } else ' ';

        return .{ .buf = ret };
    }

    pub fn sideFlip(self: *Self) void {
        self.from = sideFlipSquare(self.from);
        self.to = sideFlipSquare(self.to);
    }
};

pub const MAX_MOVES = 220;

pub const MoveList = struct {
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

    pub fn moves(self: *Self) []Move {
        return self.p[0..self.i];
    }

    pub fn filterCapturesOnly(self: *Self) void {
        var i: usize = 0;
        for (0..self.i) |j| {
            if (self.p[j].is_capture) {
                self.p[i] = self.p[j];
                i += 1;
            }
        }
        self.i = i;
    }
};

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

// "on which square black pawn should stand to attack this square?"
fn blackPawnAttacksRev(piece_pos: Square) u64 {
    const piece = @as(u64, 1) << piece_pos;
    const down_raw = bitboardUp(piece);
    const captures = bitboardLeft(down_raw) | bitboardRight(down_raw);
    return captures;
}

fn knightAttacksSlow(piece_pos: Square) u64 {
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

fn bishopAttacksSlow(piece_pos: Square, blockers: u64) u64 {
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

        pos_a &= ~blockers;
        pos_b &= ~blockers;
        pos_c &= ~blockers;
        pos_d &= ~blockers;
    }
    return moves;
}

fn rookAttacksSlow(piece_pos: Square, blockers: u64) u64 {
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

        pos_u &= ~blockers;
        pos_d &= ~blockers;
        pos_l &= ~blockers;
        pos_r &= ~blockers;
    }
    return moves;
}

fn queenAttacksSlow(piece_pos: Square, blockers: u64) u64 {
    return rookAttacksSlow(piece_pos, blockers) | bishopAttacksSlow(piece_pos, blockers);
}

fn kingAttacksSlow(piece_pos: Square) u64 {
    const pos: u64 = @as(u64, 1) << piece_pos;
    const vertical = bitboardUp(pos) | pos | bitboardDown(pos);
    const moves = bitboardLeft(vertical) | vertical | bitboardRight(vertical);
    return moves;
}
