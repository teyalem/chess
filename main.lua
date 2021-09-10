GAME = {
    name = "LőChess",
    author = "Hyeonung Baek (teyalem)",
    version = 0.9
}

-- print debug infos?
DEBUG = true

-- Goals: (not being done this way)
-- - [x] draw board
-- - [x] move pieces
-- - [x] turn
-- - [x] pieces' movements
--   + [x] calculate possible moves
--   + [x] display possible moves
--   + [x] restrict move
-- - [x] check and checkmate
--   + [x] generate attack map
--   + [x] determine check & checkmate
-- - [x] restrict moves
--   + [x] check
--   + [x] absolute pins
-- - [x] save & print move log
-- - [x] special rules
--   + [x] refactor functions to return moves (instead of positions)
--   + [x] castling
--   + [x] en passant
--   + [x] promotion
--     - [x] implement piece selection window
-- - [x] win prompt
-- - [x] more well encoded move and better move generator
--   + [x] wipe out duplicated codes
--   + [x] implement better move generator
-- - [x] refactor chess:domove and chess:is_legal
-- - [x] pieces' sprites

-- Future Goals:
-- - [ ] UI rework
--   + [ ] screen size
--   + [ ] window (checkmate/promotion)
--   + [ ] menus
--   + [ ] resizing
-- - [ ] map rotation
-- - [ ] stalemate
-- - [ ] AI (maybe)
-- - [ ] packaging

-- DONE: refactor functions to use MOVE_* enum.
-- DONE: feature - load chess board from file (args or drag-and-drop)
-- DONE: add Box module for boards, attack maps, etc.
-- DONE: add sprite

-- Naming Conventions --
-- 1. Global const variables are in CONSTANT_CASE.
-- 2. Module names are in CamelCase.
-- 3. function names are in snake_case.

-- List Functions --
-- A list is a table that has only integer indexes and iteratable with
-- ipairs.

-- init n f is {f(0), f(1), f(2), ..., f(n-1)}.
function init(n, f)
    local xs = {}
    for i = 0, n-1 do
        xs[#xs+1] = f(i)
    end
    return xs
end

-- apply function f to all elements of array xs, returning a new array.
function map(f, xs)
    local out = {}
    for i, v in ipairs(xs) do
        out[i] = f(v)
    end
    return out
end

-- filter elements.
function filter(f, arr)
    local out = {}
    for _, v in ipairs(arr) do
        if f(v) then out[#out+1] = v end
    end
    return out
end

-- test xs has x.
function mem(x, xs)
    for _, v in ipairs(xs) do
        if v == x then return true end
    end
    return false
end

-- return first element that satisfies f.
function find(f, xs)
    for _, v in ipairs(xs) do
        if f(v) then return v end
    end
    return nil
end

-- append contents of bs to as.
-- return as after appending
function append(as, bs)
    for _, v in ipairs(bs) do
        as[#as+1] = v
    end
    return as
end

function exists(f, xs)
    for _, v in ipairs(xs) do
        if f(v) then return true end
    end
    return false
end

function for_all(f, xs)
    for _, v in ipairs(xs) do
        if not f(v) then return false end
    end
    return true
end

function reverse_table(t)
    local o = {}
    for k, v in pairs(t) do
        o[v] = k
    end
    return o
end

-- creates a new empty table. use with init(f).
function empty(_)
    -- Table Trick. {} returns same table every time, but {nil} returns
    -- a new table when called.
    return {nil}
end

-- Utility functions --

-- print debug
function log(...)
    if DEBUG then print(...) end
end

function is_uppercase(c)
    return 'A' <= c and c <= 'Z'
end

function is_digit(c)
    return '0' <= c and c <= '9'
end

-- CONSTS --

-- Color: 1 is white, 2 is black
WHITE = 1
BLACK = 2
COLOR_NAME = { "white", "black" }

-- Piece: 0 is none, 1 is pawn, 2 is knight, 3 is bishop, 4 is rook, 5 is queen, 6 is king
NONE = 0
PAWN = 1
KNIGHT = 2
BISHOP = 3
ROOK = 4
QUEEN = 5
KING = 6

-- Piece types (for algebraic notation) and names
PTYPE = { 'P', 'N', 'B', 'R', 'Q', 'K' }
PTYPE_REV = reverse_table(PTYPE)
PNAME = { 'pawn', 'knight', 'bishop', 'rook', 'queen', 'king' }

-- Files --
A = 1
B = 2
C = 3
D = 4
E = 5
F = 6
G = 7
H = 8
FILE = { 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h' }
FILE_REV = reverse_table(FILE)

-- 8 1 2
-- 7 0 3
-- 6 5 4 Ray Directions --
RD = { [0] = 0, 1, 1, 0, -1, -1, -1, 0, 1 } -- row direction
FD = { [0] = 0, 0, 1, 1, 1, 0, -1, -1, -1 } -- file direction

-- order: white, black
OPPONENT = { BLACK, WHITE } -- opponent[p] player
BACKRANK = { 1, 8 } -- backrank[p] of player p
HOMERANK = { 2, 7 }
EP_RANK = { 6, 3 }
PAWN_DIR = { 1, 5 } -- pawns' marching direction

-- Sliding pieces' directions
BISHOP_DIR = init(4, function (n) return 2*n + 2 end) -- diagonals
ROOK_DIR = init(4, function (n) return 2*n + 1 end) -- horizontal and vertical
QUEEN_DIR = init(8, function (n) return n + 1 end)

-- Move Encodings --
MOVE_PUSH = 1 -- pawn push
MOVE_PIECE = 2 -- piece move (quiet or capture)
MOVE_CASTLE = 3 -- castling
MOVE_ENPASSANT = 4 -- en passant
MOVE_PROMOTION = 5 -- promotion

-- queenside and kingside
QUEENSIDE = 1 -- a to d
KINGSIDE = 2 -- e to h

-- Castling Positions --

KING_FILE = E

ROOK_FILE = { -- Initial file of rook
    [QUEENSIDE] = A,
    [KINGSIDE] = H,
}

CASTLE_FILE = { -- fileside -> file * file
    [QUEENSIDE] = {C, D}, -- king, rook
    [KINGSIDE] = {G, F}
}

-- Board --
-- board is array of Pieces, indexed like this:
-- X  a  b  c  d  e  f  g  h
-- 8  1  2  3  4  5  6  7  8
-- 7  9 10 11 12 13 14 15 16
-- 6 17 18 19 20 21 22 23 24
-- 5 25 26 27 28 29 30 31 32
-- 4 33 34 35 36 37 38 39 40
-- 3 41 42 43 44 45 46 47 48
-- 2 49 50 51 52 53 54 55 56
-- 1 57 58 59 60 61 62 63 64
-- (can be changed)

-- Board at turn 0 --

-- pieces
BCLASS = {
    4, 2, 3, 5, 6, 3, 2, 4,
    1, 1, 1, 1, 1, 1, 1, 1,
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,
    1, 1, 1, 1, 1, 1, 1, 1,
    4, 2, 3, 5, 6, 3, 2, 4,
}

-- colors of pieces
BCOLOR = {
    2, 2, 2, 2, 2, 2, 2, 2,
    2, 2, 2, 2, 2, 2, 2, 2,
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,
    1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1,
}

-- GUI --

SQUARE_SIZE = 50 -- size of each square

-- Selection Settings --

SEL_COLOR = {1, .3, .3}
SEL_RADIUS = SQUARE_SIZE/2 - 4
PROMOTION_COLOR = {.5, .5, .5}
PROMOTION_PIECES = { KNIGHT, BISHOP, ROOK, QUEEN } -- piece to promote to

-- Sprites --

SPRITE_SIZE = 32
SPRITE_POS = (SQUARE_SIZE - SPRITE_SIZE) / 2
SPRITES = { [WHITE] = {}, [BLACK] = {} }

-- load sprites
for color, cname in ipairs(COLOR_NAME) do
    for i, v in ipairs(PNAME) do
        local path = string.format("assets/%s_%s.png", v, cname)
        SPRITES[color][i] = love.graphics.newImage(path)
    end
end

-- Position Module --

Pos = {}

function Pos.is_pos(p)
    return type(p) == "table" and p._t == 'pos'
end

function Pos.rank(p)
    assert(Pos.is_pos(p), string.format("invalid type %s", type(p)))
    return p.rank
end

function Pos.file(p)
    assert(Pos.is_pos(p), string.format("invalid type %s", type(p)))
    return p.file
end

function Pos.equals(a, b)
    return Pos.rank(a) == Pos.rank(b) and Pos.file(a) == Pos.file(b)
end

Pos.mt = { __eq = Pos.equals }

function Pos.make(f, r)
    local p = { _t = 'pos', file = f, rank = r }
    setmetatable(p, Pos.mt)
    return p
end

function Pos.advance(p, f, r)
    return Pos.make(Pos.file(p) + f, Pos.rank(p) + r)
end

function Pos.slide(p, d, i)
    return Pos.advance(p, i*FD[d], i*RD[d])
end

function Pos.in_bound(p)
    local r = Pos.rank(p)
    local f = Pos.file(p)
    return (1 <= r and r <= 8) and (A <= f and f <= H)
end

function Pos.fileside(p)
    local f = Pos.file(p)
    if A <= f and f <= D then
        return QUEENSIDE
    else -- e to h
        return KINGSIDE
    end
end

function Pos.to_string(p)
    return string.format("%s%d", FILE[Pos.file(p)], Pos.rank(p))
end

-- test AC = nBC.
function Pos.is_lined(a, b, c)
    local function sub(a, b)
        return Pos.make(
            Pos.file(a) - Pos.file(b),
            Pos.rank(a) - Pos.rank(b))
    end

    local function dot(a, b)
        return Pos.file(a) * Pos.file(b) + Pos.rank(a) * Pos.rank(b)
    end

    local function l(a)
        return math.sqrt(dot(a, a))
    end

    local ca = sub(a, c)
    local cb = sub(b, c)

    return dot(ca, cb) == l(ca) * l(cb)
end

-- position to table index
function index(pos)
    return 8 * (8-Pos.rank(pos)) + Pos.file(pos)
end

-- table index to position
function pos(index)
    local f = (index-1) % 8 + 1
    local r = 8 - math.floor((index-1) / 8)
    return Pos.make(f, r)
end

-- Piece --

Piece = {}

function Piece.make(c, t)
    return {
        color = c,
        t = t,
    }
end

-- nothing
Piece.none = Piece.make(0, NONE)

function Piece.color(p)
    return p.color
end

function Piece.type(p)
    return p.t
end

function Piece.to_string(p)
    if Piece.type(p) == NONE then
        return ""
    else
        return string.format("%s %s",
            COLOR_NAME[Piece.color(p)],
            PNAME[Piece.type(p)])
    end
end

-- Move Module --
Move = {}

function Move.pawn(p, s, d)
    return {
        t = MOVE_PUSH,
        piece = p,
        src = s,
        distance = d,
    }
end

function Move.normal(p, s, d, x)
    return {
        t = MOVE_PIECE,
        piece = p,
        src = s,
        dst = d,
        captures = x,
    }
end

function Move.castle(color, side)
    return {
        t = MOVE_CASTLE,
        color = color,
        side = side,
    }
end

function Move.enpassant(p, src, cap, dst)
    return {
        t = MOVE_ENPASSANT,
        piece = p,
        src = src,
        cap = cap,
        dst = dst
    }
end

-- NOTE: not used in this version
function Move.promotion(p, src, dst, x, pro)
    return {
        t = MOVE_PROMOTION,
        piece = p,
        src = src,
        dst = dst,
        captures = x,
        promotion = pro,
    }
end

function Move.to_promotion(m, p)
    assert(m.t == MOVE_PIECE)
    m.t = MOVE_PROMOTION
    m.captures = true
    m.promotion = p
    return m
end

function Move.color(m)
    local color = m.piece ~= nil and Piece.color(m.piece) or m.color
    assert(color ~= nil)
    return color
end

function Move.dst(m)
    if m.t == MOVE_PUSH then
        local color = Piece.color(m.piece)
        return Pos.slide(m.src, PAWN_DIR[color], m.distance)
    elseif m.t == MOVE_PIECE
        or m.t == MOVE_PROMOTION
        or m.t == MOVE_ENPASSANT then
        return m.dst
    elseif m.t == MOVE_CASTLE then
        return Pos.make(CASTLE_FILE[m.side][1], BACKRANK[m.color])
    end
    error("invalid move type")
end

-- returns the position to press in order to do the move m.
function Move.sel(m)
    if m.t == MOVE_PUSH then
        local color = Piece.color(m.piece)
        return Pos.slide(m.src, PAWN_DIR[color], m.distance)
    elseif m.t == MOVE_PIECE
        or m.t == MOVE_PROMOTION
        or m.t == MOVE_ENPASSANT then
        return m.dst
    elseif m.t == MOVE_CASTLE then
        return Pos.make(ROOK_FILE[m.side], BACKRANK[m.color])
    end
    error("invalid move type")
end

-- to_algebraic returns the Algebraic Notation of m.
-- TODO: more accurate algebraic notation (need to check board)
function Move.to_algebraic(m)
    local function format_move(p, src, dst, x, promotion)
        local x = x and 'x' or ''
        local d = Pos.to_string(m.dst)

        local class = Piece.type(p)
        local s = PTYPE[class]
        if class == PAWN or promotion then
            s = captured and FILE[Pos.file(src)] or ""
        end

        local pro = ""
        if promotion then
            pro = '=' .. PTYPE[Piece.type(p)]
        end

        return string.format("%s%s%s%s", s, x, d, pro)
    end

    if m.t == MOVE_PUSH then
        local color = Piece.color(m.piece)
        local dst = Pos.slide(m.src, PAWN_DIR[color], m.distance)
        return Pos.to_string(dst)
    elseif m.t == MOVE_PIECE then
        return format_move(m.piece, m.src, m.dst, m.captures)
    elseif m.t == MOVE_CASTLE then
        return ({"O-O-O", "O-O"})[m.side]
    elseif m.t == MOVE_ENPASSANT then
        return format_move(m.piece, m.src, m.dst, true)
    elseif m.t == MOVE_PROMOTION then
        return format_move(m.piece, m.src, m.dst, m.captures)
    end

    return ""
end

-- 8x8 Box --

Box = {}

Box.mt = {
    __index = function (b, pos)
        if Pos.is_pos(pos) then
            return rawget(b, index(pos))
        else
            error("not a valid index")
        end
    end,

    __newindex = function(b, pos, v)
        if Pos.is_pos(pos) then
            rawset(b, index(pos), v)
        else
            error("not a valid index")
        end
    end
}

function Box.make(default)
    local b = {}
    for i = 1, 64 do b[i] = default end
    setmetatable(b, Box.mt)
    return b
end

function Box.init(f)
    local b = {}
    for i = 1, 64 do rawset(b, i, f(i)) end
    setmetatable(b, Box.mt)
    return b
end

-- Chess --

chess = {}

-- reset states
function chess:reset()
    -- set initial turn and side
    self.turn = 1; self.side = WHITE

    -- board and position of king
    local function make_board(colors, pieces)
        local b = Box.make(Piece.none)
        for i, s in ipairs(colors) do
            rawset(b, i, Piece.make(s, pieces[i]))
        end
        return b
    end

    self.board = Box.init(function (i)
        return Piece.make(BCOLOR[i], BCLASS[i])
    end)
    self.king_pos = { Pos.make(E, 1), Pos.make(E, 8) }

    -- rights
    self.castling_right = { {true, true}, {true, true} }
    self.dpush_file = {0, 0} -- double push file

    self.captured = {} -- captured pieces
    self.log = {} -- move log

    -- initialize attack map of opponent (empty for now)
    self.attacked = { Box.init(empty), Box.init(empty) }
end

-- end current side's turn
function chess:end_turn()
    -- update attack map
    self.attacked[BLACK] = self:attack_map(WHITE)
    self.attacked[WHITE] = self:attack_map(BLACK)

    self.dpush_file[OPPONENT[self.side]] = 0 -- clear en passant right

    self.side = OPPONENT[self.side]
    if self.side == WHITE then -- turn went back to white
        self.turn = self.turn + 1
    end

    for _, color in ipairs{WHITE, BLACK} do
        if chess:is_checked(color) then
            log(COLOR_NAME[color], "is checked")
        end

        if chess:is_checkmated(color) then
            self.winner = OPPONENT[color]
        end
    end
end

function chess:get_piece(pos)
    assert(Pos.in_bound(pos))
    return self.board[pos]
end

function chess:get_color(pos)
    local p = self:get_piece(pos)
    return Piece.color(p)
end

function chess:is_empty(pos)
    local p = self:get_piece(pos)
    return Piece.type(p) == NONE
end

function chess:can_capture(pos, color)
    local p = self:get_piece(pos)
    return Piece.color(p) == OPPONENT[color]
end

-- chess:collect_squares(color, ptypes) collects squares that is
-- occupied by pieces that is color and one of ptypes.
function chess:collect_squares(color, ptypes)
    local out = {}

    for i, p in ipairs(self.board) do
        if Piece.color(p) == color and mem(Piece.type(p), ptypes) then
            out[#out+1] = pos(i)
        end
    end

    return out
end

-- chess:is_reachable(src, dst, dir, ignores) returns true if a ray directing
-- dir from src can reach to dst ignoring pieces in ignores.
function chess:is_reachable(src, dst, dir, ignores)
    for i = 1, 7 do
        local sq = Pos.slide(src, dir, i)
        if not Pos.in_bound(sq) then break end

        if sq == dst then
            return true
        elseif not (self:is_empty(sq) or mem(sq, ignores)) then
            break
        end
    end

    return false
end

-- chess:find_raydir(p, src, dst, ignores) finds ray direction that can go from
-- src to dst ignoring pieces in ignores. If such a ray is not found, it
-- returns nil.
function chess:find_raydir(src, dst, ignores)
    local t = { {}, {}, BISHOP_DIR, ROOK_DIR, QUEEN_DIR, {} }
    ignores = ignores or {}
    local p = self:get_piece(src)

    for _, d in ipairs(t[Piece.type(p)]) do
        if self:is_reachable(src, dst, d, ignores) then
            return d
        end
    end

    return nil
end

-- chess:is_pinned_by(color, pos, atk, ep) returns true if a piece of color
-- at pos is absolutely pinned by attacking piece at atk.
function chess:is_pinned_by(color, pos, atk, ignores)
    ignores = ignores or {}
    local kpos = self.king_pos[color]

    if pos == kpos then -- king cannot be pinned!
        return false
    else
        local d = self:find_raydir(atk, pos, ignores)

        -- LuaJit (Lua 5.1) uses unpack(), Lua 5.4 uses table.unpack()
        return d ~= nil
        and self:is_reachable(atk, kpos, d, {pos, unpack(ignores)})
    end
end

function chess:is_pinned(src, ignores, atks)
    local color = Piece.color(self:get_piece(src))

    return exists(
        function (atk)
            return self:is_pinned_by(color, src, atk, ignores)
        end,
        atks or self.attacked[color][src])
end

function chess:is_checked(color)
    return #self.attacked[color][self.king_pos[color]] > 0
end

function chess:is_pseudo_legal(m)
    local color = Move.color(m)

    -- check the piece can move to dst.
    local function can_moveto(dst)
        return self:is_empty(dst) or self:can_capture(dst, color)
    end

    if m.t == MOVE_PUSH then
        if m.distance == 2 and Pos.rank(m.src) ~= HOMERANK[color] then
            return false
        end

        for i = 1, m.distance do
            local sq = Pos.slide(m.src, PAWN_DIR[color], i)
            if not self:is_empty(sq) then return false end
        end
        return true

    elseif m.t == MOVE_PIECE or m.t == MOVE_PROMOTION then
        if Piece.type(m.piece) == PAWN then -- pawn's MOVE_PIECE is always capture
            return self:can_capture(m.dst, color)
        else
            return can_moveto(m.dst)
        end

    elseif m.t == MOVE_CASTLE then
        -- castling right
        if self.castling_right[color][m.side] == false then
            return false
        end

        return true

    elseif m.t == MOVE_ENPASSANT then
        return Pos.rank(m.src) == EP_RANK[color]
        and Pos.file(m.cap) == self.dpush_file[OPPONENT[color]]
        and can_moveto(m.dst)
    end

    error("Invalid move")
end

-- chess:is_legal(move) returns true if the move is legal.
-- FIXME: chess:find_raydir function can be replaced to cheapper
-- chess:comp_raydir function.
function chess:is_legal(m)
    -- legal is a subset of pseudo legal.
    if not self:is_pseudo_legal(m) then return false end

    local color = Move.color(m)
    local kpos = self.king_pos[color]

    -- uncheck by blocking
    local function blocks_ray(dst)
        return exists(
            function (atk)
                return Pos.is_lined(kpos, dst, atk)
            end,
            self.attacked[color][kpos])
    end

    -- uncheck by capture
    local function captures(pos)
        return exists(
            function (atk)
                return pos == atk and self:can_capture(pos, color)
            end,
            self.attacked[color][kpos])
    end

    if m.t == MOVE_PUSH or m.t == MOVE_PIECE or m.t == MOVE_PROMOTION then
        local dst = Move.dst(m)

        -- special checks for king. Because kings are special.
        if Piece.type(m.piece) == KING then
            local defmap = self:defend_map(OPPONENT[color])
            -- NOTE: could just use blocks_ray?
            local in_ray = exists( -- opponent is reachable after moving?
                function (atk)
                    local d = self:find_raydir(atk, kpos)
                    return d ~= nil and self:is_reachable(atk, m.dst, d, {kpos})
                end,
                self.attacked[color][kpos])

            return not (defmap[dst] or in_ray)

        else -- other pieces
            if blocks_ray(dst) then
                return true
            elseif self:is_checked(color) then
                return captures(dst)
            else
                return not self:is_pinned(m.src)
            end
        end

    elseif m.t == MOVE_CASTLE then
        if self:is_checked(color) then return false end

        -- scan passing squares to check the way is safe and not blocked
        local d = ({-1, 1})[m.side]
        for f = KING_FILE + d, CASTLE_FILE[m.side][1], d do
            local sq = Pos.make(f, BACKRANK[color])
            if not self:is_empty(sq) or #self.attacked[color][sq] > 0 then
                return false
            end
        end
        return true

    elseif m.t == MOVE_ENPASSANT then
        if self:is_checked(color) then
            return blocks_ray(m.dst) or captures(m.cap)
        else
            -- collect opponent rooks and queens to check horizontal
            -- pinning of same rank piece.
            local atks = self:collect_squares(OPPONENT[color], {ROOK, QUEEN})
            return not self:is_pinned(m.src, {m.cap}, atks)
        end
    end

    error("Invalid Move") -- the move must be one of above
end

function chess:is_checkmated(color)
    local kpos = self.king_pos[color]
    local atks = self.attacked[color][kpos]

    if #atks == 0 then
        return false

    elseif #atks == 1 then -- single check; check there's no legal move
        for i, p in ipairs(self.board) do
            if Piece.color(p) == color then
                local ms = self:legal_move(pos(i))
                if #ms > 0 then return false end
            end
        end
        return true

    elseif #atks >= 2 then -- double check; check escape square
        local km = self:legal_move(kpos)
        return #km == 0
    end
end

-- domove(m) performs the move m. It assumes the input move m is legal.
function chess:domove(m)
    local color = Move.color(m)

    -- quiet move
    local function move(p, src, dst)
        self.board[dst] = p
        self.board[src] = Piece.none
    end

    -- capture piece at pos
    local function capture(p, pos)
        self.captured[#self.captured+1] = p
        self.board[pos] = Piece.none
    end

    local function update_state(p, src, dst)
        local pt = Piece.type(p)

        if pt == KING then
            self.king_pos[color] = dst
            self.castling_right[color] = {false, false}

        elseif pt == ROOK then
            local i = Pos.fileside(src)
            local pos = Pos.make(ROOK_FILE[i], BACKRANK[color])
            if self.castling_right[color][i] and src == pos then
                self.castling_right[color][i] = false
            end
        end
    end

    if m.t == MOVE_PUSH then
        local dst = Pos.slide(m.src, PAWN_DIR[color], m.distance)

        move(m.piece, m.src, dst)
        if m.distance == 2 then -- double push
            self.dpush_file[color] = Pos.file(m.src)
        end

    elseif m.t == MOVE_PIECE then
        if m.captures then capture(m.piece, m.dst) end
        update_state(m.piece, m.src, m.dst)
        move(m.piece, m.src, m.dst)

    elseif m.t == MOVE_CASTLE then
        -- uses hardcoded data
        local function f(f) return Pos.make(f, BACKRANK[color]) end
        local kpos = f(KING_FILE)
        local rpos = f(ROOK_FILE[m.side])
        local np = CASTLE_FILE[m.side]
        local nkpos = f(np[1])
        local nrpos = f(np[2])
        local k = self:get_piece(kpos)
        local r = self:get_piece(rpos)

        update_state(k, kpos, nkpos) -- remove castling right
        move(k, kpos, nkpos)
        move(r, rpos, nrpos)

    elseif m.t == MOVE_ENPASSANT then
        local p = self:get_piece(m.cap)
        capture(p, m.cap)
        move(m.piece, m.src, m.dst)

    elseif m.t == MOVE_PROMOTION then
        if m.captures then capture(m.dst) end
        move(m.piece, m.src, m.dst)
        m.p.t = m.promotion -- FIXME: abstraction needed
    end
end

-- Move Generating Functions --
-- move function = Piece.t -> Pos.t -> Move.t list

-- cast a ray to direction dir and save moves to ms
-- Move.t array -> Piece.t -> Pos.t -> int -> unit
function chess:raycast(ms, p, src, dir)
    local color = Piece.color(p)

    for i = 1, 7 do
        local sq = Pos.slide(src, dir, i)
        if not Pos.in_bound(sq) then break end

        local m = Move.normal(p, src, sq, self:can_capture(sq, color))
        ms[#ms+1] = m

        if not self:is_empty(sq) then
            break -- ray blocked; stop casting ray
        end
    end
end

function chess:pawn_attack(p, src)
    local ms = {}
    local color = Piece.color(p)

    for _, fd in ipairs{-1, 1} do
        local dst = Pos.advance(src, fd, RD[PAWN_DIR[color]])

        if Pos.in_bound(dst) then
            -- move by capture
            ms[#ms+1] = Move.normal(p, src, dst, true)

            -- en passant
            local cap = Pos.make(Pos.file(dst), Pos.rank(src))
            ms[#ms+1] = Move.enpassant(p, src, cap, dst)
        end
    end

    return ms
end

function chess:pawn_move(p, src)
    -- pawn push
    local ms = {
        Move.pawn(p, src, 1),
        Move.pawn(p, src, 2)
    }

    return append(ms, self:pawn_attack(p, src))
end

function chess:knight_move(p, src)
    local color = Piece.color(p)
    local ms = {}

    local function add(dst)
        if Pos.in_bound(dst) then
            ms[#ms+1] = Move.normal(p, src, dst, self:can_capture(dst, color))
        end
    end

    for _, i in ipairs(BISHOP_DIR) do
        add(Pos.advance(src, 2*FD[i], RD[i]))
        add(Pos.advance(src, FD[i], 2*RD[i]))
    end
    return ms
end

function chess:sliding_move(dirs)
    return function (chess, p, src)
        local out = {}
        for _, i in ipairs(dirs) do
            chess:raycast(out, p, src, i)
        end
        return out
    end
end

chess.bishop_move = chess:sliding_move(BISHOP_DIR)
chess.rook_move = chess:sliding_move(ROOK_DIR)
chess.queen_move = chess:sliding_move(QUEEN_DIR)

function chess:king_move(p, src)
    local ms = {}
    local color = Piece.color(p)

    for i = 1, 8 do
        local dst = Pos.advance(src, FD[i], RD[i])
        if Pos.in_bound(dst) then
            local m = Move.normal(p, src, dst, self:can_capture(dst, color))
            ms[#ms+1] = m
        end
    end

    ms[#ms+1] = Move.castle(color, KINGSIDE)
    ms[#ms+1] = Move.castle(color, QUEENSIDE)

    return ms
end

chess.moves = {
    chess.pawn_move,
    chess.knight_move,
    chess.bishop_move,
    chess.rook_move,
    chess.queen_move,
    chess.king_move
}

function chess:collect_moves(t, pos)
    assert(not self:is_empty(pos))
    local p = self:get_piece(pos)
    local ms = t[Piece.type(p)](self, p, pos)
    return filter(function (m) return self:is_legal(m) end,  ms)
end

-- legal_move returns legal moves of the piece at pos.
function chess:legal_move(pos)
    return self:collect_moves(self.moves, pos)
end

-- chess:attack_map(color) generates map whose squares have attackers'
-- positions. It is used to check if opponent king is in check or
-- absolute pinning.
function chess:attack_map(color) -- int -> Pos.t matrix
    local map = Box.init(empty)

    -- collect attacks
    for i, p in ipairs(self.board) do
        local pos = pos(i)

        if Piece.color(p) == color
            and Piece.type(p) ~= KING then -- king cannot attack
            for _, m in ipairs(self:legal_move(pos)) do
                if m.t == MOVE_PIECE or m.t == MOVE_PROMOTION then
                    table.insert(map[m.dst], pos)
                end
            end
        end
    end

    return map
end

-- Piece's defences --

function chess:pawn_def(p, pos)
    local r = RD[PAWN_DIR[Piece.color(p)]]
    local ps = map(function (f) return Pos.advance(pos, f, r) end, { -1, 1 })
    return filter(Pos.in_bound, ps)
end

function chess:knight_def(p, pos)
    local ps = {}
    for _, i in ipairs(BISHOP_DIR) do
        ps[#ps+1] = Pos.advance(pos, 2*FD[i], RD[i])
        ps[#ps+1] = Pos.advance(pos, FD[i], 2*RD[i])
    end
    return filter(Pos.in_bound, ps)
end

function chess:ray_def(ps, color, pos, dir)
    for i = 1, 7 do
        local sq = Pos.slide(pos, dir, i)
        if Pos.in_bound(sq)
            and (self:is_empty(sq) or self:get_color(sq) == color) then
            ps[#ps+1] = sq
        else
            break
        end
    end
end

function chess:sliding_def(dirs)
    return function(self, p, pos)
        local color = Piece.color(p)
        local ps = {}
        for _, d in ipairs(dirs) do
            self:ray_def(ps, color, pos, d)
        end
        return ps
    end
end

chess.bishop_def = chess:sliding_def(BISHOP_DIR)
chess.rook_def = chess:sliding_def(ROOK_DIR)
chess.queen_def = chess:sliding_def(QUEEN_DIR)

function chess:king_def(p, pos)
    local ps = {}
    for d = 1, 8 do
        ps[#ps+1] = Pos.slide(pos, d, 1)
    end
    return filter(Pos.in_bound, ps)
end

chess.defs = {
    chess.pawn_def,
    chess.knight_def,
    chess.bishop_def,
    chess.rook_def,
    chess.queen_def,
    chess.king_def
}

-- chess:defend_map generates a map that squares are marked if they are
-- defended by color's pieces. It is used to check king's escape routes.
function chess:defend_map(color)
    local map = Box.make(false)

    for i, p in ipairs(self.board) do
        local pos = pos(i)

        if Piece.color(p) == color then
            if not self:is_pinned(pos) then
                local defs = self.defs[Piece.type(p)](self, p, pos)
                for _, dst in ipairs(defs) do
                    map[dst] = true
                end
            end
        end
    end

    return map
end

function chess:take_turn(m)
    self.log[#self.log+1] = Move.to_algebraic(m) -- save log
    chess:domove(m)
    chess:end_turn()
end

-- debug: print log
function chess:print_log()
    for i = 1, #self.log, 2 do
        log(math.floor(i/2) + 1, self.log[i], self.log[i+1] or "")
    end
end

-- chess:load_fen(bstr) parses Forsyth-Edwards Notation and load it.
function chess:load_fen(fen)
    local function ppiece(c)
        local color = is_uppercase(c) and WHITE or BLACK
        local piece = PTYPE_REV[string.upper(c)]
        return Piece.make(color, piece)
    end

    local function pboard(str)
        local b = Box.make(Piece.none)
        local i = 1
        for p in string.gmatch(str, '[^/]') do
            if is_digit(p) then -- empty
                i = i + tonumber(p)
            else
                rawset(b, i, ppiece(p))
                i = i + 1
            end
        end
        assert(#b == 64)
        self.board = b
    end

    local function pside(str)
        self.side = ({ w = WHITE, b = BLACK })[str]
    end

    local function pcastle(str)
        self.castling_right = {{false, false}, {false, false}}

        for c in string.gmatch(str, '.') do
            if c == '-' then break end

            local color = is_uppercase(c) and WHITE or BLACK
            local side = ({ ['K'] = KINGSIDE, ['Q'] = QUEENSIDE })[string.upper(c)]
            assert(side ~= nil, 'invalid input')
            self.castling_right[color][side] = true
        end
    end

    local function penpassant(str)
        if str ~= "-" then
            local file, rank = string.match(str, "(%l)(%d)")
            file = FILE_REV[file]

            local color = ({ ['3'] = WHITE, ['6'] = BLACK })[rank]
            assert(color ~= nil, "invalid enpassant rank")
            self.dpush_file[color] = file
        end
    end

    local function phalfclock(str) end -- not implemented

    local function pturn(str)
        self.turn = tonumber(str)
    end

    -- parse FEN
    local t = { pboard, pside, pcastle, penpassant, phalfclock, pturn }
    local step = 1
    for part in string.gmatch(fen, "%S+") do
        t[step](part)
        step = step + 1
    end

    -- Postprocessing

    -- find kings
    for i, p in ipairs(self.board) do
        if Piece.type(p) == KING then
            self.king_pos[Piece.color(p)] = pos(i)
        end
    end

    -- find attcked squares
    self.attacked[WHITE] = self:attack_map(BLACK)
    self.attacked[BLACK] = self:attack_map(WHITE)

    for _, color in ipairs{WHITE, BLACK} do
        if chess:is_checked(color) then
            log(COLOR_NAME[color], "is checked")
        end
    end
end

-- Screen Position Functions --

-- middle point of square
function mid(n)
    return n + SQUARE_SIZE/2
end

-- file to screen x
function fx(file)
    return (file-1) * SQUARE_SIZE
end

-- screen x to file
function xf(x)
    return math.floor(x/SQUARE_SIZE) + 1
end

-- rank to screen y
function ry(rank)
    return (8-rank) * SQUARE_SIZE
end

-- screen y to rank
function yr(y)
    return 8 - math.floor(y/SQUARE_SIZE)
end

-- Game Functions --

function reset_game()
    chess:reset()
    sel:reset()
end

function love.load()
    reset_game()
    update_board()
end

function love.quit()
    chess:print_log() -- debug
end

function love.keypressed(key)
    if key == 'escape' then
        love.event.quit()
    end
end

-- click event
function love.mousepressed(x, y, button)
    if button == 1 then
        local pos = Pos.make(xf(x), yr(y))
        if Pos.in_bound(pos) then
            sel:click(pos)
            update_board()
        end
    end
end

-- board loading function
function love.filedropped(file)
    file:open('r')
    local bs, _ = file:read()
    reset_game()
    chess:load_fen(bs)
    update_board()
end

-- Click Interface --

sel = {}

function sel:reset()
    self.state = 'unsel'
    self.selected = nil
    self.moves = nil
    self.move = nil
end

function sel:select(pos)
    self.state = 'selected'
    self.selected = pos
    self.moves = chess:legal_move(pos)
end

function sel:promotion(m)
    self.state = 'promotion'
    self.move = m
end

-- Main logic of select UI
function sel:click(pos)
    if self.state == 'unsel' and chess:get_color(pos) == chess.side then
        self:select(pos)

    elseif self.state == 'selected' then
        if self.selected == pos then -- reset selection by clicking the same sqaure
            self:reset()
        else
            local m = find(function (m) return pos == Move.sel(m) end, self.moves)

            if m ~= nil then -- move
                local selp = chess:get_piece(self.selected)
                if Piece.type(selp) == PAWN
                    and Pos.rank(pos) == BACKRANK[OPPONENT[chess.side]] then -- promotion
                    self:promotion(Move.sel(m))
                    return
                end

                chess:take_turn(m)
                self:reset()
            end

            if not moved and chess:get_color(pos) == chess.side then -- reselect
                self:select(pos)
            end
        end

    elseif self.state == 'promotion' then
        local i = Pos.file(pos)
        local m
        if Pos.rank(pos) == 5 and 2 <= i and i <= 6 then
            local piece = self.pieces[i-2]
            m = Move.to_promotion(self.move, piece)
        else
            m = self.move -- move without promotion
        end

        chess:take_turn(m)
        self:reset()
    end
end

-- Drawing Functions --

local G = love.graphics

canvas = G.newCanvas()

function update_board()
    canvas:renderTo(function ()
        chess:draw()
        sel:draw()
    end)
end

-- draw a square of board
function draw_square(rank, file, color)
    local x = fx(file)
    local y = ry(rank)
    local s = SQUARE_SIZE

    G.setColor(.1, .1, .1)
    G.setLineWidth(5)
    G.rectangle('line', x, y, s, s)
    G.setColor(color)
    G.rectangle('fill', x, y, s, s)
end

-- draw a piece p at (x, y)
function Piece.draw(x, y, p)
    x = x + SPRITE_POS
    y = y + SPRITE_POS
    local color = Piece.color(p)
    local class = Piece.type(p)

    G.setColor(1, 1, 1)
    G.draw(SPRITES[color][class], x, y)
end

-- win message when the winner is choosen.
function chess:draw_win_prompt()
    local msg = "Checkmate!"
    local color
    if self.winner == WHITE then
        color = {1, 1, 1}
    elseif self.winner == BLACK then
        color = {0, 0, 0}
    end

    if color ~= nil then
        G.setColor(.5, .5, .5)
        G.rectangle('fill', SQUARE_SIZE, SQUARE_SIZE, 200, 100)
        G.setColor(color)
        G.print(msg, SQUARE_SIZE, SQUARE_SIZE, 0, 3, 3)
    end
end

-- draw chess board and pieces
function chess:draw()
    for rank = 1, 8 do
        for file = 1, 8 do
            -- square
            local color
            if (rank + file) % 2 == 1 then
                color = {.9, .7, .7} -- white
            else
                color = {.4, .3, .3} -- black
            end
            draw_square(rank, file, color)

            -- piece
            local p = self:get_piece(Pos.make(file, rank))
            if Piece.type(p) ~= NONE then
                Piece.draw(fx(file), ry(rank), p)
            end
        end
    end

    self:draw_win_prompt()
end

-- promotion window
function sel:draw_promotion_ui()
    G.setColor(PROMOTION_COLOR)
    G.rectangle('fill', fx(1), ry(5), 4*SQUARE_SIZE, SQUARE_SIZE)

    for f = 3, 6 do
        local p = Piece.make(chess.side, PROMOTION_PIECES[f-2])
        Piece.draw(fx(f), ry(5), p)
    end
    
end

-- draw move candidates
function sel:draw_options()
    G.setColor(SEL_COLOR)
    for _, m in ipairs(self.moves) do
        local dst = Move.sel(m)
        local x = fx(Pos.file(dst))
        local y = ry(Pos.rank(dst))

        G.circle('fill', mid(x), mid(y), SEL_RADIUS)
    end
end

function sel:draw()
    if self.selected ~= nil then
        G.setColor(SEL_COLOR)
        G.setLineWidth(5)
        G.rectangle('line',
            fx(Pos.file(self.selected)), ry(Pos.rank(self.selected)),
            SQUARE_SIZE, SQUARE_SIZE)
    end

    if self.moves ~= nil then
        self:draw_options()
    end

    if self.state == 'promotion' then
        self:draw_promotion_ui()
    end
end

function draw_mark(pos, color)
    G.setColor(color)
    local x = fx(Pos.file(pos))
    local y = ry(Pos.rank(pos))

    G.circle('fill', mid(x), mid(y), 5)
end

-- debug
function draw_attackmap(map, color)
    local c = {{ .8, .5, .5 }, {.5, .8, .5}}
    for i, ms in ipairs(map) do
        if #ms > 0 then
            draw_mark(pos(i), c[color])
        end
    end
end

function love.draw()
    G.setColor(1, 1, 1)
    G.draw(canvas, 0, 0)
end
