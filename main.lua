GAME = {
    name = "Chess",
    author = "Hyeonung Baek (teyalem)",
    version = 0.7
}

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
-- - [ ] pieces' sprites

-- Future Goals:
-- - [ ] map rotation
-- - [ ] stalemate
-- - [ ] AI (maybe)

-- DONE: refactor functions to use MOVE_* enum.
-- DONE: feature - load chess board from file (args or drag-and-drop)
-- TODO: add Box module for boards, attack maps, etc.
-- TODO: look for sensible colors for board
-- TODO: add sprite

-- Naming Conventions --
-- 1. Global const variables are in CONSTANT_CASE.
-- 2. Module names are in CamelCase.
-- 3. function names are in snake_case.

-- List Functions --
-- A list is a table that has only integer indexes and iteratable with ipairs.

-- init n f generates n-length list 
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

function is_uppercase(c)
    return 'A' <= c and c <= 'Z'
end

function is_digit(c)
    return '0' <= c and c <= '9'
end

function reverse_table(t)
    local o = {}
    for k, v in pairs(t) do
        o[v] = k
    end
    return o
end

-- CONSTS --

-- Color: 1 is white, 2 is black
WHITE = 1
BLACK = 2

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
PAWN_RD = { 1, -1 } -- pawns' marching direction

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

-- UI Settings --

SQUARE_SIZE = 50 -- size of each square

-- Click Interface --

sel = { -- Selection UI Settings
    color = {1, .3, .3}, -- selection color
    promotion_color = {.5, .5, .5}, -- promotion window color
    radius = SQUARE_SIZE/2 - 4, -- move sel radius
    pieces = { KNIGHT, BISHOP, ROOK, QUEEN } -- piece to promote to (knight, bishop, rook, queen)
}

function sel:reset()
    self.state = 'unsel' -- Sel.State.t
    self.selected = nil -- Pos.t
    self.moves = nil -- Move.t list
    self.move = nil
end

function sel:select(pos)
    self.state = 'selected'
    self.selected = pos
    self.moves = chess:legal_move(pos)
end

function sel:promotion(m)
    self.state = MOVE_PROMOTION
    self.move = m
end

-- Main logic of select UI
function sel:click(pos)
    if chess.winner ~= nil then return end

    if self.state == 'unsel' and chess:get_side(pos) == chess.side then
        self:select(pos)

    elseif self.state == 'selected' then
        if self.selected == pos then -- reset selection by clicking the same sqaure
            self:reset()
        else
            local m = find(function (m) return pos == Move.sel(m) end, self.moves)

            if m ~= nil then -- move
                if Piece.is_pawn(chess:get_piece(self.selected))
                    and Pos.rank(pos) == BACKRANK[OPPONENT[chess.side]] then -- promotion
                    self:promotion(Move.sel(m))
                    return
                end

                chess:take_turn(m)
                self:reset()
            end

            if not moved and chess:get_side(pos) == chess.side then -- reselect
                self:select(pos)
            end
        end

    elseif self.state == MOVE_PROMOTION then
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

-- Position Module --

Pos = {}

function Pos.is_pos(p)
    return type(p) == "table" and p._t == 'pos'
end

function Pos.rank(p)
    assert(Pos.is_pos(p))
    return p.rank
end

function Pos.file(p)
    assert(Pos.is_pos(p))
    return p.file
end

function Pos.equals(a, b)
    return Pos.rank(a) == Pos.rank(b) and Pos.file(a) == Pos.file(b)
end

Pos.mt = { __eq = Pos.equals }

function Pos.make(rank, file)
    local p = { _t = 'pos', rank = rank, file = file }
    setmetatable(p, Pos.mt)
    return p
end

function Pos.advance(p, r, f)
    return Pos.make(Pos.rank(p) + r, Pos.file(p) + f)
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
    else
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
            Pos.rank(a) - Pos.rank(b),
            Pos.file(a) - Pos.file(b))
    end

    local ca = sub(a, c)
    local cb = sub(b, c)

    return Pos.rank(ca) * Pos.file(cb) == Pos.rank(cb) * Pos.file(ca)
end

-- position to table index
function index(pos)
    return 8 * (8-Pos.rank(pos)) + Pos.file(pos)
end

-- table index to position
function pos(index)
    local r = 8 - math.floor((index-1) / 8)
    local f = (index-1) % 8 + 1
    return Pos.make(r, f)
end

-- Chess Piece Module --
Piece = {}

function Piece.make(side, class)
    return {
        side = side, 
        class = class,
    }
end

-- nothing
Piece.none = Piece.make(0, NONE)

function Piece.side(p)
    return p.side
end

function Piece.class(p)
    return p.class
end

-- shortcuts for common compares

-- is p none(empty)?
function Piece.is_none(p)
    return Piece.class(p) == NONE
end

-- is p a pawn?
function Piece.is_pawn(p)
    return Piece.class(p) == PAWN
end

-- is p a king?
function Piece.is_king(p)
    return Piece.class(p) == KING
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

-- returns the position to press in order to do the move m.
function Move.sel(m)
    if m.t == MOVE_PUSH then
        local side = Piece.side(m.piece)
        return Pos.advance(m.src, m.distance * PAWN_RD[side], 0)
    elseif m.t == MOVE_PIECE then
        return m.dst
    elseif m.t == MOVE_CASTLE then
        return Pos.make(BACKRANK[m.color], ROOK_FILE[m.side])
    elseif m.t == MOVE_ENPASSANT then
        return m.dst
    end
end

-- to_algebraic returns the Algebraic Notation of m.
-- TODO: more accurate algebraic notation (need to check board)
function Move.to_algebraic(m)
    local function format_move(p, src, dst, x, promotion)
        local x = x and 'x' or ''
        local d = Pos.to_string(m.dst)

        local s = PTYPE[Piece.class(p)]
        if Piece.is_pawn(p) or promotion then
            s = captured and FILE[Pos.file(src)] or ""
        end

        local pro = ""
        if promotion then
            pro = '=' .. PTYPE[Piece.class(p)]
        end

        return string.format("%s%s%s%s", s, x, d, pro)
    end

    if m.t == MOVE_PUSH then
        local dir = PAWN_RD[Piece.side(m.piece)]
        local d = Pos.rank(m.src) + m.distance * dir
        local dst = Pos.make(d, Pos.file(m.src))
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

-- Chess --

chess = {}

chess.mt = {
    __index = function (b, pos)
        if Pos.is_pos(pos) then
            return b[index(pos)]
        else
            assert(false, "not a valid index")
        end
    end,

    __newindex = function(b, pos, v)
        if Pos.is_pos(pos) then
            b[index(pos)] = v
        else
            assert(false, "not a valid index")
        end
    end
}

-- reset states
function chess:reset()
    -- set initial turn and side
    self.turn = 1; self.side = WHITE

    -- board and position of king
    local function make_board(colors, pieces)
        local b = {}
        for i, s in ipairs(colors) do
            b[i] = Piece.make(s, pieces[i])
        end
        setmetatable(b, self.mt)
        return b
    end

    self.board = make_board(BCOLOR, BCLASS)
    self.king_pos = {Pos.make(1, 5), Pos.make(8, 5)}

    -- rights
    self.castling_right = {{true, true}, {true, true}}
    self.dpush_file = {0, 0} -- double push file

    self.captured = {} -- captured pieces
    self.log = {} -- move log

    -- initialize attack map of opponent (empty for now)
    self.attacked = {{}, {}}
    for s = 1, 2 do
        for i = 1, 64 do self.attacked[s][i] = {} end
    end
end

-- end current side's turn
function chess:end_turn()
    -- update attack map
    local am = self:gen_attack(self.side)
    setmetatable(am, self.mt)
    self.attacked[OPPONENT[self.side]] = am
    self.dpush_file[OPPONENT[self.side]] = 0 -- clear en passant right

    self.side = OPPONENT[self.side]
    if self.side == 1 then
        self.turn = self.turn + 1
    end

    for _, color in ipairs{WHITE, BLACK} do
        if chess:is_checkmated(chess.side) then
            self.winner = OPPONENT[color]
        end
    end
end

function chess:get_piece(pos)
    assert(Pos.in_bound(pos))
    return self.board[pos]
end

function chess:get_side(pos)
    local p = self:get_piece(pos)
    return Piece.side(p)
end

function chess:get_class(pos)
    local p = self:get_piece(pos)
    return Piece.class(p)
end

function chess:is_empty(pos)
    local p = self:get_piece(pos)
    return Piece.is_none(p)
end

function chess:can_capture(pos, side)
    return self:get_side(pos) == OPPONENT[side]
end

-- chess:is_pinned(side, pos, atk) returns true if a piece of color side at pos
-- is absolutely pinned by attacking piece at atk, otherwise false.
-- NOTE: This function will return true if pos is the same as king's position.
function chess:is_pinned(side, pos, atk)
    local t = { {}, {}, BISHOP_DIR, ROOK_DIR, QUEEN_DIR, {} }
    local ap = self:get_piece(atk)
    
    for d in ipairs(t[Piece.class(ap)]) do
        local met = false
        for i = 1, 7 do
            local sq = Pos.advance(atk, i*RD[d], i*FD[d])
            if not Pos.in_bound(sq) then break end -- stop searching outside the board
            if not self:is_empty(sq) then -- only check occupied squares
                if sq == pos then
                    met = true
                elseif sq == self.king_pos[side] then
                    return met
                else
                    break -- blocked by other piece
                end
            end
        end
    end

    return false
end

-- test a move is pseudo-legal.
-- TODO: fill all checks
function chess:is_pseudo_legal(m)
    if m.t == MOVE_PUSH then
        local side = Piece.side(m.piece)

        for i = 1, m.distance do
            local sq = Pos.advance(m.src, i*PAWN_RD[side], 0)
            if not self:is_empty(sq) then return false end
        end
        return true

    elseif m.t == MOVE_PIECE then
        local side = Piece.side(m.piece)
        return Pos.in_bound(m.dst)
        and (self:is_empty(m.dst) or self:can_capture(m.dst, side))

    elseif m.t == MOVE_CASTLE then

    elseif m.t == MOVE_ENPASSANT then

    elseif m.t == MOVE_PROMOTION then
    end

    assert(false) -- all moves must be checked
end

function chess:is_checked(side)
    return #self.attacked[side][index(self.king_pos[side])] > 0
end

-- chess:is_legal(move) returns true if the move is legal.
function chess:is_legal(move)
    -- filter out non-legal move
    --if not self:is_pseudo_legal(move) then return false end

    -- check one can uncheck by move a piece to dst.
    local function is_uncheck(side, dst)
        local kpos = self.king_pos[side]
        local atks = self.attacked[side][index(kpos)]

        for _, atk in ipairs(atks) do
            if Pos.is_lined(atk, dst, kpos) -- blocks ray
                or self:can_capture(dst, side) then -- or captures
                return true
            end
        end
        return false
    end

    -- check piece is (absolutely) pinned.
    local function is_pinned(side, pos)
        local atks = self.attacked[side][index(pos)]
        return exists(
            function (atk) return self:is_pinned(side, pos, atk) end,
            atks)
    end

    local color = move.piece ~= nil and Piece.side(move.piece) or move.color
    assert(color ~= nil)

    if move.t == MOVE_PUSH then
        if move.distance == 2 and Pos.rank(move.src) ~= HOMERANK[color] then
            return false
        end

        for i = 1, move.distance do
            local sq = Pos.advance(move.src, i*PAWN_RD[color], 0)
            if not self:is_empty(sq) then return false end
        end

        if self:is_checked(color) then
            local dst = Pos.advance(move.src, move.distance * PAWN_RD[color], 0)
            return is_uncheck(color, dst)
        else
            return not is_pinned(color, move.src)
        end

    elseif move.t == MOVE_PIECE or move.t == MOVE_PROMOTION then
        if Pos.in_bound(move.dst)
            and (self:is_empty(move.dst) or self:can_capture(move.dst, color)) then
            if Piece.class(move.piece) == KING then -- king can move to uncontrolled square
                return #self.attacked[color][index(move.dst)] == 0
            elseif self:is_checked(color) then -- checked?
                return is_uncheck(color, move.dst)
            else -- not checked; check pinned
                return not is_pinned(color, move.src)
            end
        end
        return false

    elseif move.t == MOVE_CASTLE then
        if self.castling_right[color][move.side] == false -- no right
            or self:is_checked(color) then -- checked
            return false
        end

        -- scan passing squares to check the way is safe or not blocked
        local r = BACKRANK[color]
        local d = ({-1, 1})[move.side]

        for f = KING_FILE + d, ROOK_FILE[move.side] - d, d do
            local sq = Pos.make(r, f)
            if not self:is_empty(sq) or #self.attacked[color][index(sq)] > 0 then
                return false
            end
        end

        return true

    elseif move.t == MOVE_ENPASSANT then
        -- FIXME: need to check two pieces simultaneously (capturer and capturee)
        -- check en passant right
        if Pos.file(move.cap) ~= self.dpush_file[OPPONENT[color]] then
            return false
        end

        local kpos = self.king_pos[color]
        local checked = self:is_checked(color)

        if checked then
            return mem(self.attacked[color][kpos], move.cap)
        else
            return not is_pinned(color, move.src, move.cap)
        end
    end

    assert(false) -- the move must be one of above
end

function chess:is_checkmated(side)
    local kpos = self.king_pos[side]
    local atks = self.attacked[side][index(kpos)]

    if #atks == 0 then
        return false

    elseif #atks == 1 then -- single check; check there's no legal move
        for i, p in ipairs(self.board) do
            if Piece.side(p) == side then
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

-- domove performs the move m.
-- Assume input move is legal (which is restricted by selection).
function chess:domove(m)
    local function move(p, src, dst)
        self.board[dst] = p
        self.board[src] = Piece.none
    end

    local function capture(p, pos)
        self.captured[#self.captured+1] = p
        self.board[pos] = Piece.none
    end

    local function opt_check(p, src, dst)
        if Piece.class(p) == KING then
            local side = Piece.side(p)
            self.king_pos[side] = dst
            self.castling_right[side] = {false, false}

        elseif Piece.class(p) == ROOK then
            local i = Pos.fileside(src)
            local pos = Pos.make(BACKRANK[Piece.side(p)], ROOK_FILE[i])
            if pos == src then
                self.castling_right[side][i] = false
            end
        end
    end

    if m.t == MOVE_PUSH then
        local side = Piece.side(m.piece)
        local dst = Pos.advance(m.src, m.distance * PAWN_RD[side], 0)

        move(m.piece, m.src, dst)
        if m.distance == 2 then -- double push
            self.dpush_file[side] = Pos.file(m.src)
        end

    elseif m.t == MOVE_PIECE then
        if m.captures then capture(m.dst) end
        opt_check(m.piece, m.src)
        move(m.piece, m.src, m.dst)

    elseif m.t == MOVE_CASTLE then
        -- uses hardcoded data
        local b = BACKRANK[m.color]
        local kpos = Pos.make(b, KING_FILE)
        local rpos = Pos.make(b, ROOK_FILE[m.side])
        local np = CASTLE_FILE[m.side]
        local nkpos = Pos.make(b, np[1])
        local nrpos = Pos.make(b, np[2])
        local k = self:get_piece(kpos)
        local r = self:get_piece(rpos)

        opt_check(k, kpos, nkpos) -- remove castling right
        move(k, kpos, nkpos)
        move(r, rpos, nrpos)

    elseif m.t == MOVE_ENPASSANT then
        capture(m.cap)
        move(m.piece, m.src, m.dst)

    elseif m.t == MOVE_PROMOTION then
        if m.captures then capture(m.dst) end
        move(m.piece, m.src, m.dst)
        m.p.class = m.promotion
    end
end

-- Pseudo-legal Move Generating Functions --
-- move function = Piece.t -> Pos.t -> Move.t list

-- cast a ray to direction dir and save moves to ms
-- Move.t array -> Piece.t -> Pos.t -> int -> unit
function chess:raycast(ms, p, src, dir)
    for i = 1, 7 do
        local sq = Pos.advance(src, i*RD[dir], i*FD[dir])
        if not Pos.in_bound(sq) then break end

        local m = Move.normal(p, src, sq, not self:is_empty(sq))
        if self:is_pseudo_legal(m) then
            ms[#ms+1] = m
        end

        if not self:is_empty(sq) then
            break -- ray blocked; stop casting ray
        end
    end
end

function chess:pawn_attack(p, src)
    local ms = {}
    local side = Piece.side(p)

    for _, fd in ipairs{-1, 1} do
        local dst = Pos.advance(src, PAWN_RD[side], fd)

        if Pos.in_bound(dst) then
            -- move by capture
            if self:can_capture(dst, side) then
                ms[#ms+1] = Move.normal(p, src, dst, true)
            end

            -- en passant
            local cap = Pos.make(Pos.rank(src), Pos.file(dst))
            local cp = self:get_piece(cap)
            if self:can_capture(cap, side) then
                ms[#ms+1] = Move.enpassant(p, src, cap, dst)
            end
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
    local ps = {}

    for i = 2, 8, 2 do
        ps[#ps+1] = Pos.advance(src, RD[i], 2*FD[i])
        ps[#ps+1] = Pos.advance(src, 2*RD[i], FD[i])
    end

    local ms = {}
    for _, dst in ipairs(ps) do
        if Pos.in_bound(dst) then
            local m = Move.normal(p, src, dst, not self:is_empty(dst))
            if self:is_pseudo_legal(m) then ms[#ms+1] = m end
        end
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

    for i = 1, 8 do
        local dst = Pos.advance(src, RD[i], FD[i])
        if Pos.in_bound(dst) then
            local m = Move.normal(p, src, dst, not self:is_empty(dst))
            if self:is_pseudo_legal(m) then
                ms[#ms+1] = m
            end
        end
    end

    local color = Piece.side(p)
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
    local ms = t[Piece.class(p)](self, p, pos)
    return filter(function (m) return self:is_legal(m) end,  ms)
end

-- legal_move returns legal moves of the piece at pos.
function chess:legal_move(pos)
    return self:collect_moves(self.moves, pos)
end

-- generate attack map of side.
-- map[i] = n means square i is being threatened by n adversary pieces.
function chess:gen_attack(side) -- int -> Pos.t matrix
    -- initialize map
    local map = {}
    for i = 1, 64 do map[i] = {} end
    setmetatable(map, self.mt)

    -- collect attacks
    for i, p in ipairs(self.board) do
        local pos = pos(i)

        if Piece.side(p) == side then
            for _, m in ipairs(self:legal_move(pos)) do
                if m.t == MOVE_PIECE then
                    table.insert(map[(m.dst)], pos)
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
        print(math.floor(i/2) + 1, self.log[i], self.log[i+1] or "")
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
        local b = {}
        for p in string.gmatch(str, '[^/]') do
            if is_digit(p) then -- empty
                for _ = 1, tonumber(p) do
                    b[#b+1] = Piece.none
                end
            else
                b[#b+1] = ppiece(p)
            end
        end

        assert(#b == 64)
        setmetatable(b, self.mt)
        self.board = b
    end

    local function pside(str)
        if str == 'w' then
            self.side = WHITE
        elseif str == 'b' then
            self.side = BLACK
        end
    end

    local function pcastle(str)
        self.castling_right = {{false, false}, {false, false}}

        for c in string.gmatch(str, '.') do
            if c == '-' then break end

            local color = is_uppercase(c) and WHITE or BLACK
            c = string.upper(c)

            local side
            if c == 'K' then
                side = KINGSIDE
            elseif c == 'Q' then
                side = QUEENSIDE
            else
                assert(false, 'invalid input')
            end


            self.castling_right[color][side] = true
        end
    end

    local function penpassant(str)
        if str ~= "-" then
            local file, rank = string.match(str, "(%l)(%d)")
            file = FILE_REV[file]

            local color
            if rank == '3' then
                color = WHITE
            elseif rank == '6' then
                color = BLACK
            end

            assert(color ~= nil, "invalid enpassant rank")
            self.dpush_file[color] = file
        end
    end

    local function phalfclock(str) end -- not implemented

    local function pturn(str)
        self.turn = tonumber(str)
    end

    local t = { pboard, pside, pcastle, penpassant, phalfclock, pturn }
    local step = 1
    for part in string.gmatch(fen, "%S+") do
        t[step](part)
        step = step + 1
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
        local pos = Pos.make(yr(y), xf(x))
        if Pos.in_bound(pos) then
            sel:click(pos)
        end
    end
end

-- board loading function
function love.filedropped(file)
    file:open('r')
    local bs, _ = file:read()
    reset_game()
    chess:load_fen(bs)
end

-- Drawing Functions --

local G = love.graphics

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
    local x = mid(x)
    local y = mid(y)
    local side = Piece.side(p)
    local class = Piece.class(p)
    local r = SQUARE_SIZE/2 - 5

    -- temporary drawing
    local color = ({{1, 1, 1}, {0, 0, 0}})[side]
    G.setColor(color)
    G.circle('fill', x, y, r)
    G.setColor(.5, .5, .5)
    G.print(PTYPE[class], x, y)
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
            local p = self:get_piece(Pos.make(rank, file))
            if not Piece.is_none(p) then
                Piece.draw(fx(file), ry(rank), p)
            end
        end
    end

    self:draw_win_prompt()
end

-- promotion window
function sel:draw_promotion_ui()
    G.setColor(self.promotion_color)
    G.rectangle('fill', fx(1), ry(5), 4*SQUARE_SIZE, SQUARE_SIZE)

    for f = 3, 6 do
        local p = Piece.make(chess.side, self.pieces[f-2])
        Piece.draw(fx(f), ry(5), p)
    end
    
end

-- draw move candidates
function sel:draw_options()
    G.setColor(self.color)
    for _, m in ipairs(self.moves) do
        local dst = Move.sel(m)
        local x = fx(Pos.file(dst))
        local y = ry(Pos.rank(dst))

        G.circle('fill', mid(x), mid(y), self.radius)
    end
end

function sel:draw()
    if self.selected ~= nil then
        G.setColor(self.color)
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

-- debug: draw squares attacked by
function draw_attack(map)
    G.setColor(.5, 1, .5)
    for i, v in ipairs(map) do
        local p = pos(i)
        local x = fx(Pos.file(p))
        local y = ry(Pos.rank(p))

        if #v > 0 then
            G.circle('fill', mid(x), mid(y), 10)
        end
    end
end

function love.draw()
    chess:draw()
    sel:draw()
end
