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
-- - [ ] more well encoded move and better move generator
--   + [ ] wipe out duplicated codes
--   + [ ] implement better move generator
-- - [ ] refactor chess:domove and chess:is_legal
-- - [ ] pieces' sprites

-- Future Goals:
-- - [ ] map rotation
-- - [ ] stalemate
-- - [ ] AI (maybe)

-- TODO: look for sensible colors for board
-- TODO: add sprite

-- Naming Conventions --
-- 1. Global const variables are in CONSTANT_CASE.
-- 2. Module names are in CamelCase.
-- 3. function names are in snake_case.

-- External Modules --
local G = love.graphics

-- List Functions --

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
PTYPE = { '', 'N', 'B', 'R', 'Q', 'K' }
PNAME = { 'pawn', 'knight', 'bishop', 'rook', 'queen', 'king' }

-- 8 1 2
-- 7 0 3
-- 6 5 4 Ray Directions --
RD = { [0] = 0, 1, 1, 0, -1, -1, -1, 0, 1 } -- row direction
FD = { [0] = 0, 0, 1, 1, 1, 0, -1, -1, -1 } -- file direction

-- order: white, black
OPPONENT = { BLACK, WHITE } -- opponent[p] player
BACKRANK = { 1, 8 } -- backrank[p] of player p
PAWN_DIR = { 1, 5 } -- pawns' marching direction

-- Board --
-- board is array of Pieces, indexed like this:
-- X  1  2  3  4  5  6  7  8
-- 8  1  2  3  4  5  6  7  8
-- 7  9 10 11 12 13 14 15 16
-- 6 17 18 19 20 21 22 23 24
-- 5 25 26 27 28 29 30 31 32
-- 4 33 34 35 36 37 38 39 40
-- 3 41 42 43 44 45 46 47 48
-- 2 49 50 51 52 53 54 55 56
-- 1 57 58 59 60 61 62 63 64
-- (can be changed)

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

square_size = 50 -- size of each square

-- Click Interface --

sel = { -- Selection UI Settings
    color = {1, .3, .3}, -- Color
    promotion_color = {.5, .5, .5},
    radius = square_size/2 - 4, -- number
    pieces = { 2, 3, 4, 5 } -- piece to promote to (knight, bishop, rook, queen)
}

function sel:reset()
    self.state = 'normal' -- Sel.State.t
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
    self.state = 'promotion'
    self.move = m
end

-- Main logic of select UI
function sel:click(pos)
    if chess.winner ~= nil then return end

    if self.state == 'normal' and chess:get_side(pos) == chess.side then
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

                chess:turn(m)
                self:reset()
            end

            if not moved and chess:get_side(pos) == chess.side then -- reselect
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

        chess:turn(m)
        self:reset()
    end
end

-- Position Module --

Pos = {}
Pos.filename = { 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h' }

function Pos.rank(p)
    return p[1]
end

function Pos.file(p)
    return p[2]
end

function Pos.equals(a, b)
    return Pos.rank(a) == Pos.rank(b) and Pos.file(a) == Pos.file(b)
end

Pos.mt = { __eq = Pos.equals }

function Pos.make(rank, file)
    local p = {rank, file}
    setmetatable(p, Pos.mt)
    return p
end

function Pos.advance(p, r, f)
    return Pos.make(Pos.rank(p) + r, Pos.file(p) + f)
end

function Pos.in_bound(p)
    local r = Pos.rank(p)
    local f = Pos.file(p)
    return (1 <= r and r <= 8) and (1 <= f and f <= 8)
end

function Pos.is_kingside(p)
    local f = Pos.file(p)
    return 5 <= f and f <= 8
end

function Pos.to_string(p)
    return string.format("%s%d", Pos.filename[Pos.file(p)], Pos.rank(p))
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
    local r = 8 - math.floor((index-1)/8)
    local f = index % 8
    if f == 0 then f = 8 end
    return Pos.make(r, f)
end

-- Chess Piece Module --
Piece = {}

function Piece.make(side, class)
    return {
        side = side, 
        class = class,
        movecount = 0,
        last_moved = 0,
    }
end

-- nothing
Piece.none = Piece.make(0, 0)

function Piece.side(p)
    return p.side
end

function Piece.class(p)
    return p.class
end

function Piece.last_moved(p)
    return p.last_moved
end

function Piece.inc_moved(p, t)
    p.movecount = p.movecount + 1
    p.last_moved = t
end

function Piece.is_moved(p)
    return p.movecount > 0
end

function Piece.is_none(p)
    return Piece.class(p) == NONE
end

function Piece.is_pawn(p)
    return Piece.class(p) == PAWN
end

function Piece.is_king(p)
    return Piece.class(p) == KING
end

-- Move Module --
Move = {}

function Move.normal(p, s, d, x)
    return {
        t = 'normal',
        piece = p,
        src = s,
        dst = d,
        captures = x or false,
    }
end

function Move.castle(kpos, rpos)
    return {
        t = 'castle',
        kpos = kpos,
        rpos = rpos,
    }
end

function Move.enpassant(p, src, cap, dst)
    return {
        t = 'enpassant',
        piece = p,
        src = src,
        cap = cap,
        dst = dst
    }
end

-- NOTE: not used in this version
function Move.promotion(src, dst, x, p)
    return {
        t = 'promotion',
        src = src,
        dst = dst,
        captures = x,
        promotion = p,
    }
end

function Move.to_promotion(m, p)
    if m.t == 'normal' then
        m.t = 'promotion'
        m.promotion = p
    end
    return m
end

-- returns the position to press in order to do the move m.
function Move.sel(m)
    if m.t == 'normal' then
        return m.dst
    elseif m.t == 'castle' then
        return m.rpos
    elseif m.t == 'enpassant' then
        return m.dst
    end
end

-- to_algebraic returns the Algebraic Notation of m.
function Move.to_algebraic(m)
    local function format_move(p, src, dst, x, promotion)
        local x = x and 'x' or ''
        local d = Pos.to_string(m.dst)

        local s = PTYPE[Piece.class(p)]
        if Piece.is_pawn(p) or promotion then
            s = captured and Pos.filename[Pos.file(src)] or ""
        end

        local pro = ""
        if promotion then
            pro = '=' .. PTYPE[Piece.class(p)]
        end

        return string.format("%s%s%s%s", s, x, d, pro)
    end

    if m.t == 'normal' then
        return format_move(p, m.src, m.dst, m.captures)
    elseif m.t == 'castle' then
        return Pos.is_kingside(m.rpos) and "O-O" or "O-O-O"
    elseif m.t == 'enpassant' then
        return format_move(p, m.src, m.dst, true)
    elseif m.t == 'promotion' then
        return format_move(p, m.src, m.dst, m.captures)
    end

    return ""
end

-- Chess --

chess = {}

-- reset states
function chess:reset()
    -- current turn and side
    self.turn = 1; self.side = WHITE

    -- board and position of king
    local function make_board(colors, pieces)
        local b = {}
        for i, s in ipairs(colors) do
            b[i] = Piece.make(s, pieces[i])
        end
        return b
    end

    self.board = make_board(BCOLOR, BCLASS)
    self.king_pos = {Pos.make(1, 5), Pos.make(8, 5)}

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
    self.attacked[OPPONENT[self.side]] = self:gen_attack(self.side)

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
    return self.board[index(pos)]
end

function chess:get_side(pos)
    assert(Pos.in_bound(pos))
    return Piece.side(self.board[index(pos)])
end

function chess:get_class(pos)
    assert(Pos.in_bound(pos))
    return Piece.class(self.board[index(pos)])
end

function chess:is_empty(pos)
    local p = self:get_piece(pos)
    return Piece.is_none(p)
end

function chess:can_capture(pos, side)
    return self:get_side(pos) == OPPONENT[side]
end

function chess:inbetween(a, b, atk)
    local function c() return {} end
    local t = {
        c,
        c,
        self.bishop_move,
        self.rook_move,
        self.queen_move,
        c,
    }

    local atk_moves = t[self:get_class(atk)](self, atk, true)
    local xray = map(function (m) return m.dst end, atk_moves)
    return mem(a, xray) and mem(b, xray) and Pos.is_lined(a, b, atk)
end

-- test a move is pseudo-legal.
function chess:is_pseudo_legal(m)
    if m.t == 'normal' then
        local side = Piece.side(m.piece)
        return Pos.in_bound(m.dst)
        and (self:is_empty(m.dst) or self:can_capture(m.dst, side))
    end
    return true -- pseudo legality check is done in move function
end

-- chess:is_legal(move) returns true if the move is legal.
function chess:is_legal(move)
    if not self:is_pseudo_legal(move) then return false end

    if move.t == 'normal' then
        local p = self:get_piece(move.src)
        local side = Piece.side(p)
        local checked = self:is_checked(side)

        if Piece.is_king(p) then -- king can move to uncontrolled square
            return #self.attacked[side][index(move.dst)] == 0
        end

        local kpos = self.king_pos[side]
        local point = checked and kpos or move.src
        local atks = self.attacked[side][index(point)]
        local pinned = false

        for _, attacker in ipairs(atks) do
            pinned = pinned or self:inbetween(kpos, move.src, attacker) -- pinned by attacker

            if self:inbetween(kpos, move.dst, attacker) -- blocking sight
                or move.dst == attacker then -- uncheck by capture
                return true
            end
        end

        return not (pinned or checked)

    elseif move.t == 'castle' then
        local side = self:get_side(move.kpos)
        local checked = self:is_checked(side)
        if checked then return false end

        local d = Pos.is_kingside(move.rpos) and 1 or -1
        local nkpos = Pos.advance(move.kpos, 0, 2*d)

        for i = index(move.kpos), index(nkpos), d do
            if #self.attacked[side][i] > 0 then return false end
        end
        return true

    elseif move.t == 'enpassant' then
        local side = self:get_side(move.src)
        local kpos = self.king_pos[side]
        local checked = self:is_checked(side)

        local atks = self.attacked[side][index(move.src)]
        local pinned = false

        for _, attacker in ipairs(atks) do
            pinned = pinned or self:inbetween(kpos, move.src, attacker) -- pinned by attacker
        end

        if checked then
            return mem(self.attacked[side][kpos], move.cap)
        else
            return not pinned
        end
    end

    assert(false) -- must be one of above
end

function chess:is_checked(side)
    return #self.attacked[side][index(self.king_pos[side])] > 0
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
        self.board[index(dst)] = p
        self.board[index(src)] = Piece.none
        Piece.inc_moved(p, self.turn)
    end

    local function capture(pos)
        self.captured[#self.captured+1] = self:get_piece(pos)
        self.board[index(pos)] = Piece.none
    end

    if m.t == 'normal' then
        if m.captures then capture(m.dst) end

        -- update king's position
        if Piece.is_king(p) then
            self.king_pos[Piece.side(p)] = m.dst
        end

        move(p, m.src, m.dst)

    elseif m.t == 'castle' then
        local k = self:get_piece(m.kpos)
        local r = self:get_piece(m.rpos)

        local d = Pos.is_kingside(m.rpos) and 1 or -1
        local nk = Pos.advance(m.kpos, 0, 2*d)
        local nr = Pos.advance(nk, 0, -d)

        self.king_pos[Piece.side(k)] = nk
        move(k, m.kpos, nk)
        move(r, m.rpos, nr)

    elseif m.t == 'enpassant' then
        capture(m.cap)
        move(m.p, m.src, m.dst)

    elseif m.t == 'promotion' then
        if m.captures then capture(m.dst) end
        move(m.p, m.src, m.dst)
        m.p.class = m.promotion
    end
end

-- Pseudo-legal Move Generating Functions --
-- move function = Piece.t -> Pos.t -> Move.t list

-- cast a ray to direction dir and save moves to ms
-- Move.t array -> Piece.t -> Pos.t -> int -> bool -> unit
function chess:raycast(ms, p, src, dir, xray)
    local xray = xray or false

    for i = 1, 7 do
        local sq = Pos.advance(src, i*RD[dir], i*FD[dir])
        local m = Move.normal(p, src, sq, not self:is_empty(sq))

        if self:is_pseudo_legal(m) then
            ms[#ms+1] = m
        end

        if not (Pos.in_bound(sq) and (xray or self:is_empty(sq))) then
            break -- ray blocked; stop casting ray
        end
    end
end

function chess:pawn_attack(p, src)
    local dir = PAWN_DIR[Piece.side(p)]
    local s = map(function (f) return Pos.advance(src, dir, f) end, {-1, 1})
    return filter(Pos.in_bound, s)
end

-- TODO: rework pawn_move and pawn_attack
function chess:pawn_move(p, src)
    local ms = {}
    local side = Piece.side(p)
    local dir = PAWN_DIR[side]

    -- push, double push
    local steps = Piece.is_moved(p) and 1 or 2
    for i = 1, steps do
        local sq = Pos.advance(src, i*RD[dir], 0)
        if self:is_empty(sq) then
            ms[#ms+1] = Move.normal(p, src, sq)
        else
            break
        end
    end

    local function just_moved(cp)
        local mt = Piece.last_moved(cp)
        local preturn = ({0, 1})[Piece.side(cp)]
        return mt == self.turn - preturn
    end

    -- pawn attack
    for _, dst in ipairs(self:pawn_attack(src)) do
        -- move by capture
        if self:can_capture(dst, side) then
            ms[#ms+1] = Move.normal(p, src, dst, true)
        end

        -- en passant
        local cap = Pos.make(Pos.rank(src), Pos.file(dst))
        local cp = self:get_piece(cap)
        if self:can_capture(cap, side) and cp.movecount == 1 and just_moved(cp) then
            ms[#ms+1] = Move.enpassant(src, cap, dst)
        end
    end

    return ms
end

function chess:knight_move(p, src)
    local ps = {}

    for i = 2, 8, 2 do
        ps[#ps+1] = Pos.advance(src, RD[i], 2*FD[i])
        ps[#ps+1] = Pos.advance(src, 2*RD[i], FD[i])
    end

    local ms = {}
    for _, dst in ipairs(ps) do
        local m = Move.normal(p, src, dst, not self:is_empty(dst))
        if self:is_pseudo_legal(m) then ms[#ms+1] = m end
    end

    return ms
end

function chess:bishop_move(p, src, xray)
    local out = {}
    for i = 2, 8, 2 do
        self:raycast(out, p, src, i, xray)
    end
    return out
end

function chess:rook_move(p, src, xray)
    local out = {}
    for i = 1, 7, 2 do
        self:raycast(out, p, src, i, xray)
    end
    return out
end

function chess:queen_move(p, src, xray)
    return append(self:bishop_move(p, src, xray), self:rook_move(p, src, xray))
end

function chess:castling(ms, k, kpos)
    if Piece.is_moved(k) then return end

    local side = Piece.side(k)
    local rank = BACKRANK[side]

    local function t(p, s, e)
        if Piece.is_moved(p) then return false end
        for i = s, e do
            if not self:is_empty(Pos.make(rank, i)) then return false end
        end
        return true
    end

    local function add(rpos, s, e)
        local p = self:get_piece(rpos)
        if t(p, s, e) then ms[#ms+1] = Move.castle(kpos, rpos) end
    end

    local qr = Pos.make(rank, 1)
    add(qr, 2, 4)
    local kr = Pos.make(rank, 8)
    add(kr, 6, 7)
end

function chess:king_move(p, src)
    local ms = {}

    for i = 1, 8 do
        local dst = Pos.advance(src, RD[i], FD[i])
        local m = Move.normal(p, src, dst, not self:is_empty(dst))
        if self:is_pseudo_legal(m) then
            ms[#ms+1] = m
        end
    end

    self:castling(ms, p, src)

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
    local p = self:get_piece(p)
    local ms = t[self:get_class(pos)](self, p, pos)
    return filter(function (m) return self:is_legal(m) end,  ms)
end

-- legal_move returns legal moves of the piece at pos.
function chess:legal_move(pos)
    return self:collect_moves(self.moves, pos)
end

-- legal attacks
function chess:legal_attack(pos)
    -- modify move table to attack table
    local t = {unpack(self.moves)}
    t[PAWN] = chess.pawn_attack

    return self:collect_moves(t, pos)
end


-- generate attack map of side.
-- map[i] = n means square i is being threatened by n adversary pieces.
-- FIXME: not checking pawn's attack squares
function chess:gen_attack(side) -- int -> Pos.t matrix
    -- initialize map
    local map = {}
    for i = 1, 64 do map[i] = {} end

    -- collect attacks
    for i, p in ipairs(self.board) do
        local pos = pos(i)

        if Piece.side(p) == side then
            for _, m in ipairs(self:legal_attack(pos)) do
                if m.t == 'normal' then
                    local l = map[index(m.dst)]
                    l[#l+1] = pos
                end
            end
        end
    end

    return map
end

function chess:turn(m)
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

-- Screen Position Functions --

-- middle point of square
function mid(n)
    return n + square_size/2
end

-- file to screen x
function fx(file)
    return (file-1) * square_size
end

-- screen x to file
function xf(x)
    return math.floor(x/square_size) + 1
end

-- rank to screen y
function ry(rank)
    return (8-rank) * square_size
end

-- screen y to rank
function yr(y)
    return 8 - math.floor(y/square_size)
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

-- Drawing Functions --

-- draw a square of board
function draw_square(rank, file, color)
    local x = fx(file)
    local y = ry(rank)
    local s = square_size

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
    local r = square_size/2 - 5

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
        G.rectangle('fill', square_size, square_size, 200, 100)
        G.setColor(color)
        G.print(msg, square_size, square_size, 0, 3, 3)
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
    G.rectangle('fill', fx(1), ry(5), 4*square_size, square_size)

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
            square_size, square_size)
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
    for r = 1, 8 do
        for f = 1, 8 do
            local x = fx(f)
            local y = ry(r)
            local s = square_size

            if #map[index(Pos.make(r, f))] > 0 then
                G.circle('fill', x+s/2, y+s/2, s/4)
            end
        end
    end
end

function love.draw()
    chess:draw()
    sel:draw()
end
