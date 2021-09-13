GAME = {
    name = "LőChess",
    author = "Hyeonung Baek (teyalem)",
    version = 0.9
}

-- GUI --

SQUARE_SIZE = 50 -- size of each square
SCREEN_WIDTH = 8 * SQUARE_SIZE
SCREEN_HEIGHT = 8 * SQUARE_SIZE

-- Selection Settings --

SEL_COLOR = {1, .3, .3}
SEL_RADIUS = SQUARE_SIZE/2 - 4
PROMOTION_COLOR = {.5, .5, .5}
PROMOTION_PIECES = { KNIGHT, BISHOP, ROOK, QUEEN } -- piece to promote to

-- Sprites --

SPRITE_SIZE = 32
SPRITE_POS = (SQUARE_SIZE - SPRITE_SIZE) / 2

function love.conf(t)
    t.window.title = string.format("%s %f", GAME.name, GAME.version)
    t.window.width = SCREEN_WIDTH
    t.window.height = SCREEN_HEIGHT
end
