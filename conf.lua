GAME = {
    name = "LőChess",
    author = "Hyeonung Baek (teyalem)",
    version = 0.9
}

-- GUI --

SQUARE_SIZE = 50 -- size of each square

-- Board --
BOARD_X = 0
BOARD_Y = 0
BOARD_WIDTH = 8 * SQUARE_SIZE
BOARD_HEIGHT = 8 * SQUARE_SIZE

-- Log --
LOG_X = BOARD_WIDTH
LOG_Y = 0
LOG_HALFWIDTH = 50
LOG_WIDTH = 2 * LOG_HALFWIDTH
TEXT_SIZE = 20

-- Screen --
-- Orientation:
-- +--------+---+
-- |        |   |
-- | BOARD  |LOG|
-- |        |   |
-- +--------+---+
SCREEN_WIDTH = BOARD_WIDTH + LOG_WIDTH
SCREEN_HEIGHT = BOARD_HEIGHT

-- Selection Settings --

SEL_COLOR = {1, .3, .3}
SEL_RADIUS = SQUARE_SIZE/2 - 4
PROMOTION_COLOR = {.5, .5, .5}

-- Sprites --

SPRITE_SIZE = 32
SPRITE_POS = (SQUARE_SIZE - SPRITE_SIZE) / 2

function love.conf(t)
    t.window.title = string.format("%s %f", GAME.name, GAME.version)
    t.window.width = SCREEN_WIDTH
    t.window.height = SCREEN_HEIGHT
end
