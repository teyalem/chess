# TODOS

## now

- [ ] refactoring
    + [ ] split main.lua
    + [ ] objectify structs
    + [ ] standard way to mark objects

## Future Goals

- [ ] stalemate
- [ ] map rotation
- [ ] UI rework
- [ ] Sprite rework (black should be darker)
- [ ] AI (maybe)
- [ ] ~~packaging (trivial thing)~~

# (Old) Major Goals: (not being done this way)

- [x] draw board
- [x] move pieces
- [x] turn
- [x] pieces' movements
  + [x] calculate possible moves
  + [x] display possible moves
  + [x] restrict move
- [x] check and checkmate
  + [x] generate attack map
  + [x] determine check & checkmate
- [x] restrict moves
  + [x] check
  + [x] absolute pins
- [x] save & print move log
- [x] special rules
  + [x] refactor functions to return moves (instead of positions)
  + [x] castling
  + [x] en passant
  + [x] promotion
    - [x] implement piece selection window
- [x] win prompt
- [x] more well encoded move and better move generator
  + [x] wipe out duplicated codes
  + [x] implement better move generator
- [x] refactor chess:domove and chess:is_legal
- [x] pieces' sprites
- [x] refactor functions to use MOVE_* enum.
- [x] feature - load chess board from file (args or drag-and-drop)
  + [x] also from clipboard
- [x] add Box module for boards, attack maps, etc.
- [x] add sprite
