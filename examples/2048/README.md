# V 2048

This is a simple 2048 game, written in [the V programming language](https://vlang.io/).

WebAssembly demo: https://v2048.vercel.app

![screenshot](demo.png)

## Description:
Merge tiles by moving them.
After each move, a new random tile is added (2 or 4). 
The goal of the game is to create a tile with a value of 2048.

## Keys:
Escape - exit the game
Backspace - undo last move
n - restart the game
t - toggle the UI theme
Enter - toggle the tile text format

UP,LEFT,DOWN,RIGHT / W,A,S,D / touchscreen swipes - move the tiles

## Running instructions:
Compile & run the game with `./v run examples/2048`

