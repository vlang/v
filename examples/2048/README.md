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

## Compiling to WASM:

1. Install Emscripten from https://emscripten.org/docs/getting_started/downloads.html

2. Make sure that the environment in your shell is setup correctly,
i.e. that `emcc --version` works.
```sh
. /opt/emsdk/emsdk_env.sh
emcc --version
```

3. Compile the game to WASM:
```sh
v -skip-unused -prod -os wasm32_emscripten examples/2048/`
```

4. Copy the 2048 file to `index.js` (can be done once; this step will be removed soon):
```sh
cp examples/2048/2048 examples/2048/index.js
```

5. Run/test the game:
```sh
emrun examples/2048/index.html
```

Once you have run the game, you can make changes,
then just recompile (step 3), and refresh the game in your browser.
