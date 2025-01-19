<img src='https://raw.githubusercontent.com/vlang/v/master/examples/tetris/screenshot.png' width=300>

### Dependencies (Ubuntu)
```sh
sudo apt install libx11-dev
sudo apt install libxi-dev
sudo apt install libxcursor-dev
sudo apt install libxrandr-dev
sudo apt install libgl-dev
```

## Compiling to JS

```sh
v -b js_browser examples/tetris/tetris.js.v
```

And then open `index.html` with your favourite web browser.


## Compiling to WASM

1. Install Emscripten from https://emscripten.org/docs/getting_started/downloads.html

2. Make sure that the environment in your shell is setup correctly,
i.e. that `emcc --version` works.
```sh
. /opt/emsdk/emsdk_env.sh
emcc --version
```

3. Compile the game to WASM:
```sh
v -skip-unused -prod -os wasm32_emscripten examples/tetris/`
```

4. Copy the generated `tetris` file to `index.js`
This can be done once. Note that this step will be removed soon, when
the option `-os wasm32_emscripten` becomes better integrated:
```sh
cp examples/tetris/tetris examples/tetris/tetris.js
```

5. Run/test the game:
```sh
emrun examples/tetris/index.html
```

Once you have run the game, you can make changes,
then just recompile (step 3), and refresh the game in your browser.
