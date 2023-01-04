<img src='https://raw.githubusercontent.com/vlang/v/master/examples/tetris/screenshot.png' width=300>

### Dependencies (Ubuntu)
```sh
sudo apt install libx11-dev
sudo apt install libxi-dev
sudo apt install libxcursor-dev
sudo apt install libgl-dev
```

## Compiling to JS

```sh
v -b js_browser examples/tetris/tetris.js.v
```

And then open `index.html` with your favourite web browser.