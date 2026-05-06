# JS DOM Cube

## Run with veb

```sh
cd examples/js_dom_cube
v run main.v
```

This starts a `veb` server at `http://localhost:3001/` and compiles `cube.js.v` to
`cube.js` before serving `index.html`.

You can also run it from the repository root:

```sh
v run examples/js_dom_cube/main.v
```

## Compile manually

```sh
v -b js_browser examples/js_dom_cube/cube.js.v
```

Then open `index.html` in your favorite browser.
