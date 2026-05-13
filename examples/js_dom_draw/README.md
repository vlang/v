# JS DOM Draw

Drawing with mouse events using the DOM API. Adopted from MDN examples.

## Run with veb

```sh
cd examples/js_dom_draw
v run main.v
```

This starts a `veb` server at `http://localhost:3001/` and compiles `draw.js.v` to
`draw.js` before serving `index.html`.

You can also run it from the repository root:

```sh
v run examples/js_dom_draw/main.v
```

## Compile manually

```sh
v -b js_browser examples/js_dom_draw/draw.js.v
```

Then open `index.html` in your favorite browser.
