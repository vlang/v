# Run V Mandelbrot Example

## Using only V

```
v run .
```

## Using Python or Emscripten

1. First, create `change_color_by_id.wasm`. Compile with `-os browser`.

```
v -b wasm -os browser change_color_by_id.wasm.v
```

2. Then, open the `change_color_by_id.html` file in the browser.
   - CORS errors do not allow `change_color_by_id.wasm` to be loaded.
   - Use `python -m http.server 8080`
   - Use `emrun change_color_by_id.html`
