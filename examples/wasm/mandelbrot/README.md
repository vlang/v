# V Mandelbrot Example

1. First, create `mandelbrot.wasm`. Compile with `-os browser`.

```
v -b wasm -os browser mandelbrot.v
```

2. Then, open the `mandelbrot.html` file in the browser.
	- CORS errors do not allow `mandelbrot.wasm` to be loaded.
	- Use `python -m http.server 8080`
	- Use `emrun mandelbrot.html`