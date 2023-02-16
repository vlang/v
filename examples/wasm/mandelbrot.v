// v -b wasm -no-builtin mandelbrot.v # create `mandelbrot.wasm`
// 
// python -m http.server 8080
// emrun mandelbrot.html
// ....
// ....

fn JS.canvas_x() int
fn JS.canvas_y() int
fn JS.setpixel(x int, y int, c f64)

fn main() {
	max_x := JS.canvas_x()
	max_y := JS.canvas_y()

	mut y := 0
	for y < max_y {
		y += 1
		mut x := 0
		for x < max_x {
			x += 1

			e := (f64(y) / 50) - 1.5
			f := (f64(x) / 50) - 1.0

			mut a := 0.0
			mut b := 0.0
			mut i := 0.0
			mut j := 0.0
			mut c := 0.0

			for i * i + j * j < 4 && c < 255 {
				i = a * a - b * b + e
				j = 2 * a * b + f
				a = i
				b = j
				c += 1
			}

			JS.setpixel(x, y, c)
		}
	}
}