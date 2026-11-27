import svg
import time

// A small, self-contained benchmark for the parse/flatten/rasterize pipeline.
// Run with: v -prod run examples/svg/bench.v

fn make_doc() svg.Document {
	mut d := svg.new(400, 400)
	for i in 0 .. 120 {
		x := f64(i % 12) * 32
		y := f64(i / 12) * 32
		d.circle(x, y, 12, svg.fill('#336699'))
		d.rect(x + 2, y + 2, 18, 18, svg.fill('#cc6633'))
	}
	return d
}

fn main() {
	d := make_doc()
	source := d.to_svg(svg.WriteOptions{ minify: true })
	println('svg bytes=${source.len}')

	t0 := time.now()
	for _ in 0 .. 2000 {
		_ = svg.parse_svg(source) or { panic('parse failed') }
	}
	println('parse 2000: ${time.since(t0).milliseconds()}ms')

	t1 := time.now()
	for _ in 0 .. 500 {
		_ = svg.flatten_document(&d, svg.FlattenOptions{}) or { panic('flatten failed') }
	}
	println('flatten 500: ${time.since(t1).milliseconds()}ms')

	t2 := time.now()
	for _ in 0 .. 10 {
		_ = d.rasterize(svg.RasterOptions{ width: 400, height: 400, samples: 2 }) or {
			panic('raster failed')
		}
	}
	println('raster 10: ${time.since(t2).milliseconds()}ms')

	t3 := time.now()
	for _ in 0 .. 20000 {
		_ = svg.parse_path('M10-20A5.5.3-4 110-.1 L30 40 C1 2 3 4 5 6 Z') or {
			panic('path failed')
		}
	}
	println('path 20000: ${time.since(t3).milliseconds()}ms')
}
