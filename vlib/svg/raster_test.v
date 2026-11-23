module svg

fn test_rasterize_rect() {
	mut d := new(40, 40)
	d.rect(5, 5, 30, 30, fill('#ff0000'))
	img := d.rasterize(RasterOptions{ width: 40, height: 40, samples: 2 }) or {
		assert false
		return
	}
	assert img.pix.len == 40 * 40 * 4
	// Center pixel must be opaque red.
	i := 20 * img.stride + 20 * 4
	assert img.pix[i] == 255
	assert img.pix[i + 1] == 0
	assert img.pix[i + 2] == 0
	assert img.pix[i + 3] > 250
	// Corner must remain transparent.
	assert img.pix[3] == 0
}

fn test_rasterize_circle() {
	mut d := new(40, 40)
	d.circle(20, 20, 15, fill('#00ff00'))
	img := d.rasterize(RasterOptions{ width: 40, height: 40, samples: 2 }) or {
		assert false
		return
	}
	i := 20 * img.stride + 20 * 4
	assert img.pix[i] == 0
	assert img.pix[i + 1] == 255
	assert img.pix[i + 3] > 250
	assert img.pix[3] == 0
}

fn test_stroke_only() {
	mut d := new(40, 40)
	d.rect(10, 10, 20, 20, fill('none'), stroke('#000000'), stroke_width(2))
	img := d.rasterize(RasterOptions{ width: 40, height: 40 }) or {
		assert false
		return
	}
	edge := 20 * img.stride + 10 * 4 + 3
	center := 20 * img.stride + 20 * 4 + 3
	assert img.pix[edge] > 0
	assert img.pix[center] == 0
}

fn test_evenodd_across_subpaths() {
	doc := parse_svg('<svg width="100" height="100"><path d="M10 10 H90 V90 H10 Z M30 30 H70 V70 H30 Z" fill="#ff0000" fill-rule="evenodd"/></svg>') or {
		assert false
		return
	}
	img := doc.rasterize(RasterOptions{
		width:  100
		height: 100
	}) or {
		assert false
		return
	}
	i := 50 * img.stride + 50 * 4
	assert img.pix[i + 3] == 0
}

fn test_rasterize_uses_requested_viewport() {
	doc := parse_svg('<svg width="900" height="600" viewBox="0 0 100 100"><rect width="100" height="100" fill="#00ff00"/></svg>') or {
		assert false
		return
	}
	img := doc.rasterize(RasterOptions{ width: 50, height: 50 }) or {
		assert false
		return
	}
	assert img.pix.len == 50 * 50 * 4
	i := 25 * img.stride + 25 * 4 + 3
	assert img.pix[i] > 250
}

fn test_gradient_sampling() {
	mut g := Gradient{
		id:    'g'
		x1:    0
		y1:    0
		x2:    1
		y2:    0
		units: .object_bbox
	}
	g.stops << Stop{ offset: 0, color: rgb(0, 0, 0) }
	g.stops << Stop{ offset: 1, color: rgb(255, 255, 255) }
	c0 := gradient_color(g, 0)
	c1 := gradient_color(g, 1)
	assert c0.r == 0
	assert c1.r == 255
}
