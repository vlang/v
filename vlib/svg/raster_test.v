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
