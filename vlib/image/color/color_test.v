module color

fn test_rgba_and_nrgba_conversion() {
	r, g, b, a := RGBA{
		r: 0x11
		g: 0x22
		b: 0x33
		a: 0x44
	}.rgba()
	assert r == 0x1111
	assert g == 0x2222
	assert b == 0x3333
	assert a == 0x4444

	c := to_nrgba(Color(RGBA64{
		r: 0x4000
		g: 0x8000
		b: 0xc000
		a: 0x8000
	}))
	assert c.a == 0x80
	assert c.r == 0x7f || c.r == 0x80
	assert c.g == 0xff
}

fn test_ycbcr_and_cmyk_round_trips() {
	y, cb, cr := rgb_to_ycbcr(255, 0, 0)
	r, g, b := ycbcr_to_rgb(y, cb, cr)
	assert r >= 253
	assert g <= 1
	assert b <= 1

	c, m, yy, k := rgb_to_cmyk(0, 255, 0)
	assert c == 255
	assert m == 0
	assert yy == 255
	assert k == 0
	rr, gg, bb := cmyk_to_rgb(c, m, yy, k)
	assert rr == 0
	assert gg == 255
	assert bb == 0
}

fn test_palette_index_and_model() {
	p := Palette{
		colors: [
			Color(RGBA{0, 0, 0, 255}),
			Color(RGBA{255, 255, 255, 255}),
			Color(RGBA{255, 0, 0, 255}),
		]
	}
	assert p.index(Color(RGBA{250, 1, 2, 255})) == 2
	assert p.convert(Color(RGBA{254, 254, 254, 255}))! == Color(RGBA{255, 255, 255, 255})

	converted := rgba_model.convert(Color(Gray{
		y: 0x12
	}))!
	assert converted == Color(RGBA{
		r: 0x12
		g: 0x12
		b: 0x12
		a: 0xff
	})
}
