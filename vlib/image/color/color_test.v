module color

fn delta(x u8, y u8) u8 {
	if x >= y {
		return x - y
	}
	return y - x
}

fn assert_same_rgba(c0 Color, c1 Color) {
	r0, g0, b0, a0 := c0.rgba()
	r1, g1, b1, a1 := c1.rgba()
	assert r0 == r1
	assert g0 == g1
	assert b0 == b1
	assert a0 == a1
}

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

fn test_sq_diff() {
	test_cases := [
		u32(0),
		1,
		2,
		0x0fffd,
		0x0fffe,
		0x0ffff,
		0x10000,
		0x10001,
		0x10002,
		0xfffffffd,
		0xfffffffe,
		0xffffffff,
	]
	for x in test_cases {
		for y in test_cases {
			d := if x > y { x - y } else { y - x }
			want := (d * d) >> 2
			assert sq_diff(x, y) == want
		}
	}
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

fn test_ycbcr_roundtrip_table() {
	for r := 0; r < 256; r += 7 {
		for g := 0; g < 256; g += 5 {
			for b := 0; b < 256; b += 3 {
				r0 := u8(r)
				g0 := u8(g)
				b0 := u8(b)
				y, cb, cr := rgb_to_ycbcr(r0, g0, b0)
				r1, g1, b1 := ycbcr_to_rgb(y, cb, cr)
				assert delta(r0, r1) <= 2
				assert delta(g0, g1) <= 2
				assert delta(b0, b1) <= 2
			}
		}
	}
}

fn test_ycbcr_to_rgb_consistency() {
	for y := 0; y < 256; y += 7 {
		for cb := 0; cb < 256; cb += 5 {
			for cr := 0; cr < 256; cr += 3 {
				x := YCbCr{
					y:  u8(y)
					cb: u8(cb)
					cr: u8(cr)
				}
				r0, g0, b0, _ := x.rgba()
				r1 := u8(r0 >> 8)
				g1 := u8(g0 >> 8)
				b1 := u8(b0 >> 8)
				r2, g2, b2 := ycbcr_to_rgb(x.y, x.cb, x.cr)
				assert r1 == r2
				assert g1 == g2
				assert b1 == b2
			}
		}
	}
}

fn test_ycbcr_gray_and_nycbcra_supersets() {
	for i in 0 .. 256 {
		assert_same_rgba(Color(YCbCr{
			y:  u8(i)
			cb: 0x80
			cr: 0x80
		}), Color(Gray{
			y: u8(i)
		}))
		assert_same_rgba(Color(NYCbCrA{
			ycbcr: YCbCr{
				y:  0xff
				cb: 0x80
				cr: 0x80
			}
			a:     u8(i)
		}), Color(Alpha{
			a: u8(i)
		}))
		assert_same_rgba(Color(NYCbCrA{
			ycbcr: YCbCr{
				y:  u8(i)
				cb: 0x40
				cr: 0xc0
			}
			a:     0xff
		}), Color(YCbCr{
			y:  u8(i)
			cb: 0x40
			cr: 0xc0
		}))
	}
}

fn test_cmyk_roundtrip_table() {
	for r := 0; r < 256; r += 7 {
		for g := 0; g < 256; g += 5 {
			for b := 0; b < 256; b += 3 {
				r0 := u8(r)
				g0 := u8(g)
				b0 := u8(b)
				c, m, y, k := rgb_to_cmyk(r0, g0, b0)
				r1, g1, b1 := cmyk_to_rgb(c, m, y, k)
				assert delta(r0, r1) <= 1
				assert delta(g0, g1) <= 1
				assert delta(b0, b1) <= 1
			}
		}
	}
}

fn test_cmyk_to_rgb_consistency_and_gray() {
	for c := 0; c < 256; c += 7 {
		for m := 0; m < 256; m += 5 {
			for y := 0; y < 256; y += 3 {
				for k := 0; k < 256; k += 11 {
					x := CMYK{
						c: u8(c)
						m: u8(m)
						y: u8(y)
						k: u8(k)
					}
					r0, g0, b0, _ := x.rgba()
					r1 := u8(r0 >> 8)
					g1 := u8(g0 >> 8)
					b1 := u8(b0 >> 8)
					r2, g2, b2 := cmyk_to_rgb(x.c, x.m, x.y, x.k)
					assert r1 == r2
					assert g1 == g2
					assert b1 == b2
				}
			}
		}
	}
	for i in 0 .. 256 {
		assert_same_rgba(Color(CMYK{
			c: 0
			m: 0
			y: 0
			k: u8(255 - i)
		}), Color(Gray{
			y: u8(i)
		}))
	}
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

	p2 := Palette{
		colors: [
			Color(RGBA{0xff, 0xff, 0xff, 0xff}),
			Color(RGBA{0x80, 0x00, 0x00, 0xff}),
			Color(RGBA{0x7f, 0x00, 0x00, 0x7f}),
			Color(RGBA{0x00, 0x00, 0x00, 0x7f}),
			Color(RGBA{0x00, 0x00, 0x00, 0x00}),
			Color(RGBA{0x40, 0x40, 0x40, 0x40}),
		]
	}
	for i, c in p2.colors {
		assert p2.index(c) == i
	}
	assert p2.convert(Color(RGBA{0x80, 0x00, 0x00, 0x80}))! == Color(RGBA{0x7f, 0x00, 0x00, 0x7f})
}
