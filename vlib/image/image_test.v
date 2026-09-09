module image

import image.color

fn rect_in(f Rectangle, g Rectangle) bool {
	if !f.inside(g) {
		return false
	}
	for y := f.min.y; y < f.max.y; y++ {
		for x := f.min.x; x < f.max.x; x++ {
			if !pt(x, y).in_rect(g) {
				return false
			}
		}
	}
	return true
}

fn assert_rgba64_matches_at(img RGBA64Image, x int, y int) {
	got := img.rgba64_at(x, y)
	want_r, want_g, want_b, want_a := img.at(x, y).rgba()
	assert u32(got.r) == want_r
	assert u32(got.g) == want_g
	assert u32(got.b) == want_b
	assert u32(got.a) == want_a
}

fn test_point_and_rectangle_geometry() {
	assert pt(3, 4).str() == '(3,4)'
	assert pt(3, 4).add(pt(1, -2)) == pt(4, 2)
	assert pt(3, 4).sub(pt(1, -2)) == pt(2, 6)
	assert pt(-1, 7).mod(rect(0, 0, 5, 5)) == pt(4, 2)

	r := rect(10, 20, 0, 5)
	assert r.min == pt(0, 5)
	assert r.max == pt(10, 20)
	assert r.dx() == 10
	assert r.dy() == 15
	assert r.inset(2) == rect(2, 7, 8, 18)
	assert rect(0, 0, 5, 5).intersect(rect(3, 4, 8, 7)) == rect(3, 4, 5, 5)
	assert rect(0, 0, 5, 5).union(rect(10, 10, 11, 12)) == rect(0, 0, 11, 12)
	assert rect(0, 0, 1, 1).overlaps(rect(0, 0, 2, 2))
	assert rect(0, 0, 1, 1).inside(rect(-1, -1, 2, 2))
}

fn test_rectangle_invariants_from_go() {
	rects := [
		rect(0, 0, 10, 10),
		rect(10, 0, 20, 10),
		rect(1, 2, 3, 4),
		rect(4, 6, 10, 10),
		rect(2, 3, 12, 5),
		rect(-1, -2, 0, 0),
		rect(-1, -2, 4, 6),
		rect(-10, -20, 30, 40),
		rect(8, 8, 8, 8),
		rect(88, 88, 88, 88),
		rect(6, 5, 4, 3),
	]

	for r in rects {
		for s in rects {
			want_eq := rect_in(r, s) && rect_in(s, r)
			assert r.eq(s) == want_eq

			a := r.intersect(s)
			assert rect_in(a, r)
			assert rect_in(a, s)
			assert (a == Rectangle{}) != r.overlaps(s)

			mut larger_than_a := [a, a, a, a]
			larger_than_a[0].min.x--
			larger_than_a[1].min.y--
			larger_than_a[2].max.x++
			larger_than_a[3].max.y++
			for b in larger_than_a {
				if b.empty() {
					continue
				}
				assert !(rect_in(b, r) && rect_in(b, s))
			}

			u := r.union(s)
			assert rect_in(r, u)
			assert rect_in(s, u)
			if u.empty() {
				continue
			}
			mut smaller_than_u := [u, u, u, u]
			smaller_than_u[0].min.x++
			smaller_than_u[1].min.y++
			smaller_than_u[2].max.x--
			smaller_than_u[3].max.y--
			for b in smaller_than_u {
				assert !(rect_in(r, b) && rect_in(s, b))
			}
		}
	}
}

fn test_rgba_layout_and_sub_image() {
	mut img := new_rgba(rect(10, 20, 12, 22))
	assert img.pix.len == 16
	assert img.stride == 8
	assert img.pix_offset(10, 20) == 0
	assert img.pix_offset(11, 21) == 12

	img.set_rgba(11, 20, color.RGBA{
		r: 1
		g: 2
		b: 3
		a: 4
	})
	assert img.rgba_at(11, 20) == color.RGBA{
		r: 1
		g: 2
		b: 3
		a: 4
	}
	assert img.rgba64_at(11, 20) == color.RGBA64{
		r: 0x0101
		g: 0x0202
		b: 0x0303
		a: 0x0404
	}
	assert !img.opaque()

	sub := img.sub_image(rect(11, 20, 12, 22))
	assert sub.rect == rect(11, 20, 12, 22)
	assert sub.pix_offset(11, 20) == 0
	assert sub.pix[0] == 1

	mut sub_shared := img.sub_image(rect(11, 20, 12, 21))
	sub_shared.set_rgba(11, 20, color.RGBA{
		r: 9
		g: 8
		b: 7
		a: 255
	})
	assert img.rgba_at(11, 20) == color.RGBA{
		r: 9
		g: 8
		b: 7
		a: 255
	}
}

fn test_nrgba_alpha_gray_and_uniform() {
	mut nrgba := new_nrgba(rect(0, 0, 1, 1))
	nrgba.set_rgba64(0, 0, color.RGBA64{
		r: 0x4000
		g: 0x8000
		b: 0xc000
		a: 0x8000
	})
	assert nrgba.nrgba_at(0, 0).a == 0x80
	assert !nrgba.opaque()

	mut gray := new_gray(rect(0, 0, 1, 1))
	gray.set(0, 0, color.Color(color.RGBA{
		r: 255
		g: 255
		b: 255
		a: 255
	}))
	assert gray.gray_at(0, 0) == color.Gray{
		y: 255
	}
	assert gray.opaque()

	u := new_uniform(color.Color(color.RGBA{
		r: 4
		g: 5
		b: 6
		a: 255
	}))
	assert u.at(-100, 100) == color.Color(color.RGBA{
		r: 4
		g: 5
		b: 6
		a: 255
	})
	assert u.opaque()
}

fn test_paletted_image() {
	pal := color.Palette{
		colors: [
			color.transparent,
			color.opaque,
		]
	}
	mut img := new_paletted(rect(0, 0, 2, 1), pal)
	img.set_color_index(1, 0, 1)
	assert img.color_index_at(1, 0) == 1
	assert img.at(1, 0) == color.opaque
	assert !img.opaque()
}

fn test_ycbcr_and_nycbcra_layout() {
	mut img := new_ycbcr(rect(0, 0, 4, 2), .ratio_420)
	assert img.y.len == 8
	assert img.cb.len == 2
	assert img.cr.len == 2
	assert img.y_stride == 4
	assert img.c_stride == 2
	assert img.y_offset(3, 1) == 7
	assert img.c_offset(3, 1) == 1

	img.y[img.y_offset(0, 0)] = 76
	img.cb[img.c_offset(0, 0)] = 84
	img.cr[img.c_offset(0, 0)] = 255
	c := img.ycbcr_at(0, 0)
	assert c.y == 76
	assert c.cb == 84
	assert c.cr == 255
	assert img.opaque()

	mut with_alpha := new_nycbcra(rect(0, 0, 2, 1), .ratio_444)
	assert !with_alpha.opaque()
	with_alpha.a[0] = 255
	with_alpha.a[1] = 255
	assert with_alpha.opaque()
}

fn check_ycbcr_sub_images(r Rectangle, subsample_ratio YCbCrSubsampleRatio, delta Point) {
	r1 := r.add(delta)
	mut img := new_ycbcr(r1, subsample_ratio)
	assert img.y.len <= 100 * 100

	for y := r1.min.y; y < r1.max.y; y++ {
		for x := r1.min.x; x < r1.max.x; x++ {
			yi := img.y_offset(x, y)
			ci := img.c_offset(x, y)
			img.y[yi] = u8((16 * y + x) & 0xff)
			img.cb[ci] = u8((y + 16 * x) & 0xff)
			img.cr[ci] = u8((y + 16 * x) & 0xff)
		}
	}

	for y0 := delta.y + 3; y0 < delta.y + 7; y0++ {
		for y1 := delta.y + 8; y1 < delta.y + 13; y1++ {
			for x0 := delta.x + 3; x0 < delta.x + 7; x0++ {
				for x1 := delta.x + 8; x1 < delta.x + 13; x1++ {
					sub_rect := rect(x0, y0, x1, y1)
					sub := img.sub_image(sub_rect)
					for y := sub.rect.min.y; y < sub.rect.max.y; y++ {
						for x := sub.rect.min.x; x < sub.rect.max.x; x++ {
							assert img.ycbcr_at(x, y) == sub.ycbcr_at(x, y)
						}
					}
				}
			}
		}
	}
}

fn test_ycbcr_sub_images_from_go() {
	rects := [
		rect(0, 0, 16, 16),
		rect(1, 0, 16, 16),
		rect(0, 1, 16, 16),
		rect(1, 1, 16, 16),
		rect(1, 1, 15, 16),
		rect(1, 1, 16, 15),
		rect(1, 1, 15, 15),
		rect(2, 3, 14, 15),
		rect(7, 0, 7, 16),
		rect(0, 8, 16, 8),
		rect(0, 0, 10, 11),
		rect(5, 6, 16, 16),
		rect(7, 7, 8, 8),
		rect(7, 8, 8, 9),
		rect(8, 7, 9, 8),
		rect(8, 8, 9, 9),
		rect(7, 7, 17, 17),
		rect(8, 8, 17, 17),
		rect(9, 9, 17, 17),
		rect(10, 10, 17, 17),
	]
	subsample_ratios := [
		YCbCrSubsampleRatio.ratio_444,
		.ratio_422,
		.ratio_420,
		.ratio_440,
		.ratio_411,
		.ratio_410,
	]
	deltas := [
		pt(0, 0),
		pt(1000, 1001),
		pt(5001, -400),
		pt(-701, -801),
	]
	for r in rects {
		for ratio in subsample_ratios {
			for delta in deltas {
				check_ycbcr_sub_images(r, ratio, delta)
			}
		}
	}
}

fn test_ycbcr_slices_do_not_overlap_from_go() {
	mut img := new_ycbcr(rect(0, 0, 8, 8), .ratio_420)
	for i in 0 .. img.y.len {
		img.y[i] = 10
	}
	for i in 0 .. img.cb.len {
		img.cb[i] = 11
	}
	for i in 0 .. img.cr.len {
		img.cr[i] = 12
	}
	for got in img.y {
		assert got == 10
	}
	for got in img.cb {
		assert got == 11
	}
	for got in img.cr {
		assert got == 12
	}
}

fn test_16_bits_per_color_channel_from_go() {
	models := [
		color.rgba64_model,
		color.nrgba64_model,
		color.alpha16_model,
		color.gray16_model,
	]
	for model in models {
		c := model.convert(color.Color(color.RGBA64{
			r: 0x1234
			g: 0x1234
			b: 0x1234
			a: 0x1234
		}))!
		r, _, _, _ := c.rgba()
		assert r == 0x1234
	}

	mut rgba64 := new_rgba64(rect(0, 0, 10, 10))
	rgba64.set(1, 2, color.Color(color.NRGBA64{0xffff, 0xffff, 0xffff, 0x1357}))
	r, _, _, _ := rgba64.at(1, 2).rgba()
	assert r == 0x1357

	mut nrgba64 := new_nrgba64(rect(0, 0, 10, 10))
	nrgba64.set(1, 2, color.Color(color.NRGBA64{0xffff, 0xffff, 0xffff, 0x1357}))
	r2, _, _, _ := nrgba64.at(1, 2).rgba()
	assert r2 == 0x1357

	mut alpha16 := new_alpha16(rect(0, 0, 10, 10))
	alpha16.set(1, 2, color.Color(color.NRGBA64{0xffff, 0xffff, 0xffff, 0x1357}))
	r3, _, _, _ := alpha16.at(1, 2).rgba()
	assert r3 == 0x1357

	mut gray16 := new_gray16(rect(0, 0, 10, 10))
	gray16.set(1, 2, color.Color(color.NRGBA64{0xffff, 0xffff, 0xffff, 0x1357}))
	r4, _, _, _ := gray16.at(1, 2).rgba()
	assert r4 == 0x1357
}

fn test_rgba64_at_matches_at_rgba_from_go() {
	r := rect(0, 0, 3, 2)
	c := color.RGBA64{
		r: 0x7fff
		g: 0x3fff
		b: 0x0000
		a: 0x7fff
	}

	mut alpha := new_alpha(r)
	alpha.set_rgba64(1, 1, c)
	assert_rgba64_matches_at(alpha, 1, 1)

	mut alpha16 := new_alpha16(r)
	alpha16.set_rgba64(1, 1, c)
	assert_rgba64_matches_at(alpha16, 1, 1)

	mut cmyk := new_cmyk(r)
	cmyk.set_rgba64(1, 1, c)
	assert_rgba64_matches_at(cmyk, 1, 1)

	mut gray := new_gray(r)
	gray.set_rgba64(1, 1, c)
	assert_rgba64_matches_at(gray, 1, 1)

	mut gray16 := new_gray16(r)
	gray16.set_rgba64(1, 1, c)
	assert_rgba64_matches_at(gray16, 1, 1)

	mut nrgba := new_nrgba(r)
	nrgba.set_rgba64(1, 1, c)
	assert_rgba64_matches_at(nrgba, 1, 1)

	mut nrgba64 := new_nrgba64(r)
	nrgba64.set_rgba64(1, 1, c)
	assert_rgba64_matches_at(nrgba64, 1, 1)

	mut paletted := new_paletted(r, color.Palette{
		colors: [
			color.Color(c),
		]
	})
	paletted.set_rgba64(1, 1, c)
	assert_rgba64_matches_at(paletted, 1, 1)

	mut rgba := new_rgba(r)
	rgba.set_rgba64(1, 1, c)
	assert_rgba64_matches_at(rgba, 1, 1)

	mut rgba64 := new_rgba64(r)
	rgba64.set_rgba64(1, 1, c)
	assert_rgba64_matches_at(rgba64, 1, 1)

	uniform := new_uniform(color.Color(c))
	assert_rgba64_matches_at(uniform, 1, 1)

	mut ycbcr := new_ycbcr(r, .ratio_444)
	for i in 0 .. ycbcr.y.len {
		ycbcr.y[i] = 0x77
		ycbcr.cb[i] = 0x88
		ycbcr.cr[i] = 0x99
	}
	assert_rgba64_matches_at(ycbcr, 1, 1)

	mut nycbcra := new_nycbcra(r, .ratio_444)
	for i in 0 .. nycbcra.ycbcr.y.len {
		nycbcra.ycbcr.y[i] = 0x77
		nycbcra.ycbcr.cb[i] = 0x88
		nycbcra.ycbcr.cr[i] = 0x99
		nycbcra.a[i] = 0xaa
	}
	assert_rgba64_matches_at(nycbcra, 1, 1)

	assert_rgba64_matches_at(r, 1, 1)
}
