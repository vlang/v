module image

import image.color

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
