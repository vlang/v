module image

import image.color

// A width of 4 and a height of 4, so `opaque` has to look at 16 pixels across
// more than one row to prove it walks the stride rather than the first row.
const types_rect = rect(1, 2, 5, 6)

fn opaque_uniform() color.Color {
	return color.Color(color.RGBA{
		r: 1
		g: 2
		b: 3
		a: 255
	})
}

fn test_uniform_constants_and_helpers() {
	br, bg, bb, ba := black.rgba()
	assert br == 0 && bg == 0 && bb == 0 && ba == 0xffff
	wr, wg, wb, wa := white.rgba()
	assert wr == 0xffff && wg == 0xffff && wb == 0xffff && wa == 0xffff
	tr, tg, tb, ta := transparent.rgba()
	assert tr == 0 && tg == 0 && tb == 0 && ta == 0
	or_, og, ob, oa := opaque.rgba()
	assert or_ == 0xffff && og == 0xffff && ob == 0xffff && oa == 0xffff

	u := new_uniform(opaque_uniform())
	r, g, b, a := u.rgba()
	assert r == 0x0101
	assert g == 0x0202
	assert b == 0x0303
	assert a == 0xffff
	assert u.rgba64_at(-7, 9) == color.RGBA64{
		r: 0x0101
		g: 0x0202
		b: 0x0303
		a: 0xffff
	}
	assert u.at(-100, 100) == opaque_uniform()
	assert u.at(0, 0) == opaque_uniform()
	assert u.convert(color.opaque) == opaque_uniform()
	assert u.opaque()
	assert !transparent.opaque()
	assert opaque.opaque()
	assert u.bounds() == rect(-1_000_000_000, -1_000_000_000, 1_000_000_000, 1_000_000_000)

	// The uniform model maps every input onto the uniform color.
	model := u.color_model()
	assert model.convert(color.opaque)! == opaque_uniform()
	assert model.convert(color.transparent)! == opaque_uniform()
	assert transparent.color_model().convert(color.opaque)! == color.transparent
}

fn test_uniform_color_model_of_the_named_constants() {
	assert black.color_model().convert(color.opaque)! == color.Color(color.Gray16{})
	assert white.color_model().convert(color.opaque)! == color.Color(color.Gray16{
		y: 0xffff
	})
}

fn test_rgba_out_of_bounds_is_a_no_op() {
	mut img := new_rgba(types_rect)
	assert img.rgba_at(0, 0) == color.RGBA{}
	assert img.rgba_at(100, 100) == color.RGBA{}
	assert img.at(0, 0) == color.Color(color.RGBA{})
	assert img.rgba64_at(0, 0) == color.RGBA64{}
	// A fresh image is zeroed, so every alpha channel is 0 and it is not opaque.
	assert !img.opaque()

	img.set(0, 0, color.opaque)
	img.set_rgba(100, 100, color.RGBA{
		r: 1
		g: 2
		b: 3
		a: 4
	})
	img.set_rgba64(100, 100, color.RGBA64{
		r: 1
		g: 2
		b: 3
		a: 4
	})
	assert img.pix[0] == 0
	assert img.pix.len == 64
}

fn test_rgba_opaque_walks_every_row() {
	mut img := new_rgba(types_rect)
	for y in types_rect.min.y .. types_rect.max.y {
		for x in types_rect.min.x .. types_rect.max.x {
			img.set_rgba(x, y, color.RGBA{
				r: x
				g: y
				b: 0
				a: 0xff
			})
		}
	}
	assert img.opaque()
	img.set_rgba(types_rect.max.x - 1, types_rect.max.y - 1, color.RGBA{
		r: 0
		g: 0
		b: 0
		a: 0xfe
	})
	assert !img.opaque()
}

fn test_rgba_sub_image_of_a_disjoint_rectangle_is_empty() {
	mut img := new_rgba(types_rect)
	img.set_rgba(1, 2, color.RGBA{
		r: 9
		g: 8
		b: 7
		a: 255
	})
	sub := img.sub_image(rect(100, 100, 110, 110))
	assert sub.rect == Rectangle{}
	assert sub.pix.len == 0
	assert sub.stride == 0
	assert sub.opaque()
}

fn test_empty_images_are_opaque() {
	assert new_rgba(Rectangle{}).opaque()
	assert new_rgba64(Rectangle{}).opaque()
	assert new_nrgba(Rectangle{}).opaque()
	assert new_nrgba64(Rectangle{}).opaque()
	assert new_alpha(Rectangle{}).opaque()
	assert new_alpha16(Rectangle{}).opaque()
	assert new_cmyk(Rectangle{}).opaque()
	assert new_gray(Rectangle{}).opaque()
	assert new_gray16(Rectangle{}).opaque()
	assert new_ycbcr(Rectangle{}, .ratio_420).opaque()
	assert new_nycbcra(Rectangle{}, .ratio_420).opaque()
	assert new_paletted(Rectangle{}, color.Palette{
		colors: [
			color.opaque,
		]
	}).opaque()
}

fn test_rgba64_stores_big_endian_channels() {
	mut img := new_rgba64(types_rect)
	assert img.rgba64_at(1, 2) == color.RGBA64{}
	assert img.at(9, 9) == color.Color(color.RGBA64{})

	img.set_rgba64(1, 2, color.RGBA64{
		r: 0x1234
		g: 0x5678
		b: 0x9abc
		a: 0xdef0
	})
	assert img.rgba64_at(1, 2) == color.RGBA64{
		r: 0x1234
		g: 0x5678
		b: 0x9abc
		a: 0xdef0
	}
	i := img.pix_offset(1, 2)
	assert img.pix[i] == 0x12
	assert img.pix[i + 1] == 0x34
	assert img.pix[i + 6] == 0xde
	assert img.pix[i + 7] == 0xf0

	sub := img.sub_image(rect(200, 200, 210, 210))
	assert sub.rect == Rectangle{}
	assert sub.pix.len == 0
}

fn test_rgba64_opaque_needs_both_bytes_of_alpha() {
	mut img := new_rgba64(types_rect)
	for y in types_rect.min.y .. types_rect.max.y {
		for x in types_rect.min.x .. types_rect.max.x {
			img.set_rgba64(x, y, color.RGBA64{
				r: 0
				g: 0
				b: 0
				a: 0xffff
			})
		}
	}
	assert img.opaque()

	// A single 0xfe00 in the high byte alone still reads as "not opaque".
	img.set_rgba64(types_rect.max.x - 1, types_rect.max.y - 1, color.RGBA64{
		a: 0xfffe
	})
	assert !img.opaque()
}

fn test_nrgba_unpremultiplies_when_storing_rgba64() {
	mut img := new_nrgba(types_rect)
	assert img.nrgba_at(0, 0) == color.NRGBA{}
	img.set_rgba64(1, 2, color.RGBA64{
		r: 0x8000
		g: 0x8000
		b: 0x8000
		a: 0x8000
	})
	assert img.nrgba_at(1, 2) == color.NRGBA{
		r: 0xff
		g: 0xff
		b: 0xff
		a: 0x80
	}
	assert img.rgba64_at(1, 2) == color.RGBA64{
		r: 0x8080
		g: 0x8080
		b: 0x8080
		a: 0x8080
	}
	assert !img.opaque()
	assert img.sub_image(Rectangle{}).rect == Rectangle{}
}

fn test_nrgba64_unpremultiplies_when_storing_rgba64() {
	mut img := new_nrgba64(types_rect)
	img.set_rgba64(1, 2, color.RGBA64{
		r: 0x4000
		g: 0x8000
		b: 0xc000
		a: 0x8000
	})
	// The un-premultiplied blue channel is 0x18002 and is truncated to u16.
	assert img.nrgba64_at(1, 2) == color.NRGBA64{
		r: 0x7fff
		g: 0xffff
		b: 0x7ffe
		a: 0x8000
	}
}

fn test_alpha_images() {
	mut img := new_alpha(types_rect)
	assert img.color_model() == color.alpha_model
	assert img.alpha_at(0, 0) == color.Alpha{}
	assert img.rgba64_at(0, 0) == color.RGBA64{}
	img.set_rgba64(1, 2, color.RGBA64{
		r: 0x1234
		g: 0x1234
		b: 0x1234
		a: 0x5678
	})
	assert img.alpha_at(1, 2) == color.Alpha{
		a: 0x56
	}
	assert img.rgba64_at(1, 2) == color.RGBA64{
		r: 0x5656
		g: 0x5656
		b: 0x5656
		a: 0x5656
	}
	assert !img.opaque()
	assert img.sub_image(Rectangle{}).rect == Rectangle{}
}

fn test_alpha16_images() {
	mut img := new_alpha16(types_rect)
	assert img.color_model() == color.alpha16_model
	assert img.alpha16_at(0, 0) == color.Alpha16{}
	img.set_alpha16(1, 2, color.Alpha16{
		a: 0xabcd
	})
	assert img.alpha16_at(1, 2) == color.Alpha16{
		a: 0xabcd
	}
	assert img.rgba64_at(1, 2) == color.RGBA64{
		r: 0xabcd
		g: 0xabcd
		b: 0xabcd
		a: 0xabcd
	}
	assert !img.opaque()
	assert img.sub_image(Rectangle{}).rect == Rectangle{}
}

fn test_gray_images_are_always_opaque() {
	mut img := new_gray(types_rect)
	assert img.color_model() == color.gray_model
	assert img.opaque()
	assert img.gray_at(9, 9) == color.Gray{}
	img.set_gray(1, 2, color.Gray{
		y: 0x80
	})
	assert img.gray_at(1, 2) == color.Gray{
		y: 0x80
	}
	assert img.rgba64_at(1, 2) == color.RGBA64{
		r: 0x8080
		g: 0x8080
		b: 0x8080
		a: 0xffff
	}
	assert img.opaque()
	assert img.sub_image(Rectangle{}).rect == Rectangle{}
}

fn test_gray16_images_are_always_opaque() {
	mut img := new_gray16(types_rect)
	assert img.color_model() == color.gray16_model
	assert img.opaque()
	assert img.gray16_at(9, 9) == color.Gray16{}
	assert img.rgba64_at(1, 2) == color.RGBA64{
		a: 0xffff
	}
	img.set_gray16(1, 2, color.Gray16{
		y: 0xbeef
	})
	assert img.gray16_at(1, 2) == color.Gray16{
		y: 0xbeef
	}
	assert img.opaque()
	assert img.sub_image(Rectangle{}).rect == Rectangle{}
}

fn test_cmyk_images() {
	mut img := new_cmyk(types_rect)
	assert img.color_model() == color.cmyk_model
	assert img.cmyk_at(0, 0) == color.CMYK{}
	// CMYK{0,0,0,0} is white: no ink on white paper.
	assert img.rgba64_at(0, 0) == color.RGBA64{
		r: 0xffff
		g: 0xffff
		b: 0xffff
		a: 0xffff
	}
	img.set_cmyk(1, 2, color.CMYK{
		c: 1
		m: 2
		y: 3
		k: 4
	})
	assert img.cmyk_at(1, 2) == color.CMYK{
		c: 1
		m: 2
		y: 3
		k: 4
	}
	assert img.rgba64_at(1, 2).a == 0xffff
	assert img.opaque()
	assert img.sub_image(Rectangle{}).rect == Rectangle{}
}

fn test_paletted_image_edges() {
	empty := new_paletted(types_rect, color.Palette{})
	assert empty.at(1, 2) == color.transparent
	assert empty.color_index_at(1, 2) == 0
	assert empty.color_index_at(100, 100) == 0
	assert empty.opaque()
	assert empty.sub_image(Rectangle{}).palette.colors.len == 0

	pal := color.Palette{
		colors: [
			color.opaque,
			color.transparent,
		]
	}
	mut img := new_paletted(types_rect, pal)
	assert img.color_model() == color.Model(pal)
	assert img.color_index_at(100, 100) == 0
	img.set_color_index(1, 2, 1)
	assert img.at(1, 2) == color.transparent
	assert img.color_index_at(1, 2) == 1
	assert !img.opaque()
	img.set_color_index(1, 2, 0)
	img.set_color_index(2, 2, 1)
	assert !img.opaque()
	img.set_color_index(2, 2, 0)
	// Every referenced palette entry is now opaque.
	for y in types_rect.min.y .. types_rect.max.y {
		for x in types_rect.min.x .. types_rect.max.x {
			img.set_color_index(x, y, 0)
		}
	}
	assert img.opaque()

	// set stores the nearest palette index.
	img.set(3, 3, color.opaque)
	assert img.color_index_at(3, 3) == 0
	img.set(3, 3, color.transparent)
	assert img.color_index_at(3, 3) == 1

	sub := img.sub_image(Rectangle{})
	assert sub.rect == Rectangle{}
	assert sub.palette.colors.len == 2
	assert sub.opaque()
}

fn test_ycbcr_subsample_ratio_names() {
	assert YCbCrSubsampleRatio.ratio_444.str() == 'YCbCrSubsampleRatio444'
	assert YCbCrSubsampleRatio.ratio_422.str() == 'YCbCrSubsampleRatio422'
	assert YCbCrSubsampleRatio.ratio_420.str() == 'YCbCrSubsampleRatio420'
	assert YCbCrSubsampleRatio.ratio_440.str() == 'YCbCrSubsampleRatio440'
	assert YCbCrSubsampleRatio.ratio_411.str() == 'YCbCrSubsampleRatio411'
	assert YCbCrSubsampleRatio.ratio_410.str() == 'YCbCrSubsampleRatio410'
}

// A 4x4 rectangle gives every ratio a non-trivial chroma plane, so the strides
// and buffer sizes are checked per ratio rather than only for the default.
fn test_ycbcr_buffer_layout_per_subsample_ratio() {
	mut sizes := map[string]int{}
	sizes['YCbCrSubsampleRatio444'] = 16
	sizes['YCbCrSubsampleRatio422'] = 12
	sizes['YCbCrSubsampleRatio420'] = 6
	sizes['YCbCrSubsampleRatio440'] = 8
	sizes['YCbCrSubsampleRatio411'] = 8
	sizes['YCbCrSubsampleRatio410'] = 4
	mut c_strides := map[string]int{}
	c_strides['YCbCrSubsampleRatio444'] = 4
	c_strides['YCbCrSubsampleRatio422'] = 3
	c_strides['YCbCrSubsampleRatio420'] = 3
	c_strides['YCbCrSubsampleRatio440'] = 4
	c_strides['YCbCrSubsampleRatio411'] = 2
	c_strides['YCbCrSubsampleRatio410'] = 2

	for ratio in [YCbCrSubsampleRatio.ratio_444, .ratio_422, .ratio_420, .ratio_440, .ratio_411,
		.ratio_410] {
		name := ratio.str()
		mut img := new_ycbcr(types_rect, ratio)
		assert img.y.len == 16
		assert img.y_stride == 4
		assert img.cb.len == sizes[name]
		assert img.cr.len == sizes[name]
		assert img.c_stride == c_strides[name]
		assert img.color_model() == color.ycbcr_model
		assert img.bounds() == types_rect
		assert img.opaque()
		assert img.ycbcr_at(0, 0) == color.YCbCr{}

		sub := img.sub_image(Rectangle{})
		assert sub.rect == Rectangle{}
		assert sub.subsample_ratio == ratio
		assert sub.c_stride == 0
		assert sub.y.len == 0

		mut with_alpha := new_nycbcra(types_rect, ratio)
		assert with_alpha.color_model() == color.nycbcra_model
		assert with_alpha.bounds() == types_rect
		assert with_alpha.a.len == 16
		assert with_alpha.a_stride == 4
		// types_rect.min is (1,2), so the alpha offset of the origin is 0.
		assert with_alpha.a_offset(1, 2) == 0
		assert with_alpha.a_offset(2, 2) == 1
		assert with_alpha.a_offset(1, 3) == 4
		assert with_alpha.nycbcra_at(0, 0) == color.NYCbCrA{}
		assert !with_alpha.opaque()
		alpha_sub := with_alpha.sub_image(Rectangle{})
		assert alpha_sub.ycbcr.rect == Rectangle{}
		assert alpha_sub.ycbcr.subsample_ratio == ratio
		assert alpha_sub.a.len == 0
	}
}

fn test_nycbcra_opaque_walks_the_alpha_plane() {
	mut img := new_nycbcra(types_rect, .ratio_444)
	for y in types_rect.min.y .. types_rect.max.y {
		for x in types_rect.min.x .. types_rect.max.x {
			img.a[img.a_offset(x, y)] = 0xff
		}
	}
	assert img.opaque()
	img.a[img.a_offset(types_rect.max.x - 1, types_rect.max.y - 1)] = 0xfe
	assert !img.opaque()
}
