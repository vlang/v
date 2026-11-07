module canvas

fn (mut rec ScalerRec) filter_rec(font Font) ! {
	if font.edging == .alias {
		return error('gg.canvas: aliased (BW) text is not supported yet')
	}
	// SkTypeface_Mac::onFilterRec().
	rec.flags &= ~(flag_force_autohinting | flag_lcd_bgr_order | flag_lcd_vertical)
	smooth := C.v_canvas_ct_smooth_behavior()
	if rec.hinting() != .none {
		rec.set_hinting(.normal)
	}
	if smooth == 0 {
		rec.set_hinting(.none)
	}
	if font.typeface.has_color_glyphs {
		return error('gg.canvas: color glyphs are not supported yet')
	}
	if rec.mask_format == mask_a8 && rec.hinting() == .none {
		// Skia ignores the pre-blend here unless SK_GAMMA_APPLY_TO_A8 is
		// defined; LibWeb's Skia defines it, so unhinted A8 keeps its gamma.
	} else {
		mut lum := rec.lum_bits
		r := lum >> 16 & 0xff
		g := lum >> 8 & 0xff
		b := lum & 0xff
		if smooth == 1 {
			lum = r * 1 / 2 << 16 | g * 1 / 2 << 8 | b * 1 / 2
		} else if smooth == 2 {
			lum = r * 3 / 4 << 16 | g * 3 / 4 << 8 | b * 3 / 4
		}
		rec.set_luminance_color(lum)
		rec.contrast = 0
	}
}

fn (mut s Strike) release_native() {
	C.v_canvas_ct_offscreen_free(s.offscreen)
	C.v_canvas_cf_release(s.native_font)
	s.offscreen = unsafe { nil }
	s.native_font = unsafe { nil }
}

// new_strike mirrors the SkScalerContext_Mac constructor.
fn new_strike(rec ScalerRec, typeface &Typeface) !&Strike {
	scale, sa, invertible := rec.compute_matrices_vertical()
	t := [f64(sa.m[mat_scale_x]), -f64(sa.m[mat_skew_y]), -f64(sa.m[mat_skew_x]),
		f64(sa.m[mat_scale_y]), f64(sa.m[mat_trans_x]), f64(sa.m[mat_trans_y])]!
	mut inv := [6]f64{}
	if invertible {
		C.v_canvas_cg_invert(&t[0], &inv[0])
	} else {
		inv = t
	}
	ct := C.v_canvas_ct_font_exact_copy(typeface.ct_font, f64(scale.y), 0, 0)
	if ct == unsafe { nil } {
		return error('gg.canvas: CoreText could not size the font')
	}
	offscreen := C.v_canvas_ct_offscreen_new()
	if offscreen == unsafe { nil } {
		C.v_canvas_cf_release(ct)
		return error('gg.canvas: could not allocate the glyph context')
	}
	pre_blend := rec.preblend()
	return &Strike{
		rec:         rec
		native_font: ct
		transform:   t
		inverse:     inv
		offscreen:   offscreen
		pre_blend:   pre_blend
		sub_pos:     rec.flags & flag_subpixel_positioning != 0
	}
}

// make_glyph mirrors SkScalerContext::internalMakeGlyph() with
// SkScalerContext_Mac::generateMetrics() and generateImage().
fn (mut s Strike) make_glyph(packed u32) !Glyph {
	mut g := Glyph{}
	glyph_id := packed_glyph_id(packed)
	mut advance := [2]f32{}
	mut cg := [4]f64{}
	if C.v_canvas_ct_glyph_metrics(s.native_font, &s.transform[0], glyph_id, &advance[0],
		&cg[0]) == 0 {
		g.advance_x = advance[0]
		g.advance_y = advance[1]
		return g
	}
	g.advance_x = advance[0]
	g.advance_y = advance[1]
	// SkRect::MakeXYWH(x, -y - h, w, h) from CGFloat values.
	x := f32(cg[0])
	y := f32(-cg[1] - cg[3])
	w := f32(cg[2])
	h := f32(cg[3])
	left := x
	top := y
	mut right := x + w
	mut bottom := y + h
	if s.sub_pos {
		right += sk_fixed_to_float(packed_sub_x_fixed(packed))
		bottom += sk_fixed_to_float(packed_sub_y_fixed(packed))
	}
	// roundOut, then outset by one pixel for CoreGraphics antialiasing.
	l := int(C.floorf(left)) - 1
	t := int(C.floorf(top)) - 1
	r := int(C.ceilf(right)) + 1
	b := int(C.ceilf(bottom)) + 1
	g.left = int(i16(l))
	g.top = int(i16(t))
	g.width = int(u16(r - l))
	g.height = int(u16(b - t))
	if g.width == 0 || g.height == 0 {
		g.left = 0
		g.top = 0
		g.width = 0
		g.height = 0
		return g
	}
	if g.width > max_glyph_dimension || g.height > max_glyph_dimension {
		return error('gg.canvas: glyph mask exceeds the text cache size limit')
	}
	s.generate_image(mut g, packed)
	return g
}

// The linearization of CoreGraphics' smoothed coverage (gamma 2.0).
const linear_from_cg_lcd = build_linear_from_cg_lcd()

fn build_linear_from_cg_lcd() []u8 {
	mut t := []u8{len: 256}
	for i in 0 .. 256 {
		t[i] = u8((i * i + 128) / 255)
	}
	return t
}

fn (mut s Strike) generate_image(mut g Glyph, packed u32) {
	request_smooth := s.rec.hinting() != .none
	mut do_aa := false
	mut do_lcd := false
	if s.rec.mask_format != mask_bw {
		do_lcd = true
		do_aa = true
	}
	if !request_smooth && s.rec.mask_format == mask_a8 {
		do_lcd = false
		do_aa = true
	}
	mut sub_x := f32(0)
	mut sub_y := f32(0)
	if s.sub_pos {
		sub_x = sk_fixed_to_float(packed_sub_x_fixed(packed))
		sub_y = sk_fixed_to_float(packed_sub_y_fixed(packed))
	}
	mut cg_row_bytes := u64(0)
	pixels := C.v_canvas_ct_draw_glyph(s.offscreen, s.native_font, &s.transform[0], &s.inverse[0],
		packed_glyph_id(packed), g.left, g.top, g.width, g.height, sub_x, sub_y, int(do_aa),
		int(do_lcd), &cg_row_bytes)
	stride := int(cg_row_bytes / 4)
	linearize := s.rec.mask_format == mask_a8 && request_smooth
		&& C.v_canvas_ct_smooth_behavior() != 0
	g.image = []u8{len: g.width * g.height}
	for y in 0 .. g.height {
		row := unsafe { pixels + y * stride }
		for x in 0 .. g.width {
			mut p := unsafe { row[x] }
			if linearize {
				p = u32(linear_from_cg_lcd[(p >> 16) & 0xff]) << 16 | u32(linear_from_cg_lcd[(p >> 8) & 0xff]) << 8 | u32(linear_from_cg_lcd[p & 0xff])
			}
			r := 0xff - ((p >> 16) & 0xff)
			gr := 0xff - ((p >> 8) & 0xff)
			b := 0xff - (p & 0xff)
			mut lum := compute_luminance(r, gr, b)
			if s.pre_blend.len > 0 {
				lum = s.pre_blend[lum]
			}
			g.image[y * g.width + x] = u8(lum)
		}
	}
	g.has_image = true
}

fn C.v_canvas_ct_smooth_behavior() int
fn C.v_canvas_ct_font_exact_copy(base voidptr, size f64, opsz_set int, opsz f64) voidptr
fn C.v_canvas_cg_invert(t &f64, out &f64)
fn C.v_canvas_ct_offscreen_new() voidptr
fn C.v_canvas_ct_offscreen_free(offscreen voidptr)
fn C.v_canvas_ct_glyph_metrics(font voidptr, transform &f64, glyph u16, advance &f32, bounds &f64) int
fn C.v_canvas_ct_draw_glyph(offscreen voidptr, font voidptr, transform &f64, inverse &f64, glyph u16, left int, top int, width int, height int, sub_x f32, sub_y f32, do_aa int, do_lcd int, row_bytes &u64) &u32
