module canvas

fn C.v_canvas_ft_load_glyph(font voidptr, size f64, transform &f64, glyph u16, sub_x f32, sub_y f32, hinting int, monochrome int, embedded_bitmaps int, linear_metrics int, max_dimension int, bounds &i32, advance &f32) int
fn C.v_canvas_ft_copy_bitmap(font voidptr, output &u8)

fn (mut rec ScalerRec) filter_rec(font Font) ! {
	if font.typeface.has_color_glyphs {
		return error('gg.canvas: color glyphs are not supported yet')
	}
	// An unknown pixel geometry has a single coverage channel, including when
	// the caller requests subpixel antialiasing. It must not produce LCD stripes.
	if rec.mask_format == mask_a8 && rec.hinting() == .full {
		rec.set_hinting(.normal)
	}
}

fn new_strike(rec ScalerRec, typeface &Typeface) !&Strike {
	scale, sa, invertible := rec.compute_matrices_vertical()
	return &Strike{
		rec:         rec
		native_font: typeface.ft_font
		native_size: if invertible { scale.y } else { 0 }
		transform:   [f64(sa.m[mat_scale_x]), -f64(sa.m[mat_skew_y]), -f64(sa.m[mat_skew_x]),
			f64(sa.m[mat_scale_y]), f64(0), f64(0)]!
		pre_blend:   if rec.mask_format == mask_bw { []u8{} } else { rec.preblend() }
		sub_pos:     rec.flags & flag_subpixel_positioning != 0
	}
}

fn (mut s Strike) release_native() {
	// Typeface owns this face and evicts its strikes before releasing it.
	s.native_font = unsafe { nil }
}

fn (mut s Strike) make_glyph(packed u32) !Glyph {
	if s.native_size == 0 {
		return Glyph{}
	}
	// The native bridge writes C int values, including when V int is 64-bit.
	mut bounds := [4]i32{}
	mut advance := [2]f32{}
	sub_x := if s.sub_pos { sk_fixed_to_float(packed_sub_x_fixed(packed)) } else { f32(0) }
	sub_y := if s.sub_pos { sk_fixed_to_float(packed_sub_y_fixed(packed)) } else { f32(0) }
	status := C.v_canvas_ft_load_glyph(s.native_font, f64(s.native_size), &s.transform[0],
		packed_glyph_id(packed), sub_x, sub_y, int(s.rec.hinting()),
		int(s.rec.mask_format == mask_bw), int(s.rec.flags & flag_embedded_bitmap_text != 0),
		int(s.rec.flags & flag_linear_metrics != 0), max_glyph_dimension, &bounds[0], &advance[0])
	if status == -2 {
		return error('gg.canvas: glyph mask exceeds the text cache size limit')
	}
	if status == -3 {
		return error('gg.canvas: FreeType returned an unsupported glyph pixel format')
	}
	if status != 0 {
		return error('gg.canvas: FreeType could not render glyph ${packed_glyph_id(packed)} (error ${status})')
	}
	mut glyph := Glyph{
		left:      int(bounds[0])
		top:       int(bounds[1])
		width:     int(bounds[2])
		height:    int(bounds[3])
		advance_x: advance[0]
		advance_y: advance[1]
		image:     []u8{len: bounds[2] * bounds[3]}
		has_image: true
	}
	if glyph.image.len != 0 {
		C.v_canvas_ft_copy_bitmap(s.native_font, glyph.image.data)
		if s.pre_blend.len != 0 {
			for i, coverage in glyph.image {
				glyph.image[i] = s.pre_blend[coverage]
			}
		}
	}
	return glyph
}
