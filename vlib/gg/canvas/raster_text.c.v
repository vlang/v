module canvas

// draw_glyph_run shares positioning and compositing across the native rasterizers.
fn (mut s RasterSurface) draw_glyph_run(font Font, glyphs []u16, positions []Point, origin Point, paint Paint) ! {
	if glyphs.len != positions.len {
		return error('gg.canvas: ${glyphs.len} glyphs but ${positions.len} positions')
	}
	if font.typeface == unsafe { nil } {
		return error('gg.canvas: font without a typeface')
	}
	if !font.typeface.is_live() {
		return error('gg.canvas: typeface has been freed')
	}
	if paint.style != .fill {
		return error('gg.canvas: stroked text is not supported yet')
	}
	if paint.gradient != none {
		return error('gg.canvas: gradient text is not supported yet')
	}
	color := paint.color.to_sk_color()!
	if color >> 24 == 0 {
		// SkPaint::nothingToDraw() for src-over.
		return
	}
	mut position_matrix := RasterMatrix.from(s.matrix)
	if position_matrix.has_perspective() {
		return error('gg.canvas: perspective text is not supported')
	}
	position_matrix.pre_translate(origin.x, origin.y)
	if should_draw_as_path(font, position_matrix) {
		return error('gg.canvas: glyphs larger than 256px need path rendering (not supported yet)')
	}
	rec := make_rec(font, color, s.props, position_matrix)!
	mut strike := find_or_create_strike(rec, font.typeface)!

	// SkGlyphPositionRoundingSpec for the strike.
	is_subpixel := rec.flags & flag_subpixel_positioning != 0
	axis := rec.axis_alignment_for_h_text()
	mut half_x := f32(0.5)
	mut half_y := f32(0.5)
	if is_subpixel {
		match axis {
			.x { half_x = subpixel_round }
			.y { half_y = subpixel_round }
			.none { half_x, half_y = subpixel_round, subpixel_round }
		}
	}
	mask_x := if is_subpixel && axis != .y { u32(3) } else { u32(0) }
	mask_y := if is_subpixel && axis != .x { u32(3) << 18 } else { u32(0) }
	mut rounding := position_matrix
	rounding.post_translate(half_x, half_y)
	for i, glyph_id in glyphs {
		pos := positions[i]
		if !is_finite_f32(pos.x) || !is_finite_f32(pos.y) {
			continue
		}
		mapped := rounding.map_point(pos)
		packed := pack_glyph_id(glyph_id, mapped, mask_x, mask_y)
		g := strike.glyph(packed)!
		if g.is_empty() || !g.has_image {
			continue
		}
		rx := C.floorf(mapped.x)
		ry := C.floorf(mapped.y)
		if !check_glyph_position(rx, ry) {
			continue
		}
		s.blit_glyph_mask(g, int(rx), int(ry), color)
	}
}

enum AxisAlignment {
	none
	x
	y
}

// SkPackedGlyphID::kSubpixelRound: 1/8.
const subpixel_round = f32(1) / f32(8)

// axis_alignment_for_h_text mirrors SkScalerContextRec::computeAxisAlignmentForHText().
fn (r &ScalerRec) axis_alignment_for_h_text() AxisAlignment {
	if r.flags & flag_baseline_snap == 0 {
		return .none
	}
	if r.post2x2[2] == 0 {
		return .x
	}
	if r.post2x2[0] == 0 {
		return .y
	}
	return .none
}

// pack_glyph_id mirrors SkPackedGlyphID::PackIDSkPoint().
fn pack_glyph_id(glyph u16, pt Point, mask_x u32, mask_y u32) u32 {
	magic_x := f32(1 << 2)
	magic_y := f32(1 << 20)
	x := (pt.x - C.floorf(pt.x)) + f32(1)
	y := (pt.y - C.floorf(pt.y)) + f32(1)
	sub_x := u32(int(x * magic_x)) & mask_x
	sub_y := u32(int(y * magic_y)) & mask_y
	return u32(glyph) << 2 | sub_x | sub_y
}

// should_draw_as_path mirrors SkStrikeSpec::ShouldDrawAsPath() for fills.
fn should_draw_as_path(font Font, view RasterMatrix) bool {
	mut text := make_text_matrix(font.size, font.scale_x, font.skew_x)
	text.post_concat(view)
	max_size_squared := f32(256 * 256)
	t := text.m
	ax := t[mat_scale_x] * t[mat_scale_x]
	ay := t[mat_skew_y] * t[mat_skew_y]
	bx := t[mat_skew_x] * t[mat_skew_x]
	by := t[mat_scale_y] * t[mat_scale_y]
	return ax + ay > max_size_squared || bx + by > max_size_squared
}

// check_glyph_position mirrors SkDraw_text.cpp's check_glyph_position().
fn check_glyph_position(x f32, y f32) bool {
	max := f32(2147483647 - (32767 + 65535))
	min := f32(-2147483648 - (-32768))
	return !(!(x <= max) || !(x >= min) || !(y <= max) || !(y >= min))
}

// blit_glyph_mask mirrors skcpu::Draw::paintMasks() for an A8 mask clipped
// to the device, then SkRasterPipelineBlitter::blitMask(): this Skia build's
// kN32 is RGBA, so BGRA destinations always use the raster pipeline.
fn (mut s RasterSurface) blit_glyph_mask(g &Glyph, x int, y int, color u32) {
	left := x + g.left
	top := y + g.top
	right := left + g.width
	bottom := top + g.height
	cl := if left > 0 { left } else { 0 }
	ct := if top > 0 { top } else { 0 }
	cr := if right < s.width { right } else { s.width }
	cb := if bottom < s.height { bottom } else { s.height }
	if cl >= cr || ct >= cb {
		return
	}
	src := PipelineColor.from_sk_color(color)
	for row in ct .. cb {
		mut d := row * s.width + cl
		mut m := (row - top) * g.width + (cl - left)
		for _ in cl .. cr {
			s.pixels[d] = src.blit_a8(s.pixels[d], u32(g.image[m]))
			d++
			m++
		}
	}
}

// PipelineColor is a paint color as SkRasterPipelineBlitter prepares it: the
// premultiplied float color stored in 16-bit slots for the lowp
// uniform_color stage, and whether src-over reduces to src.
struct PipelineColor {
	r      u32
	g      u32
	b      u32
	a      u32
	opaque bool
}

// PipelineColor.from_sk_color mirrors SkPaint::getColor4f() (SkColor4f::FromColor),
// SkRGBA4f::premul() and SkRasterPipeline::appendConstantColor().
fn PipelineColor.from_sk_color(color u32) PipelineColor {
	c := Color4f.from_rgba8(u8(color >> 16), u8(color >> 8), u8(color), u8(color >> 24))
	pm := c.premul()
	return PipelineColor{
		r:      lowp_from_float(pm.r)
		g:      lowp_from_float(pm.g)
		b:      lowp_from_float(pm.b)
		a:      lowp_from_float(pm.a)
		opaque: pm.a == 1
	}
}

// lowp_from_float mirrors appendConstantColor()'s `(uint16_t)(c * 255 + 0.5)`.
fn lowp_from_float(c f32) u32 {
	scaled := c * f32(255)
	return u32(scaled + f32(0.5))
}

// div255 mirrors the NEON lowp div255(): (v + 128 + ((v + 128) >> 8)) >> 8.
@[inline]
fn div255(v u32) u32 {
	t := v + ((v + 128) >> 8)
	return (t + 128) >> 8
}

// lowp_lerp mirrors lowp lerp(): div255(from * (255 - t) + to * t).
@[inline]
fn lowp_lerp(from u32, to u32, t u32) u32 {
	return div255(from * (255 - t) + to * t)
}

// blit_a8 runs one pixel of the A8 mask pipeline on a BGRA8 premultiplied
// pixel: an opaque color blends as src (load_8888_dst, lerp_u8), otherwise
// coverage pre-scales the source (scale_u8) before src-over; store_8888
// clamps each channel to 255.
fn (p &PipelineColor) blit_a8(dst u32, c u32) u32 {
	db := dst & 0xff
	dg := dst >> 8 & 0xff
	dr := dst >> 16 & 0xff
	da := dst >> 24
	mut r, mut g, mut b, mut a := u32(0), u32(0), u32(0), u32(0)
	if p.opaque {
		r = lowp_lerp(dr, p.r, c)
		g = lowp_lerp(dg, p.g, c)
		b = lowp_lerp(db, p.b, c)
		a = lowp_lerp(da, p.a, c)
	} else {
		sr := div255(p.r * c)
		sg := div255(p.g * c)
		sb := div255(p.b * c)
		sa := div255(p.a * c)
		inv := 255 - sa
		r = sr + div255(dr * inv)
		g = sg + div255(dg * inv)
		b = sb + div255(db * inv)
		a = sa + div255(da * inv)
	}
	return min_u32(a, 255) << 24 | min_u32(r, 255) << 16 | min_u32(g, 255) << 8 | min_u32(b, 255)
}

@[inline]
fn min_u32(a u32, b u32) u32 {
	return if a < b { a } else { b }
}
