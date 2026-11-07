module canvas

// draw_glyphs draws positioned glyphs: glyph i is placed at `origin + positions[i]`.
// Text drawing and typeface/cache cleanup must run serially.
pub fn (mut s RasterSurface) draw_glyphs(font Font, glyphs []u16, positions []Point, origin Point, paint Paint) ! {
	if !is_finite_f32(font.size) || font.size < 0 || !is_finite_f32(font.scale_x)
		|| !is_finite_f32(font.skew_x) || !matrix_is_finite(RasterMatrix.from(s.matrix))
		|| !is_finite_f32(origin.x) || !is_finite_f32(origin.y) {
		return error('gg.canvas: invalid text size or transform')
	}
	if !is_finite_f32(s.props.text_gamma) || s.props.text_gamma < 0
		|| s.props.text_gamma >= 4 || !is_finite_f32(s.props.text_contrast)
		|| s.props.text_contrast < 0 || s.props.text_contrast > 1 {
		return error('gg.canvas: invalid text gamma or contrast')
	}
	s.draw_glyph_run(font, glyphs, positions, origin, paint)!
}
