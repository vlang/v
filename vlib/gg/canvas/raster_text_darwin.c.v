module canvas

// draw_glyphs draws positioned glyphs: glyph i is placed at `origin + positions[i]`.
// Text drawing and typeface/cache cleanup must run serially.
pub fn (mut s RasterSurface) draw_glyphs(font Font, glyphs []u16, positions []Point, origin Point, paint Paint) ! {
	s.draw_glyph_run(font, glyphs, positions, origin, paint)!
}
