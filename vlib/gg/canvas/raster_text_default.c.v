module canvas

// draw_glyphs returns an unsupported-platform error without modifying pixels.
pub fn (mut s RasterSurface) draw_glyphs(font Font, glyphs []u16, positions []Point, origin Point, paint Paint) ! {
	return error('gg.canvas: raster text requires macOS CoreText or Linux FreeType')
}
