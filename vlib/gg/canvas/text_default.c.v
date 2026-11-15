module canvas

fn (mut rec ScalerRec) filter_rec(font Font) ! {
	return error('gg.canvas: raster text requires macOS CoreText or Linux FreeType')
}

fn new_strike(rec ScalerRec, typeface &Typeface) !&Strike {
	return error('gg.canvas: raster text requires macOS CoreText or Linux FreeType')
}

fn (mut s Strike) release_native() {}

fn (mut s Strike) make_glyph(packed u32) !Glyph {
	return error('gg.canvas: raster text requires macOS CoreText or Linux FreeType')
}
