module canvas

// Typeface.from_data is unsupported without the CoreText text backend.
pub fn Typeface.from_data(data []u8) !&Typeface {
	return error('gg.canvas: typefaces require macOS CoreText')
}

// Typeface.from_file is unsupported without the CoreText text backend.
pub fn Typeface.from_file(path string) !&Typeface {
	return error('gg.canvas: typefaces require macOS CoreText')
}

// Typeface.from_ct_font is unsupported without the CoreText text backend.
pub fn Typeface.from_ct_font(ct_font voidptr) !&Typeface {
	return error('gg.canvas: typefaces require macOS CoreText')
}

// free is a no-op on platforms without native typefaces.
pub fn (mut t Typeface) free() {}

// clear_text_cache is a no-op on platforms without a text backend.
pub fn clear_text_cache() {}
