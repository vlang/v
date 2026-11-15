module canvas

// Typeface.from_data is unsupported without a native text backend.
pub fn Typeface.from_data(data []u8) !&Typeface {
	return error('gg.canvas: typefaces require macOS CoreText or Linux FreeType')
}

// Typeface.from_file is unsupported without a native text backend.
pub fn Typeface.from_file(path string) !&Typeface {
	return error('gg.canvas: typefaces require macOS CoreText or Linux FreeType')
}

// Typeface.from_ct_font requires the macOS CoreText backend.
pub fn Typeface.from_ct_font(ct_font voidptr) !&Typeface {
	return error('gg.canvas: CTFont wrapping requires macOS CoreText')
}

// free is a no-op on platforms without native typefaces.
pub fn (mut t Typeface) free() {}
