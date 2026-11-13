module canvas

import os

#pkgconfig freetype2
#include "@DIR/text_freetype_linux.h"

fn C.v_canvas_ft_font_from_data(data &u8, length u64) voidptr
fn C.v_canvas_ft_font_free(font voidptr)
fn C.v_canvas_ft_font_has_color(font voidptr) int
fn C.v_canvas_ft_font_is_scalable(font voidptr) int

// Typeface.from_data loads the first face of TrueType/OpenType font data.
// The face owns a copy of the bytes; the caller can release or reuse its array.
pub fn Typeface.from_data(data []u8) !&Typeface {
	if data.len == 0 {
		return error('gg.canvas: empty font data')
	}
	ft := C.v_canvas_ft_font_from_data(data.data, u64(data.len))
	if ft == unsafe { nil } {
		return error('gg.canvas: FreeType cannot load the font data')
	}
	if C.v_canvas_ft_font_is_scalable(ft) == 0 {
		C.v_canvas_ft_font_free(ft)
		return error('gg.canvas: Linux text requires a scalable font')
	}
	mut ids := unsafe { typeface_ids }
	id := ids.next
	ids.next++
	return &Typeface{
		id:               id
		ft_font:          ft
		has_color_glyphs: C.v_canvas_ft_font_has_color(ft) != 0
	}
}

// Typeface.from_file loads the first face of a font file.
pub fn Typeface.from_file(path string) !&Typeface {
	data := os.read_bytes(path)!
	defer {
		unsafe { data.free() }
	}
	return Typeface.from_data(data)
}

// Typeface.from_ct_font requires macOS CoreText; Linux accepts font data or files.
pub fn Typeface.from_ct_font(ct_font voidptr) !&Typeface {
	return error('gg.canvas: CTFont wrapping requires macOS CoreText')
}

// free releases this face's font bytes, FreeType resources and cached strikes.
// It is idempotent; drawing and cleanup must be serial, and drawing after free fails.
pub fn (mut t Typeface) free() {
	if t.ft_font == unsafe { nil } {
		return
	}
	release_typeface_strikes(t.id)
	C.v_canvas_ft_font_free(t.ft_font)
	t.ft_font = unsafe { nil }
}
