module canvas

import os

#flag darwin -framework CoreText -framework CoreGraphics -framework CoreFoundation
#include "@DIR/text_coretext_darwin.h"

fn C.v_canvas_ct_font_from_data(bytes &u8, length u64) voidptr
fn C.v_canvas_ct_font_traits(font voidptr) u32
fn C.v_canvas_cf_release(ref voidptr)
fn C.v_canvas_cf_retain(ref voidptr) voidptr

// kCTFontColorGlyphsTrait
const ct_color_glyphs_trait = u32(1) << 13

struct TypefaceIds {
mut:
	next u32 = 1
}

const typeface_ids = &TypefaceIds{}

// Typeface.from_data loads the first face of TrueType/OpenType font data.
pub fn Typeface.from_data(data []u8) !&Typeface {
	if data.len == 0 {
		return error('gg.canvas: empty font data')
	}
	ct := C.v_canvas_ct_font_from_data(data.data, u64(data.len))
	if ct == unsafe { nil } {
		return error('gg.canvas: CoreText cannot load the font data')
	}
	mut ids := unsafe { typeface_ids }
	id := ids.next
	ids.next++
	return &Typeface{
		id:               id
		ct_font:          ct
		has_color_glyphs: C.v_canvas_ct_font_traits(ct) & ct_color_glyphs_trait != 0
	}
}

// Typeface.from_file loads the first face of a font file.
pub fn Typeface.from_file(path string) !&Typeface {
	data := os.read_bytes(path)!
	return Typeface.from_data(data)
}

// Typeface.from_ct_font wraps (and retains) an existing CTFontRef, like
// SkMakeTypefaceFromCTFont(). Glyphs are sized from copies of this font.
pub fn Typeface.from_ct_font(ct_font voidptr) !&Typeface {
	if ct_font == unsafe { nil } {
		return error('gg.canvas: nil CTFont')
	}
	mut ids := unsafe { typeface_ids }
	id := ids.next
	ids.next++
	return &Typeface{
		id:               id
		ct_font:          C.v_canvas_cf_retain(ct_font)
		has_color_glyphs: C.v_canvas_ct_font_traits(ct_font) & ct_color_glyphs_trait != 0
	}
}
