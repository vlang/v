module canvas

import os

#flag darwin -framework CoreText -framework CoreGraphics -framework CoreFoundation
#include "@DIR/text_coretext_darwin.h"

fn C.v_canvas_ct_font_from_data(bytes &u8, length u64) voidptr
fn C.v_canvas_ct_font_traits(font voidptr) u32
fn C.v_canvas_cf_release(ref voidptr)

// kCTFontColorGlyphsTrait
const ct_color_glyphs_trait = u32(1) << 13

// Typeface is a font face loaded from font data through CoreText, like
// SkTypeface_Mac created by SkFontMgr_New_CoreText()->makeFromStream().
@[heap]
pub struct Typeface {
pub:
	id u32
mut:
	ct_font          voidptr
	has_color_glyphs bool
}

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
