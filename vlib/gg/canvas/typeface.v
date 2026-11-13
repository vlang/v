module canvas

// Typeface owns a native font face loaded through CoreText on macOS or
// FreeType on Linux. Drawing and cleanup must run serially on the drawing thread.
@[heap]
pub struct Typeface {
pub:
	id u32
mut:
	ct_font          voidptr
	ft_font          voidptr
	has_color_glyphs bool
}

struct TypefaceIds {
mut:
	next u32 = 1
}

const typeface_ids = &TypefaceIds{}

fn (t &Typeface) is_live() bool {
	$if macos {
		return t.ct_font != unsafe { nil }
	} $else $if linux {
		return t.ft_font != unsafe { nil }
	} $else {
		return false
	}
}
