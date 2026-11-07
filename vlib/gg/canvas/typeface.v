module canvas

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
