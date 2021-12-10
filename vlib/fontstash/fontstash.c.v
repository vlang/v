module fontstash

#flag -I @VEXEROOT/thirdparty/fontstash
#define FONTSTASH_IMPLEMENTATION
$if gcboehm ? {
	#define FONTSTASH_MALLOC GC_MALLOC
	#define FONTSTASH_REALLOC GC_REALLOC
	#define FONTSTASH_FREE GC_FREE
}
#include "fontstash.h"
#flag darwin -I/usr/local/Cellar/freetype/2.10.2/include/freetype2

$if windows {
	$if tinyc {
		#flag @VEXEROOT/thirdparty/tcc/lib/openlibm.o
	}
} $else {
	#flag -lm
}

pub type Context = C.FONScontext

//#flag -lfreetype
pub const (
	// TODO: fontstash.used_import is used to keep v from warning about unused imports
	used_import = 1
)

// Contructor and destructor.
[inline]
pub fn create_internal(params &C.FONSparams) &Context {
	return C.fonsCreateInternal(params)
}

[inline]
pub fn delete_internal(s &Context) {
	C.fonsDeleteInternal(s)
}

[inline]
pub fn (s &Context) set_error_callback(callback fn (voidptr, int, int), uptr voidptr) {
	C.fonsSetErrorCallback(s, callback, uptr)
}

// Returns current atlas size.
[inline]
pub fn (s &Context) get_atlas_size() (int, int) {
	mut width := 0
	mut height := 0
	C.fonsGetAtlasSize(s, &width, &height)
	return width, height
}

// Expands the atlas size.
[inline]
pub fn (s &Context) expand_atlas(width int, height int) int {
	return C.fonsExpandAtlas(s, width, height)
}

// Resets the whole stash.
[inline]
pub fn (s &Context) reset_atlas(width int, height int) int {
	return C.fonsResetAtlas(s, width, height)
}

// Add fonts
[inline]
pub fn (s &Context) get_font_by_name(name string) int {
	return C.fonsGetFontByName(s, &char(name.str))
}

[inline]
pub fn (s &Context) add_fallback_font(base int, fallback int) int {
	return C.fonsAddFallbackFont(s, base, fallback)
}

[inline]
pub fn (s &Context) add_font_mem(name string, data []byte, free_data bool) int {
	return C.fonsAddFontMem(s, &char(name.str), data.data, data.len, int(free_data))
}

// State handling
[inline]
pub fn (s &Context) push_state() {
	C.fonsPushState(s)
}

[inline]
pub fn (s &Context) pop_state() {
	C.fonsPopState(s)
}

[inline]
pub fn (s &Context) clear_state() {
	C.fonsClearState(s)
}

// State setting
[inline]
pub fn (s &Context) set_size(size f32) {
	C.fonsSetSize(s, size)
}

[inline]
pub fn (s &Context) set_color(color u32) {
	C.fonsSetColor(s, color)
}

[inline]
pub fn (s &Context) set_spacing(spacing f32) {
	C.fonsSetSpacing(s, spacing)
}

[inline]
pub fn (s &Context) set_blur(blur f32) {
	C.fonsSetBlur(s, blur)
}

[inline]
pub fn (s &Context) set_align(align int) {
	C.fonsSetAlign(s, align)
}

[inline]
pub fn (s &Context) set_font(font int) {
	C.fonsSetFont(s, font)
}

// Draw text
[inline]
pub fn (s &Context) draw_text(x f32, y f32, text string) f32 {
	return C.fonsDrawText(s, x, y, &char(text.str), &char(0))
}

// Measure text
[inline]
pub fn (s &Context) text_bounds(x f32, y f32, text string, bounds &f32) f32 {
	return C.fonsTextBounds(s, x, y, &char(text.str), &char(0), bounds)
}

[inline]
pub fn (s &Context) line_bounds(y f32, miny &f32, maxy &f32) {
	C.fonsLineBounds(s, y, miny, maxy)
}

[inline]
pub fn (s &Context) vert_metrics(ascender &f32, descender &f32, lineh &f32) {
	C.fonsVertMetrics(s, ascender, descender, lineh)
}

// Text iterator
[inline]
pub fn (s &Context) text_iter_init(iter &C.FONStextIter, x f32, y f32, str &char, end &char) int {
	return C.fonsTextIterInit(s, iter, x, y, str, end)
}

[inline]
pub fn (s &Context) text_iter_next(iter &C.FONStextIter, quad &C.FONSquad) int {
	return C.fonsTextIterNext(s, iter, quad)
}

// Pull texture changes
[inline]
pub fn (s &Context) get_texture_data(width &int, height &int) &byte {
	return &byte(C.fonsGetTextureData(s, width, height))
}

[inline]
pub fn (s &Context) validate_texture(dirty &int) int {
	return C.fonsValidateTexture(s, dirty)
}

// Draws the stash texture for debugging
[inline]
pub fn (s &Context) draw_debug(x f32, y f32) {
	C.fonsDrawDebug(s, x, y)
}
