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
	invalid     = C.FONS_INVALID // -1
)

// create_internal returns a fontstash Context allocated on the heap.
//
// See also: delete_internal
[inline]
pub fn create_internal(params &C.FONSparams) &Context {
	return C.fonsCreateInternal(params)
}

// delete_internal deletes and free memory of `s` fontstash Context.
//
// See also: create_internal
[inline]
pub fn delete_internal(s &Context) {
	C.fonsDeleteInternal(s)
}

// set_error_callback sets `callback` as a function to be called if fontstash
// encounter any errors. `uptr` can be used to pass custom userdata.
[inline]
pub fn (s &Context) set_error_callback(callback fn (voidptr, int, int), uptr voidptr) {
	C.fonsSetErrorCallback(s, callback, uptr)
}

// get_atlas_size returns the current size of the texture atlas which
// the font is rendered to.
[inline]
pub fn (s &Context) get_atlas_size() (int, int) {
	mut width := 0
	mut height := 0
	C.fonsGetAtlasSize(s, &width, &height)
	return width, height
}

// expand_atlas expands the font texture atlas size to `width` x `height`.
[inline]
pub fn (s &Context) expand_atlas(width int, height int) int {
	return C.fonsExpandAtlas(s, width, height)
}

// reset_atlas resets `width` x `height` of the font texture atlas.
[inline]
pub fn (s &Context) reset_atlas(width int, height int) int {
	return C.fonsResetAtlas(s, width, height)
}

// get_font_by_name returns the id of the font with `name` or
// `fontstash.invalid` if no font with `name` could be found.
[inline]
pub fn (s &Context) get_font_by_name(name string) int {
	return C.fonsGetFontByName(s, &char(name.str))
}

// add_fallback_font adds a fallback font to the `base` font id in the Context.
// `fallback` is expected to be the id of a previous, successfully, added font.
// add_fallback_font returns `1` on success, `0` otherwise.
[inline]
pub fn (s &Context) add_fallback_font(base int, fallback int) int {
	return C.fonsAddFallbackFont(s, base, fallback)
}

// add_font_mem adds the font data located in memory to the Context.
// `name` is the human readable name for the font.
// `free_data` indicates if `data` should be freed after the font is added.
// The function returns the id of the font on success, `fontstash.invalid` otherwise.
[inline]
pub fn (s &Context) add_font_mem(name string, data []u8, free_data bool) int {
	return C.fonsAddFontMem(s, &char(name.str), data.data, data.len, int(free_data))
}

// push_state pushes a new state on the state stack.
// A state holds the current attributes of the rendering,
// attributes are things like color, size, the font in use, blur effect etc.
//
// See also: pop_state
// See also: clear_state
// See also: set_size
// See also: set_color
// See also: set_spacing
// See also: set_blur
// See also: set_align
// See also: set_font
[inline]
pub fn (s &Context) push_state() {
	C.fonsPushState(s)
}

// pop_state pops the current state from the state stack.
//
// See also: push_state
// See also: clear_state
[inline]
pub fn (s &Context) pop_state() {
	C.fonsPopState(s)
}

// clear_state clears the current state.
//
// See also: push_state
// See also: pop_state
[inline]
pub fn (s &Context) clear_state() {
	C.fonsClearState(s)
}

// set_size sets the font size to `size` on the active state.
//
// See also: push_state
// See also: pop_state
// See also: clear_state
[inline]
pub fn (s &Context) set_size(size f32) {
	C.fonsSetSize(s, size)
}

// set_color sets the font color to `color` on the active state.
//
// See also: push_state
// See also: pop_state
// See also: clear_state
[inline]
pub fn (s &Context) set_color(color u32) {
	C.fonsSetColor(s, color)
}

// set_spacing sets the font spacing to `spacing` on the active state.
//
// See also: push_state
// See also: pop_state
// See also: clear_state
[inline]
pub fn (s &Context) set_spacing(spacing f32) {
	C.fonsSetSpacing(s, spacing)
}

// set_blur sets the font blur effect to `blur` on the active state.
//
// See also: push_state
// See also: pop_state
// See also: clear_state
[inline]
pub fn (s &Context) set_blur(blur f32) {
	C.fonsSetBlur(s, blur)
}

// set_align sets the font aligning to `align` on the active state.
//
// See also: push_state
// See also: pop_state
// See also: clear_state
[inline]
pub fn (s &Context) set_align(align int) {
	C.fonsSetAlign(s, int(align))
}

// set_alignment sets the font aligning to the `align` flags.
//
// See also: push_state
// See also: pop_state
// See also: clear_state
[inline]
pub fn (s &Context) set_alignment(align Align) {
	C.fonsSetAlign(s, int(align))
}

// set_font sets the font used for this render on the active state.
// `font_id` is the id of the loaded font.
//
// See also: push_state
// See also: pop_state
// See also: clear_state
[inline]
pub fn (s &Context) set_font(font_id int) {
	C.fonsSetFont(s, font_id)
}

// draw_text draws the `text` string at position `x`,`y`.
// The function returns the `x` coordinate of the resulting render.
[inline]
pub fn (s &Context) draw_text(x f32, y f32, text string) f32 {
	return C.fonsDrawText(s, x, y, &char(text.str), &char(0))
}

// text_bounds fills the `bounds` argument with the pixel dimensions
// of the rendered `text` at position `x`,`y`.
//
// `bounds` is expected to be of type `mut bounds := [4]f32{}`.
// Call example: `ctx.text_bounds(0, 0, 'example', &bounds[0])`.
// `bounds[0]` is the `x` coordinate of the top-left point.
// `bounds[1]` is the `y` coordinate of the top-left point.
// `bounds[2]` is the `x` coordinate of the bottom-right point.
// `bounds[3]` is the `y` coordinate of the bottom-right point.
[inline]
pub fn (s &Context) text_bounds(x f32, y f32, text string, bounds &f32) f32 {
	return C.fonsTextBounds(s, x, y, &char(text.str), &char(0), bounds)
}

// line_bounds fills `miny` and `maxy` with the values of the `minimum`
// and `maximum` line bounds respectively.
[inline]
pub fn (s &Context) line_bounds(y f32, miny &f32, maxy &f32) {
	C.fonsLineBounds(s, y, miny, maxy)
}

// vert_metrics assigns the respective values of `ascender`, `descender` and `lineh`.
[inline]
pub fn (s &Context) vert_metrics(ascender &f32, descender &f32, lineh &f32) {
	C.fonsVertMetrics(s, ascender, descender, lineh)
}

// text_iter_init initalizes the text iterator `iter`.
[inline]
pub fn (s &Context) text_iter_init(iter &C.FONStextIter, x f32, y f32, str &char, end &char) int {
	return C.fonsTextIterInit(s, iter, x, y, str, end)
}

// text_iter_next advances `iter` to the next `quad`.
[inline]
pub fn (s &Context) text_iter_next(iter &C.FONStextIter, quad &C.FONSquad) int {
	return C.fonsTextIterNext(s, iter, quad)
}

// get_texture_data returns the current Context's raw texture data.
// `width` and `height` is assigned the size of the texture dimensions.
[inline]
pub fn (s &Context) get_texture_data(width &int, height &int) &u8 {
	return &u8(C.fonsGetTextureData(s, width, height))
}

// validate_texture fills the `dirty` argument with the pixel dimensions
// of the dirty rectangle of the Context's raw texture, if any.
//
// `dirty` is expected to be of type `mut dirty := [4]int{}`.
// Call example: `is_dirty := ctx.validate_texture(&dirty[0])`.
// The function returns `1` if the texture has a dirty rectangle, `0` otherwise.
[inline]
pub fn (s &Context) validate_texture(dirty &int) int {
	return C.fonsValidateTexture(s, dirty)
}

// draw_debug draws the stash texture for debugging.
[inline]
pub fn (s &Context) draw_debug(x f32, y f32) {
	C.fonsDrawDebug(s, x, y)
}
