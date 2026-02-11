// BEAM Backend: Graphics/GPU modules are not applicable on the BEAM VM.
// The BEAM backend focuses on server-side, networking, and concurrent workloads.
// All functions return safe defaults (empty structs, 0, false).
// For GUI on BEAM, consider using wx (Erlang's native GUI toolkit).
//
// Fontstash is a C font rendering library. On BEAM, font operations are no-ops
// and text measurement functions return 0. Context creation returns empty structs.
module fontstash

// Context is the BEAM stub for the fontstash context (replaces C.FONScontext)
pub struct Context {}

pub const invalid = -1

pub struct Params {
pub:
	width        int
	height       int
	flags        char
	user_ptr     voidptr
	render_create fn (uptr voidptr, width int, height int) int = unsafe { nil }
	render_resize fn (uptr voidptr, width int, height int) int = unsafe { nil }
	render_update fn (uptr voidptr, rect &int, data &u8)       = unsafe { nil }
	render_delete fn (uptr voidptr)                            = unsafe { nil }
}

pub struct Quad {
	x0 f32
	y0 f32
	s0 f32
	t0 f32
	x1 f32
	y1 f32
	s1 f32
	t1 f32
}

pub struct TextIter {
	x              f32
	y              f32
	nextx          f32
	nexty          f32
	scale          f32
	spacing        f32
	codepoint      u32
	isize          i16
	iblur          i16
	prev_glyph_idx int
}

pub struct Font {}

pub fn create_internal(params &Params) &Context {
	return &Context{}
}

pub fn delete_internal(s &Context) {}

pub fn (s &Context) set_error_callback(callback fn (voidptr, int, int), uptr voidptr) {}

pub fn (s &Context) get_atlas_size() (int, int) {
	return 0, 0
}

pub fn (s &Context) expand_atlas(width int, height int) int {
	return 0
}

pub fn (s &Context) reset_atlas(width int, height int) int {
	return 0
}

pub fn (s &Context) get_font_by_name(name string) int {
	return invalid
}

pub fn (s &Context) add_fallback_font(base int, fallback int) int {
	return 0
}

pub fn (s &Context) add_font_mem(name string, data []u8, free_data bool) int {
	return invalid
}

pub fn (s &Context) push_state() {}

pub fn (s &Context) pop_state() {}

pub fn (s &Context) clear_state() {}

pub fn (s &Context) set_size(size f32) {}

pub fn (s &Context) set_color(color u32) {}

pub fn (s &Context) set_spacing(spacing f32) {}

pub fn (s &Context) set_blur(blur f32) {}

pub fn (s &Context) set_align(align int) {}

pub fn (s &Context) set_alignment(align Align) {}

pub fn (s &Context) set_font(font_id int) {}

pub fn (s &Context) draw_text(x f32, y f32, text string) f32 {
	return 0.0
}

pub fn (s &Context) text_bounds(x f32, y f32, text string, bounds &f32) f32 {
	return 0.0
}

pub fn (s &Context) line_bounds(y f32, miny &f32, maxy &f32) {}

pub fn (s &Context) vert_metrics(ascender &f32, descender &f32, lineh &f32) {}

pub fn (s &Context) text_iter_init(iter &TextIter, x f32, y f32, str &char, end &char) int {
	return 0
}

pub fn (s &Context) text_iter_next(iter &TextIter, quad &Quad) int {
	return 0
}

pub fn (s &Context) get_texture_data(width &int, height &int) &u8 {
	return unsafe { nil }
}

pub fn (s &Context) validate_texture(dirty &int) int {
	return 0
}

pub fn (s &Context) draw_debug(x f32, y f32) {}
