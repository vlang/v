// BEAM Backend: Sokol Application functions bridged to Erlang wx + OpenGL via vbeam_sokol.
// Codegen intercepts all sapp.* calls -> vbeam_sokol:* (runtime module).
// The vbeam_sokol gen_server creates a wxFrame + wxGLCanvas for real GPU rendering.
// Stub bodies below are dead code — codegen emits direct calls to vbeam_sokol.
module sapp

import sokol.gfx
import sokol.memory

// ALL types must be defined here since this module has ZERO .v files.

// --- Enums ---

pub enum EventType {
	invalid
	key_down
	key_up
	char
	mouse_down
	mouse_up
	mouse_scroll
	mouse_move
	mouse_enter
	mouse_leave
	touches_began
	touches_moved
	touches_ended
	touches_cancelled
	resized
	iconified
	restored
	focused
	unfocused
	suspended
	resumed
	quit_requested
	clipboard_pasted
	files_dropped
	num
}

pub enum MouseButton {
	invalid = -1
	left    = 0
	right   = 1
	middle  = 2
}

pub enum MouseCursor {
	default       = 0
	arrow         = 1
	ibeam         = 2
	crosshair     = 3
	pointing_hand = 4
	resize_ew     = 5
	resize_ns     = 6
	resize_nwse   = 7
	resize_nesw   = 8
	resize_all    = 9
	not_allowed   = 10
}

pub enum Modifier {
	shift = 1
	ctrl  = 2
	alt   = 4
	super = 8
	lmb   = 0x100
	rmb   = 0x200
	mmb   = 0x400
}

pub enum KeyCode {
	invalid       = 0
	space         = 32
	apostrophe    = 39
	comma         = 44
	minus         = 45
	period        = 46
	slash         = 47
	_0            = 48
	_1            = 49
	_2            = 50
	_3            = 51
	_4            = 52
	_5            = 53
	_6            = 54
	_7            = 55
	_8            = 56
	_9            = 57
	semicolon     = 59
	equal         = 61
	a             = 65
	b             = 66
	c             = 67
	d             = 68
	e             = 69
	f             = 70
	g             = 71
	h             = 72
	i             = 73
	j             = 74
	k             = 75
	l             = 76
	m             = 77
	n             = 78
	o             = 79
	p             = 80
	q             = 81
	r             = 82
	s             = 83
	t             = 84
	u             = 85
	v             = 86
	w             = 87
	x             = 88
	y             = 89
	z             = 90
	left_bracket  = 91
	backslash     = 92
	right_bracket = 93
	grave_accent  = 96
	world_1       = 161
	world_2       = 162
	escape        = 256
	enter         = 257
	tab           = 258
	backspace     = 259
	insert        = 260
	delete        = 261
	right         = 262
	left          = 263
	down          = 264
	up            = 265
	page_up       = 266
	page_down     = 267
	home          = 268
	end           = 269
	caps_lock     = 280
	scroll_lock   = 281
	num_lock      = 282
	print_screen  = 283
	pause         = 284
	f1            = 290
	f2            = 291
	f3            = 292
	f4            = 293
	f5            = 294
	f6            = 295
	f7            = 296
	f8            = 297
	f9            = 298
	f10           = 299
	f11           = 300
	f12           = 301
	f13           = 302
	f14           = 303
	f15           = 304
	f16           = 305
	f17           = 306
	f18           = 307
	f19           = 308
	f20           = 309
	f21           = 310
	f22           = 311
	f23           = 312
	f24           = 313
	f25           = 314
	kp_0          = 320
	kp_1          = 321
	kp_2          = 322
	kp_3          = 323
	kp_4          = 324
	kp_5          = 325
	kp_6          = 326
	kp_7          = 327
	kp_8          = 328
	kp_9          = 329
	kp_decimal    = 330
	kp_divide     = 331
	kp_multiply   = 332
	kp_subtract   = 333
	kp_add        = 334
	kp_enter      = 335
	kp_equal      = 336
	left_shift    = 340
	left_control  = 341
	left_alt      = 342
	left_super    = 343
	right_shift   = 344
	right_control = 345
	right_alt     = 346
	right_super   = 347
	menu          = 348
}

pub enum TouchToolType {
	unknown
	finger
	stylus
	mouse
	eraser
	palm
}

// --- Constants ---

const max_touchpoints = 8
const max_mousebuttons = 3
const max_keycodes = 512
const max_iconimages = 8

// --- Allocator and logger structs ---

pub struct Allocator {
pub mut:
	alloc_fn  memory.FnAllocatorAlloc = unsafe { nil }
	free_fn   memory.FnAllocatorFree  = unsafe { nil }
	user_data voidptr
}

pub struct SappLogger {
pub mut:
	func      memory.FnLogCb = unsafe { nil }
	user_data voidptr
}

// --- Structs ---

pub struct Range {
pub:
	ptr  voidptr
	size usize
}

pub struct ImageDesc {
pub:
	width  int
	height int
	pixels Range
}

pub struct IconDesc {
pub:
	sokol_default bool
	images        [max_iconimages]ImageDesc
}

pub struct TouchPoint {
pub:
	identifier       u64
	pos_x            f32
	pos_y            f32
	android_tooltype TouchToolType
	changed          bool
}

pub struct Event {
pub:
	frame_count        u64
	@type              EventType
	key_code           KeyCode
	char_code          u32
	key_repeat         bool
	modifiers          u32
	mouse_button       MouseButton
	mouse_x            f32
	mouse_y            f32
	mouse_dx           f32
	mouse_dy           f32
	scroll_x           f32
	scroll_y           f32
	num_touches        int
	touches            [max_touchpoints]TouchPoint
	window_width       int
	window_height      int
	framebuffer_width  int
	framebuffer_height int
}

pub fn (e &Event) str() string {
	return 'evt: frame_count=${e.frame_count}, type=${e.@type}'
}

pub struct Desc {
pub:
	init_cb    fn ()       = unsafe { nil }
	frame_cb   fn ()       = unsafe { nil }
	cleanup_cb fn ()       = unsafe { nil }
	event_cb   fn (&Event) = unsafe { nil }

	user_data           voidptr
	init_userdata_cb    fn (voidptr)         = unsafe { nil }
	frame_userdata_cb   fn (voidptr)         = unsafe { nil }
	cleanup_userdata_cb fn (voidptr)         = unsafe { nil }
	event_userdata_cb   fn (&Event, voidptr) = unsafe { nil }

	width                        int
	height                       int
	sample_count                 int
	swap_interval                int
	high_dpi                     bool
	fullscreen                   bool
	alpha                        bool
	window_title                 &char = &char(unsafe { nil })
	enable_clipboard             bool
	clipboard_size               int
	enable_dragndrop             bool
	max_dropped_files            int
	max_dropped_file_path_length int
	icon                         IconDesc

	win32_console_utf8            bool
	win32_console_create          bool
	win32_console_attach          bool
	html5_canvas_name             &char = &char(unsafe { nil })
	html5_canvas_resize           bool
	html5_preserve_drawing_buffer bool
	html5_premultiplied_alpha     bool
	html5_ask_leave_site          bool
	ios_keyboard_resizes_canvas   bool

	v_native_render bool
	min_width       int
	min_height      int
pub mut:
	allocator Allocator
	logger    SappLogger
}

pub struct Screenshot {
	width  int
	height int
	size   int
mut:
	pixels &u8 = unsafe { nil }
}

// --- Functions ---

pub fn create_desc() gfx.Desc {
	return gfx.Desc{
		environment: glue_environment()
		image_pool_size: 1000
	}
}

pub fn create_default_pass(action gfx.PassAction) gfx.Pass {
	return gfx.Pass{
		action:    action
		swapchain: glue_swapchain()
	}
}

pub fn glue_environment() gfx.Environment {
	mut env := gfx.Environment{}
	env.defaults.color_format = .rgba8
	env.defaults.depth_format = .depth
	env.defaults.sample_count = 1
	return env
}

pub fn glue_swapchain() gfx.Swapchain {
	mut swapchain := gfx.Swapchain{}
	swapchain.width = width()
	swapchain.height = height()
	swapchain.sample_count = sample_count()
	swapchain.color_format = .rgba8
	swapchain.depth_format = .depth
	return swapchain
}

pub fn isvalid() bool {
	return false
}

pub fn width() int {
	return 0
}

pub fn height() int {
	return 0
}

pub fn color_format() int {
	return 0
}

pub fn depth_format() int {
	return 0
}

pub fn sample_count() int {
	return 1
}

pub fn high_dpi() bool {
	return false
}

pub fn dpi_scale() f32 {
	return 1.0
}

pub fn show_keyboard(visible bool) {
}

pub fn keyboard_shown() bool {
	return false
}

pub fn show_mouse(visible bool) {
}

pub fn set_mouse_cursor(cursor MouseCursor) {
}

pub fn mouse_shown() bool {
	return false
}

pub fn lock_mouse(locked bool) {
}

pub fn mouse_locked() bool {
	return false
}

pub fn userdata() voidptr {
	return unsafe { nil }
}

pub fn query_desc() Desc {
	return Desc{}
}

pub fn request_quit() {
}

pub fn cancel_quit() {
}

pub fn quit() {
}

pub fn consume_event() {
}

pub fn frame_count() u64 {
	return 0
}

pub fn frame_duration() f64 {
	return 0.0
}

pub fn set_clipboard_string(str &u8) {
}

pub fn get_clipboard_string() &char {
	return &char(unsafe { nil })
}

pub fn run(desc &Desc) {
}

pub fn html5_ask_leave_site(ask bool) {
}

pub fn metal_get_device() voidptr {
	return unsafe { nil }
}

pub fn metal_get_current_drawable() voidptr {
	return unsafe { nil }
}

pub fn metal_get_depth_stencil_texture() voidptr {
	return unsafe { nil }
}

pub fn metal_get_msaa_color_texture() voidptr {
	return unsafe { nil }
}

pub fn macos_get_window() voidptr {
	return unsafe { nil }
}

pub fn ios_get_window() voidptr {
	return unsafe { nil }
}

pub fn d3d11_get_device() voidptr {
	return unsafe { nil }
}

pub fn d3d11_get_device_context() voidptr {
	return unsafe { nil }
}

pub fn d3d11_get_render_view() voidptr {
	return unsafe { nil }
}

pub fn d3d11_get_resolve_view() voidptr {
	return unsafe { nil }
}

pub fn d3d11_get_depth_stencil_view() voidptr {
	return unsafe { nil }
}

pub fn win32_get_hwnd() voidptr {
	return unsafe { nil }
}

pub fn wgpu_get_device() voidptr {
	return unsafe { nil }
}

pub fn wgpu_get_render_view() voidptr {
	return unsafe { nil }
}

pub fn wgpu_get_resolve_view() voidptr {
	return unsafe { nil }
}

pub fn wgpu_get_depth_stencil_view() voidptr {
	return unsafe { nil }
}

pub fn gl_get_framebuffer() u32 {
	return 0
}

pub fn android_get_native_activity() voidptr {
	return unsafe { nil }
}

pub fn toggle_fullscreen() {
}

pub fn is_fullscreen() bool {
	return false
}

pub fn get_num_dropped_files() int {
	return 0
}

pub fn get_dropped_file_path(index int) string {
	return ''
}

pub fn screenshot(path string) ! {
	return error('screenshots not supported on BEAM backend')
}

pub fn screenshot_ppm(path string) ! {
	return error('screenshots not supported on BEAM backend')
}

pub fn screenshot_png(path string) ! {
	return error('screenshots not supported on BEAM backend')
}

pub fn screenshot_window() &Screenshot {
	return &Screenshot{
		width:  0
		height: 0
		size:   0
		pixels: unsafe { nil }
	}
}

@[unsafe]
pub fn (mut ss Screenshot) free() {
}

@[unsafe]
pub fn (mut ss Screenshot) destroy() {
}
