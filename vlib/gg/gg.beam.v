// BEAM Backend: Graphics/GPU modules are not applicable on the BEAM VM.
// The BEAM backend focuses on server-side, networking, and concurrent workloads.
// All functions return safe defaults (empty structs, 0, false).
// For GUI on BEAM, consider using wx (Erlang's native GUI toolkit).
//
// Provides type-compatible placeholders so V code that uses gg compiles on BEAM.
// Context, Config, and event types are defined for compilation but the event loop is a no-op.
module gg

import time
import sokol.sapp
import sokol.sgl
import sokol.gfx

pub enum BEAMEventType {
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
	update_cursor
	quit_requested
	clipboard_pasted
	files_dropped
	num
}

pub struct TouchPoint {
pub mut:
	identifier u64
	pos_x      f32
	pos_y      f32
	changed    bool
}

pub struct IconDesc {
pub:
	sokol_default bool = true
}

pub struct Event {
pub mut:
	frame_count        u64
	typ                BEAMEventType
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
	touches            [8]TouchPoint
	window_width       int
	window_height      int
	framebuffer_width  int
	framebuffer_height int
}

pub struct Config {
pub:
	width         int = 800
	height        int = 600
	retina        bool
	resizable     bool
	user_data     voidptr
	font_size     int
	create_window bool
	window_title      string = 'A GG Window. Set window_title: to change it.'
	icon              IconDesc
	html5_canvas_name string = 'canvas'
	borderless_window bool
	always_on_top     bool
	bg_color          Color
	init_fn           FNCb   = unsafe { nil }
	frame_fn          FNCb   = unsafe { nil }
	native_frame_fn   FNCb   = unsafe { nil }
	cleanup_fn        FNCb   = unsafe { nil }
	fail_fn           FNFail = unsafe { nil }

	update_fn FNUpdate = unsafe { nil }

	event_fn FNEvent  = unsafe { nil }
	on_event FNEvent2 = unsafe { nil }
	quit_fn  FNEvent  = unsafe { nil }

	keydown_fn FNKeyDown = unsafe { nil }
	keyup_fn   FNKeyUp   = unsafe { nil }
	char_fn    FNChar    = unsafe { nil }

	move_fn    FNMove    = unsafe { nil }
	click_fn   FNClick   = unsafe { nil }
	unclick_fn FNUnClick = unsafe { nil }
	leave_fn   FNEvent   = unsafe { nil }
	enter_fn   FNEvent   = unsafe { nil }
	resized_fn FNEvent   = unsafe { nil }
	scroll_fn  FNEvent   = unsafe { nil }
	fullscreen    bool
	scale         f32 = 1.0
	sample_count  int
	swap_interval int = 1
	font_path             string
	custom_bold_font_path string
	ui_mode               bool
	font_bytes_normal []u8
	font_bytes_bold   []u8
	font_bytes_mono   []u8
	font_bytes_italic []u8
	native_rendering  bool
	enable_dragndrop             bool
	max_dropped_files            int = 1
	max_dropped_file_path_length int = 2048

	min_width  int
	min_height int
}

@[heap]
pub struct PipelineContainer {
pub mut:
	alpha sgl.Pipeline
	add   sgl.Pipeline
}

fn (mut container PipelineContainer) init_pipeline() {
	// BEAM: no-op - pipelines not available
}

@[heap]
pub struct Context {
mut:
	render_text bool = true
	image_cache   []Image
	needs_refresh bool = true
	ticks         int
pub:
	native_rendering bool
pub mut:
	scale       f32 = 1.0
	width       int
	height      int
	clear_pass  gfx.PassAction
	window      sapp.Desc
	pipeline    &PipelineContainer = unsafe { nil }
	config      Config
	user_data   voidptr
	ft          &FT = unsafe { nil }
	font_inited bool
	ui_mode     bool
	frame       u64

	mbtn_mask     u8
	mouse_buttons MouseButtons
	mouse_pos_x   int
	mouse_pos_y   int
	mouse_dx      int
	mouse_dy      int
	scroll_x      int
	scroll_y      int

	key_modifiers     Modifier
	key_repeat        bool
	pressed_keys      [key_code_max]bool
	pressed_keys_edge [key_code_max]bool
	fps          FPSConfig
	has_started  bool
	timer        time.StopWatch
	update_timer time.StopWatch
}

pub struct FPSConfig {
pub mut:
	x                int
	y                int
	width            int
	height           int
	show             bool
	text_config      TextCfg = TextCfg{
		color:          yellow
		size:           20
		align:          .center
		vertical_align: .middle
	}
	background_color Color = Color{
		r: 0
		g: 0
		b: 0
		a: 128
	}
}

pub enum EndEnum {
	clear
	passthru
}

@[params]
pub struct EndOptions {
pub:
	how EndEnum
}

pub fn start(cfg Config) {
	mut ctx := new_context(cfg)
	ctx.run()
}

pub fn new_context(cfg Config) &Context {
	mut ctx := &Context{
		user_data:        cfg.user_data
		width:            cfg.width
		height:           cfg.height
		config:           cfg
		ft:               unsafe { nil }
		ui_mode:          cfg.ui_mode
		native_rendering: cfg.native_rendering
	}
	return ctx
}

pub fn (mut ctx Context) run() {
	if ctx.user_data == unsafe { nil } {
		ctx.user_data = ctx
	}
	// BEAM: no event loop — GUI not available
}

pub fn (ctx &Context) quit() {
	// BEAM: no-op
}

pub fn (mut ctx Context) set_bg_color(c Color) {
	// BEAM: no-op
}

pub fn (mut ctx Context) resize(width int, height int) {
	ctx.width = width
	ctx.height = height
}

pub fn (mut ctx Context) refresh_ui() {
	ctx.needs_refresh = true
	ctx.ticks = 0
}

pub fn (ctx &Context) begin() {
	// BEAM: no-op
}

pub fn (ctx &Context) end(options EndOptions) {
	// BEAM: no-op
}

pub fn (ctx &Context) show_fps() {
	// BEAM: no-op
}

pub fn create_default_pass(action voidptr) voidptr {
	return unsafe { nil }
}

pub fn dpi_scale() f32 {
	return 1.0
}

pub fn high_dpi() bool {
	return false
}

pub fn screen_size() Size {
	return Size{}
}

pub fn window_size() Size {
	return Size{}
}

pub fn (ctx Context) window_size() Size {
	return Size{ctx.width, ctx.height}
}

pub fn set_window_title(title string) {
	// BEAM: no-op
}

pub fn window_size_real_pixels() Size {
	return Size{}
}

pub fn is_fullscreen() bool {
	return false
}

pub fn toggle_fullscreen() {
	// BEAM: no-op
}

@[if gg_memory_trace_frame ?; manualfree]
fn (mut ctx Context) memory_trace_frame() {
	// BEAM: no-op
}

fn (mut ctx Context) record_frame() {
	// BEAM: no-op
}

// From gg_ui.c.v — required for ui.DrawDevice interface
pub fn (ctx &Context) scissor_rect(x int, y int, w int, h int) {
	// BEAM: no-op
}

pub fn (ctx &Context) has_text_style() bool {
	return false
}

pub fn (ctx &Context) set_text_style(font_name string, font_path string, size int, color Color, align int,
	vertical_align int) {
	// BEAM: no-op
}

pub fn (ctx &Context) draw_text_default(x int, y int, text string) {
	// BEAM: no-op
}
