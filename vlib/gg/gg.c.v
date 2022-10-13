// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.

module gg

import os
import os.font
import gx
import sokol
import sokol.sapp
import sokol.sgl
import sokol.gfx

$if windows {
	#flag -lgdi32
	#include "windows.h"
}

// call Windows API to get screen size
fn C.GetSystemMetrics(int) int

// fn C.WaitMessage()

pub type TouchPoint = C.sapp_touchpoint

pub struct Event {
pub mut:
	frame_count        u64
	typ                sapp.EventType
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
	width         int
	height        int
	use_ortho     bool // unused, still here just for backwards compatibility
	retina        bool
	resizable     bool
	user_data     voidptr
	font_size     int
	create_window bool
	// window_user_ptr voidptr
	window_title      string
	borderless_window bool
	always_on_top     bool
	bg_color          gx.Color
	init_fn           FNCb   = unsafe { nil }
	frame_fn          FNCb   = unsafe { nil }
	native_frame_fn   FNCb   = unsafe { nil }
	cleanup_fn        FNCb   = unsafe { nil }
	fail_fn           FNFail = unsafe { nil }
	//
	event_fn FNEvent = unsafe { nil }
	quit_fn  FNEvent = unsafe { nil }
	//
	keydown_fn FNKeyDown = unsafe { nil }
	keyup_fn   FNKeyUp   = unsafe { nil }
	char_fn    FNChar    = unsafe { nil }
	//
	move_fn    FNMove    = unsafe { nil }
	click_fn   FNClick   = unsafe { nil }
	unclick_fn FNUnClick = unsafe { nil }
	leave_fn   FNEvent   = unsafe { nil }
	enter_fn   FNEvent   = unsafe { nil }
	resized_fn FNEvent   = unsafe { nil }
	scroll_fn  FNEvent   = unsafe { nil }
	// wait_events       bool // set this to true for UIs, to save power
	fullscreen    bool
	scale         f32 = 1.0
	sample_count  int
	swap_interval int = 1 // 1 = 60fps, 2 = 30fps etc. The preferred swap interval (ignored on some platforms)
	// ved needs this
	// init_text bool
	font_path             string
	custom_bold_font_path string
	ui_mode               bool // refreshes only on events to save CPU usage
	// font bytes for embedding
	font_bytes_normal []u8
	font_bytes_bold   []u8
	font_bytes_mono   []u8
	font_bytes_italic []u8
	native_rendering  bool // Cocoa on macOS/iOS, GDI+ on Windows
	// drag&drop
	enable_dragndrop             bool // enable file dropping (drag'n'drop), default is false
	max_dropped_files            int = 1 // max number of dropped files to process (default: 1)
	max_dropped_file_path_length int = 2048 // max length in bytes of a dropped UTF-8 file path (default: 2048)
}

[heap]
pub struct Context {
mut:
	render_text bool = true
	// a cache with all images created by the user. used for sokol image init and to save space
	// (so that the user can store image ids, not entire Image objects)
	image_cache   []Image
	needs_refresh bool = true
	ticks         int // for ui mode only
pub:
	native_rendering bool
pub mut:
	scale       f32 = 1.0 // will get set to 2.0 for retina, will remain 1.0 for normal
	width       int
	height      int
	clear_pass  gfx.PassAction
	window      sapp.Desc
	timage_pip  sgl.Pipeline
	config      Config
	user_data   voidptr
	ft          &FT = unsafe { nil }
	font_inited bool
	ui_mode     bool // do not redraw everything 60 times/second, but only when the user requests
	frame       u64  // the current frame counted from the start of the application; always increasing
	//
	mbtn_mask     u8
	mouse_buttons MouseButtons // typed version of mbtn_mask; easier to use for user programs
	mouse_pos_x   int
	mouse_pos_y   int
	mouse_dx      int
	mouse_dy      int
	scroll_x      int
	scroll_y      int
	//
	key_modifiers     Modifier // the current key modifiers
	key_repeat        bool     // whether the pressed key was an autorepeated one
	pressed_keys      [key_code_max]bool // an array representing all currently pressed keys
	pressed_keys_edge [key_code_max]bool // true when the previous state of pressed_keys,
	// *before* the current event was different
	fps FPSConfig
}

fn gg_init_sokol_window(user_data voidptr) {
	mut ctx := unsafe { &Context(user_data) }
	desc := sapp.create_desc()
	/*
	desc := gfx.Desc{
		mtl_device: sapp.metal_get_device()
		mtl_renderpass_descriptor_cb: sapp.metal_get_renderpass_descriptor
		mtl_drawable_cb: sapp.metal_get_drawable
		d3d11_device: sapp.d3d11_get_device()
		d3d11_device_context: sapp.d3d11_get_device_context()
		d3d11_render_target_view_cb: sapp.d3d11_get_render_target_view
		d3d11_depth_stencil_view_cb: sapp.d3d11_get_depth_stencil_view
	}
	*/
	gfx.setup(&desc)
	sgl_desc := sgl.Desc{}
	sgl.setup(&sgl_desc)
	ctx.set_scale()
	// is_high_dpi := sapp.high_dpi()
	// fb_w := sapp.width()
	// fb_h := sapp.height()
	// println('ctx.scale=$ctx.scale is_high_dpi=$is_high_dpi fb_w=$fb_w fb_h=$fb_h')
	// if ctx.config.init_text {
	// `os.is_file()` won't work on Android if the font file is embedded into the APK
	exists := $if !android { os.is_file(ctx.config.font_path) } $else { true }
	if ctx.config.font_path != '' && !exists {
		ctx.render_text = false
	} else if ctx.config.font_path != '' && exists {
		// t := time.ticks()
		ctx.ft = new_ft(
			font_path: ctx.config.font_path
			custom_bold_font_path: ctx.config.custom_bold_font_path
			scale: ctx.scale
		) or { panic(err) }
		// println('FT took ${time.ticks()-t} ms')
		ctx.font_inited = true
	} else {
		if ctx.config.font_bytes_normal.len > 0 {
			ctx.ft = new_ft(
				bytes_normal: ctx.config.font_bytes_normal
				bytes_bold: ctx.config.font_bytes_bold
				bytes_mono: ctx.config.font_bytes_mono
				bytes_italic: ctx.config.font_bytes_italic
				scale: sapp.dpi_scale()
			) or { panic(err) }
			ctx.font_inited = true
		} else {
			sfont := font.default()
			if ctx.config.font_path != '' {
				eprintln('font file "$ctx.config.font_path" does not exist, the system font ($sfont) was used instead.')
			}

			ctx.ft = new_ft(
				font_path: sfont
				custom_bold_font_path: ctx.config.custom_bold_font_path
				scale: sapp.dpi_scale()
			) or { panic(err) }
			ctx.font_inited = true
		}
	}
	//
	mut pipdesc := gfx.PipelineDesc{
		label: c'alpha_image'
	}
	unsafe { vmemset(&pipdesc, 0, int(sizeof(pipdesc))) }

	color_state := gfx.ColorState{
		blend: gfx.BlendState{
			enabled: true
			src_factor_rgb: .src_alpha
			dst_factor_rgb: .one_minus_src_alpha
		}
	}
	pipdesc.colors[0] = color_state

	ctx.timage_pip = sgl.make_pipeline(&pipdesc)
	//
	if ctx.config.init_fn != unsafe { nil } {
		$if android {
			// NOTE on Android sokol can emit resize events *before* the init function is
			// called (Android has to initialize a lot more through the Activity system to
			// reach a valid coontext) and thus the user's code will miss the resize event.
			// To prevent this we emit a custom window resize event, if the screen size has
			// changed meanwhile.
			win_size := ctx.window_size()
			if ctx.width != win_size.width || ctx.height != win_size.height {
				ctx.width = win_size.width
				ctx.height = win_size.height
				if ctx.config.resized_fn != unsafe { nil } {
					e := Event{
						typ: .resized
						window_width: ctx.width
						window_height: ctx.height
					}
					ctx.config.resized_fn(&e, ctx.user_data)
				}
			}
		}
		ctx.config.init_fn(ctx.user_data)
	}
	// Create images now that we can do that after sg is inited
	if ctx.native_rendering {
		return
	}

	for i in 0 .. ctx.image_cache.len {
		if ctx.image_cache[i].simg.id == 0 {
			ctx.image_cache[i].init_sokol_image()
		}
	}
}

fn gg_frame_fn(user_data voidptr) {
	mut ctx := unsafe { &Context(user_data) }
	ctx.frame++
	if ctx.config.frame_fn == unsafe { nil } {
		return
	}
	if ctx.native_rendering {
		// return
	}

	ctx.record_frame()

	if ctx.ui_mode && !ctx.needs_refresh {
		// Draw 3 more frames after the "stop refresh" command
		ctx.ticks++
		if ctx.ticks > 3 {
			return
		}
	}
	ctx.config.frame_fn(ctx.user_data)
	ctx.needs_refresh = false
}

fn gg_event_fn(ce voidptr, user_data voidptr) {
	// e := unsafe { &sapp.Event(ce) }
	mut e := unsafe { &Event(ce) }
	mut ctx := unsafe { &Context(user_data) }
	if ctx.ui_mode {
		ctx.refresh_ui()
	}
	if e.typ == .mouse_down {
		bitplace := int(e.mouse_button)
		ctx.mbtn_mask |= u8(1 << bitplace)
		ctx.mouse_buttons = unsafe { MouseButtons(ctx.mbtn_mask) }
	}
	if e.typ == .mouse_up {
		bitplace := int(e.mouse_button)
		ctx.mbtn_mask &= ~(u8(1 << bitplace))
		ctx.mouse_buttons = unsafe { MouseButtons(ctx.mbtn_mask) }
	}
	if e.typ == .mouse_move && e.mouse_button == .invalid {
		if ctx.mbtn_mask & 0x01 > 0 {
			e.mouse_button = .left
		}
		if ctx.mbtn_mask & 0x02 > 0 {
			e.mouse_button = .right
		}
		if ctx.mbtn_mask & 0x04 > 0 {
			e.mouse_button = .middle
		}
	}
	ctx.mouse_pos_x = int(e.mouse_x / ctx.scale)
	ctx.mouse_pos_y = int(e.mouse_y / ctx.scale)
	ctx.mouse_dx = int(e.mouse_dx / ctx.scale)
	ctx.mouse_dy = int(e.mouse_dy / ctx.scale)
	ctx.scroll_x = int(e.scroll_x / ctx.scale)
	ctx.scroll_y = int(e.scroll_y / ctx.scale)
	ctx.key_modifiers = unsafe { Modifier(e.modifiers) }
	ctx.key_repeat = e.key_repeat
	if e.typ in [.key_down, .key_up] {
		key_idx := int(e.key_code) % key_code_max
		prev := ctx.pressed_keys[key_idx]
		next := e.typ == .key_down
		ctx.pressed_keys[key_idx] = next
		ctx.pressed_keys_edge[key_idx] = prev != next
	}
	if ctx.config.event_fn != unsafe { nil } {
		ctx.config.event_fn(e, ctx.config.user_data)
	}
	match e.typ {
		.mouse_move {
			if ctx.config.move_fn != unsafe { nil } {
				ctx.config.move_fn(e.mouse_x / ctx.scale, e.mouse_y / ctx.scale, ctx.config.user_data)
			}
		}
		.mouse_down {
			if ctx.config.click_fn != unsafe { nil } {
				ctx.config.click_fn(e.mouse_x / ctx.scale, e.mouse_y / ctx.scale, e.mouse_button,
					ctx.config.user_data)
			}
		}
		.mouse_up {
			if ctx.config.unclick_fn != unsafe { nil } {
				ctx.config.unclick_fn(e.mouse_x / ctx.scale, e.mouse_y / ctx.scale, e.mouse_button,
					ctx.config.user_data)
			}
		}
		.mouse_leave {
			if ctx.config.leave_fn != unsafe { nil } {
				ctx.config.leave_fn(e, ctx.config.user_data)
			}
		}
		.mouse_enter {
			if ctx.config.enter_fn != unsafe { nil } {
				ctx.config.enter_fn(e, ctx.config.user_data)
			}
		}
		.mouse_scroll {
			if ctx.config.scroll_fn != unsafe { nil } {
				ctx.config.scroll_fn(e, ctx.config.user_data)
			}
		}
		.key_down {
			if ctx.config.keydown_fn != unsafe { nil } {
				ctx.config.keydown_fn(e.key_code, unsafe { Modifier(e.modifiers) }, ctx.config.user_data)
			}
		}
		.key_up {
			if ctx.config.keyup_fn != unsafe { nil } {
				ctx.config.keyup_fn(e.key_code, unsafe { Modifier(e.modifiers) }, ctx.config.user_data)
			}
		}
		.char {
			if ctx.config.char_fn != unsafe { nil } {
				ctx.config.char_fn(e.char_code, ctx.config.user_data)
			}
		}
		.resized {
			if ctx.config.resized_fn != unsafe { nil } {
				ctx.config.resized_fn(e, ctx.config.user_data)
			}
		}
		.quit_requested {
			if ctx.config.quit_fn != unsafe { nil } {
				ctx.config.quit_fn(e, ctx.config.user_data)
			}
		}
		else {
			// dump(e)
		}
	}
}

fn gg_cleanup_fn(user_data voidptr) {
	mut ctx := unsafe { &Context(user_data) }
	if ctx.config.cleanup_fn != unsafe { nil } {
		ctx.config.cleanup_fn(ctx.config.user_data)
	}
	gfx.shutdown()
}

fn gg_fail_fn(msg &char, user_data voidptr) {
	mut ctx := unsafe { &Context(user_data) }
	vmsg := unsafe { tos3(msg) }
	if ctx.config.fail_fn != unsafe { nil } {
		ctx.config.fail_fn(vmsg, ctx.config.user_data)
	} else {
		eprintln('gg error: $vmsg')
	}
}

//---- public methods

// new_context returns an initialized `Context` allocated on the heap.
pub fn new_context(cfg Config) &Context {
	mut ctx := &Context{
		user_data: cfg.user_data
		width: cfg.width
		height: cfg.height
		config: cfg
		ft: 0
		ui_mode: cfg.ui_mode
		native_rendering: cfg.native_rendering
	}
	if cfg.user_data == unsafe { nil } {
		ctx.user_data = ctx
	}
	ctx.set_bg_color(cfg.bg_color)
	// C.printf('new_context() %p\n', cfg.user_data)
	window := sapp.Desc{
		user_data: ctx
		init_userdata_cb: gg_init_sokol_window
		frame_userdata_cb: gg_frame_fn
		event_userdata_cb: gg_event_fn
		fail_userdata_cb: gg_fail_fn
		cleanup_userdata_cb: gg_cleanup_fn
		window_title: &char(cfg.window_title.str)
		html5_canvas_name: &char(cfg.window_title.str)
		width: cfg.width
		height: cfg.height
		sample_count: cfg.sample_count
		high_dpi: true
		fullscreen: cfg.fullscreen
		__v_native_render: cfg.native_rendering
		// drag&drop
		enable_dragndrop: cfg.enable_dragndrop
		max_dropped_files: cfg.max_dropped_files
		max_dropped_file_path_length: cfg.max_dropped_file_path_length
		swap_interval: cfg.swap_interval
	}
	ctx.window = window
	return ctx
}

// run starts the main loop of the context.
pub fn (ctx &Context) run() {
	sapp.run(&ctx.window)
}

// quit closes the context window and exits the event loop for it
pub fn (ctx &Context) quit() {
	sapp.request_quit() // does not require ctx right now, but sokol multi-window might in the future
}

// set_bg_color sets the color of the window background to `c`.
pub fn (mut ctx Context) set_bg_color(c gx.Color) {
	ctx.clear_pass = gfx.create_clear_pass(f32(c.r) / 255.0, f32(c.g) / 255.0, f32(c.b) / 255.0,
		f32(c.a) / 255.0)
}

// Resize the context's Window
pub fn (mut ctx Context) resize(width int, height int) {
	ctx.width = width
	ctx.height = height
}

// refresh_ui requests a complete re-draw of the window contents.
pub fn (mut ctx Context) refresh_ui() {
	ctx.needs_refresh = true
	ctx.ticks = 0
}

// begin prepares the context for drawing.
pub fn (ctx &Context) begin() {
	if ctx.render_text && ctx.font_inited {
		ctx.ft.flush()
	}
	sgl.defaults()
	sgl.matrix_mode_projection()
	sgl.ortho(0.0, f32(sapp.width()), f32(sapp.height()), 0.0, -1.0, 1.0)
}

// end finishes drawing for the context.
pub fn (ctx &Context) end() {
	$if show_fps ? {
		ctx.show_fps()
	} $else {
		if ctx.fps.show {
			ctx.show_fps()
		}
	}
	gfx.begin_default_pass(ctx.clear_pass, sapp.width(), sapp.height())
	sgl.draw()
	gfx.end_pass()
	gfx.commit()
	/*
	if gg.config.wait_events {
		// println('gg: waiting')
		wait_events()
	}
	*/
}

pub struct FPSConfig {
pub mut:
	x           int
	y           int
	width       int
	height      int
	show        bool // do not show by default, use `-d show_fps` or set it manually in your app to override with: `app.gg.fps.show = true`
	text_config gx.TextCfg = gx.TextCfg{
		color: gx.yellow
		size: 20
		align: .center
		vertical_align: .middle
	}
	background_color gx.Color = gx.Color{
		r: 0
		g: 0
		b: 0
		a: 128
	}
}

pub fn (ctx &Context) show_fps() {
	if !ctx.font_inited {
		return
	}
	frame_duration := sapp.frame_duration()
	sgl.defaults()
	sgl.matrix_mode_projection()
	sgl.ortho(0.0, f32(sapp.width()), f32(sapp.height()), 0.0, -1.0, 1.0)
	ctx.set_text_cfg(ctx.fps.text_config)
	if ctx.fps.width == 0 {
		mut fps := unsafe { &ctx.fps }
		fps.width, fps.height = ctx.text_size('00') // maximum size; prevents blinking on variable width fonts
	}
	fps_text := int(0.5 + 1.0 / frame_duration).str()
	ctx.draw_rect_filled(ctx.fps.x, ctx.fps.y, ctx.fps.width + 2, ctx.fps.height + 4,
		ctx.fps.background_color)
	ctx.draw_text(ctx.fps.x + ctx.fps.width / 2 + 1, ctx.fps.y + ctx.fps.height / 2 + 2,
		fps_text, ctx.fps.text_config)
}

fn (mut ctx Context) set_scale() {
	mut s := sapp.dpi_scale()
	$if android {
		w := ctx.config.width
		h := ctx.config.height
		dw := sapp.width()
		dh := sapp.height()
		if dw <= dh {
			if w <= 0 {
				s = 1.0
			} else {
				s = f32(dw) / w
			}
		} else {
			if h <= 0 {
				s = 1.0
			} else {
				s = f32(dh) / h
			}
		}
	}
	// Note: on older X11, `Xft.dpi` from ~/.Xresources, that sokol uses,
	// may not be set which leads to sapp.dpi_scale reporting incorrectly 0.0
	if s < 0.1 {
		s = 1.0
	}
	ctx.scale = s
}

// window_size returns the current dimensions of the window.
pub fn (ctx Context) window_size() Size {
	s := ctx.scale
	return Size{int(sapp.width() / s), int(sapp.height() / s)}
}

//---- public module functions

// dpi_scale returns the DPI scale coefficient for the screen.
// Do not use for Android development, use `Context.scale` instead.
pub fn dpi_scale() f32 {
	mut s := sapp.dpi_scale()
	$if android {
		s *= android_dpi_scale()
	}
	// Note: on older X11, `Xft.dpi` from ~/.Xresources, that sokol uses,
	// may not be set which leads to sapp.dpi_scale reporting incorrectly 0.0
	if s < 0.1 {
		s = 1.0
	}
	return s
}

// high_dpi returns true if `gg` is running on a high DPI monitor or screen.
pub fn high_dpi() bool {
	return C.sapp_high_dpi()
}

// screen_size returns the size of the active screen.
pub fn screen_size() Size {
	$if macos {
		return C.gg_get_screen_size()
	}
	$if windows {
		return Size{
			width: int(C.GetSystemMetrics(C.SM_CXSCREEN))
			height: int(C.GetSystemMetrics(C.SM_CYSCREEN))
		}
	}
	// TODO linux, etc
	return Size{}
}

// window_size returns the `Size` of the active window.
// Do not use for Android development, use `Context.window_size()` instead.
pub fn window_size() Size {
	s := dpi_scale()
	return Size{int(sapp.width() / s), int(sapp.height() / s)}
}

// window_size_real_pixels returns the `Size` of the active window without scale
pub fn window_size_real_pixels() Size {
	return Size{sapp.width(), sapp.height()}
}

/*
pub fn wait_events() {
	unsafe {
		$if macos {
			#NSEvent *event = [NSApp nextEventMatchingMask:NSEventMaskAny
			#untilDate:[NSDate distantFuture]
			#inMode:NSDefaultRunLoopMode
			#dequeue:YES];
			#[NSApp sendEvent:event];
		}
		$if windows {
			C.WaitMessage()
		}
	}
}
*/
