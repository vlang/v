// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.

module gg

import os
import os.font
import gx
import time
import sokol.sapp
import sokol.sgl
import sokol.gfx

@[typedef]
struct C.XRRScreenResources {
	noutput int
	outputs &int
}

@[typedef]
struct C.XRROutputInfo {
	crtc u64
}

@[typedef]
struct C.XRRCrtcInfo {
	width  u32
	height u32
}

fn C.XOpenDisplay(int) voidptr
fn C.XCloseDisplay(voidptr) int
fn C.DefaultScreen(voidptr) int
fn C.DefaultRootWindow(voidptr) u64
fn C.XRRGetScreenResources(voidptr, u64) &C.XRRScreenResources
fn C.XRRGetOutputPrimary(voidptr, u64) u64
fn C.XRRFreeScreenResources(&C.XRRScreenResources)
fn C.XRRGetOutputInfo(voidptr, &C.XRRScreenResources, u64) &C.XRROutputInfo
fn C.XRRFreeOutputInfo(&C.XRROutputInfo)
fn C.XRRGetCrtcInfo(voidptr, &C.XRRScreenResources, u64) &C.XRRCrtcInfo
fn C.XRRFreeCrtcInfo(&C.XRRCrtcInfo)

$if linux {
	#flag -lXrandr
	#include <X11/extensions/Xrandr.h>
}

$if windows {
	#flag -lgdi32
	#include "windows.h"
}

#flag wasm32_emscripten --embed-file @VEXEROOT/examples/assets/fonts/RobotoMono-Regular.ttf@/assets/fonts/RobotoMono-Regular.ttf

// call Windows API to get screen size
fn C.GetSystemMetrics(int) int

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
	width         int     // desired start width of the window
	height        int     // desired start height of the window
	retina        bool    // TODO: implement or deprecate
	resizable     bool    // TODO: implement or deprecate
	user_data     voidptr // a custom pointer to the application data/instance
	font_size     int     // TODO: implement or deprecate
	create_window bool    // TODO: implement or deprecate
	// window_user_ptr voidptr
	window_title      string // the desired title of the window
	icon              sapp.IconDesc
	html5_canvas_name string = 'canvas'
	borderless_window bool     // TODO: implement or deprecate
	always_on_top     bool     // TODO: implement or deprecate
	bg_color          gx.Color // The background color of the window. By default, the first thing gg does in ctx.begin(), is clear the whole buffer with that color.
	init_fn           FNCb   = unsafe { nil } // Called once, after Sokol has finished its setup. Some gg and Sokol functions have to be called *in this* callback, or after this callback, but not before
	frame_fn          FNCb   = unsafe { nil } // Called once per frame, usually 60 times a second (depends on swap_interval). See also https://dri.freedesktop.org/wiki/ConfigurationOptions/#synchronizationwithverticalrefreshswapintervals
	native_frame_fn   FNCb   = unsafe { nil }
	cleanup_fn        FNCb   = unsafe { nil } // Called once, after Sokol determines that the application is finished/closed. Put your app specific cleanup/free actions here.
	fail_fn           FNFail = unsafe { nil } // Called once per Sokol error/log message. TODO: currently it does nothing with latest Sokol, reimplement using Sokol's new sapp_logger APIs.

	event_fn FNEvent  = unsafe { nil } // Called once per each user initiated event, received by Sokol/GG.
	on_event FNEvent2 = unsafe { nil } // Called once per each user initiated event, received by Sokol/GG. Same as event_fn, just the parameter order is different. TODO: deprecate this, in favor of event_fn
	quit_fn  FNEvent  = unsafe { nil } // Called when the user closes the app window.

	keydown_fn FNKeyDown = unsafe { nil } // Called once per key press, no matter how long the key is held down. Note that here you can access the scan code/physical key, but not the logical character.
	keyup_fn   FNKeyUp   = unsafe { nil } // Called once per key press, when the key is released.
	char_fn    FNChar    = unsafe { nil } // Called once per character (after the key is pressed down, and then released). Note that you can access the character/utf8 rune here, not just the scan code.

	move_fn    FNMove    = unsafe { nil } // Called while the mouse/touch point is moving.
	click_fn   FNClick   = unsafe { nil } // Called once when the mouse/touch button is clicked.
	unclick_fn FNUnClick = unsafe { nil } // Called once when the mouse/touch button is released.
	leave_fn   FNEvent   = unsafe { nil } // Called once when the mouse/touch point leaves the window.
	enter_fn   FNEvent   = unsafe { nil } // Called once when the mouse/touch point enters again the window.
	resized_fn FNEvent   = unsafe { nil } // Called once when the window has changed its size.
	scroll_fn  FNEvent   = unsafe { nil } // Called while the user is scrolling. The direction of scrolling is indicated by either 1 or -1.
	// wait_events       bool // set this to true for UIs, to save power
	fullscreen    bool // set this to true, if you want your window to start in fullscreen mode (suitable for games/demos/screensavers)
	scale         f32 = 1.0
	sample_count  int // bigger values usually have performance impact, but can produce smoother/antialiased lines, if you draw lines or polygons (2 is usually good enough)
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
	max_dropped_files            int = 1    // max number of dropped files to process (default: 1)
	max_dropped_file_path_length int = 2048 // max length in bytes of a dropped UTF-8 file path (default: 2048)

	min_width  int // desired minimum width of the window
	min_height int // desired minimum height of the window
}

@[heap]
pub struct PipelineContainer {
pub mut:
	alpha sgl.Pipeline
	add   sgl.Pipeline
}

fn (mut container PipelineContainer) init_pipeline() {
	// FIXME(FireRedz): this looks kinda funny, find a better way to initialize pipeline.

	// Alpha
	mut alpha_pipdesc := gfx.PipelineDesc{}
	unsafe { vmemset(&alpha_pipdesc, 0, int(sizeof(alpha_pipdesc))) }
	alpha_pipdesc.label = c'alpha-pipeline'
	alpha_pipdesc.colors[0] = gfx.ColorTargetState{
		blend: gfx.BlendState{
			enabled:        true
			src_factor_rgb: .src_alpha
			dst_factor_rgb: .one_minus_src_alpha
		}
	}
	container.alpha = sgl.make_pipeline(&alpha_pipdesc)

	// Add
	mut add_pipdesc := gfx.PipelineDesc{}
	unsafe { vmemset(&add_pipdesc, 0, int(sizeof(add_pipdesc))) }
	add_pipdesc.label = c'additive-pipeline'
	add_pipdesc.colors[0] = gfx.ColorTargetState{
		blend: gfx.BlendState{
			enabled:        true
			src_factor_rgb: .src_alpha
			dst_factor_rgb: .one
		}
	}
	container.add = sgl.make_pipeline(&add_pipdesc)
}

@[heap]
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
	pipeline    &PipelineContainer = unsafe { nil }
	config      Config
	user_data   voidptr
	ft          &FT = unsafe { nil }
	font_inited bool
	ui_mode     bool // do not redraw everything 60 times/second, but only when the user requests
	frame       u64  // the current frame counted from the start of the application; always increasing
	timer       time.StopWatch

	mbtn_mask     u8
	mouse_buttons MouseButtons // typed version of mbtn_mask; easier to use for user programs
	mouse_pos_x   int
	mouse_pos_y   int
	mouse_dx      int
	mouse_dy      int
	scroll_x      int
	scroll_y      int

	key_modifiers     Modifier           // the current key modifiers
	key_repeat        bool               // whether the pressed key was an autorepeated one
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
			font_path:             ctx.config.font_path
			custom_bold_font_path: ctx.config.custom_bold_font_path
			scale:                 ctx.scale
		) or { panic(err) }
		// println('FT took ${time.ticks()-t} ms')
		ctx.font_inited = true
	} else {
		if ctx.config.font_bytes_normal.len > 0 {
			ctx.ft = new_ft(
				bytes_normal: ctx.config.font_bytes_normal
				bytes_bold:   ctx.config.font_bytes_bold
				bytes_mono:   ctx.config.font_bytes_mono
				bytes_italic: ctx.config.font_bytes_italic
				scale:        sapp.dpi_scale()
			) or { panic(err) }
			ctx.font_inited = true
		} else {
			sfont := font.default()
			if ctx.config.font_path != '' {
				eprintln('font file "${ctx.config.font_path}" does not exist, the system font (${sfont}) was used instead.')
			}

			ctx.ft = new_ft(
				font_path:             sfont
				custom_bold_font_path: ctx.config.custom_bold_font_path
				scale:                 sapp.dpi_scale()
			) or { panic(err) }
			ctx.font_inited = true
		}
	}

	// Pipeline
	ctx.pipeline = &PipelineContainer{}
	ctx.pipeline.init_pipeline()

	ctx.timer = time.new_stopwatch()
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
						typ:           .resized
						window_width:  ctx.width
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

fn gg_frame_fn(mut ctx Context) {
	ctx.frame++
	if ctx.config.frame_fn == unsafe { nil } {
		return
	}
	if ctx.native_rendering {
		// return
	}
	defer {
		ctx.mouse_dx = 0
		ctx.mouse_dy = 0
		ctx.scroll_x = 0
		ctx.scroll_y = 0
	}

	ctx.record_frame()
	ctx.memory_trace_frame()

	if ctx.ui_mode && !ctx.needs_refresh {
		// println('ui mode, exiting')
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
	e.mouse_x /= ctx.scale
	e.mouse_y /= ctx.scale
	e.mouse_dx /= ctx.scale
	e.mouse_dy /= ctx.scale
	e.scroll_x /= ctx.scale
	e.scroll_y /= ctx.scale
	ctx.mouse_pos_x = int(e.mouse_x)
	ctx.mouse_pos_y = int(e.mouse_y)
	ctx.mouse_dx = int(e.mouse_dx)
	ctx.mouse_dy = int(e.mouse_dy)
	ctx.scroll_x = int(e.scroll_x)
	ctx.scroll_y = int(e.scroll_y)
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
	} else if ctx.config.on_event != unsafe { nil } {
		ctx.config.on_event(ctx.config.user_data, e)
	}
	match e.typ {
		.mouse_move {
			if ctx.config.move_fn != unsafe { nil } {
				ctx.config.move_fn(e.mouse_x, e.mouse_y, ctx.config.user_data)
			}
		}
		.mouse_down {
			if ctx.config.click_fn != unsafe { nil } {
				ctx.config.click_fn(e.mouse_x, e.mouse_y, e.mouse_button, ctx.config.user_data)
			}
		}
		.mouse_up {
			if ctx.config.unclick_fn != unsafe { nil } {
				ctx.config.unclick_fn(e.mouse_x, e.mouse_y, e.mouse_button, ctx.config.user_data)
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
			ctx.scale = dpi_scale()
			ctx.ft.scale = ctx.scale
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
	$if linux {
		if e.typ == .key_down && e.key_code in [.backspace, .delete, .enter, .tab] {
			// with X11, sokol does not send .char events for some keys; we will emulate them for consistency here:
			e.char_code = match e.key_code {
				.backspace { u32(8) }
				.tab { 9 }
				.enter { 13 }
				.delete { 127 }
				else { u32(e.key_code) }
			}
			e.key_code = .invalid
			e.typ = .char
			if ctx.config.event_fn != unsafe { nil } {
				ctx.config.event_fn(e, ctx.config.user_data)
			} else if ctx.config.on_event != unsafe { nil } {
				ctx.config.on_event(ctx.config.user_data, e)
			}
			if ctx.config.char_fn != unsafe { nil } {
				ctx.config.char_fn(e.char_code, ctx.config.user_data)
			}
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
		eprintln('gg error: ${vmsg}')
	}
}

//---- public methods

// start creates a new context and runs it right away.
// It is a convenient way to start short/throwaway gg based prototypes,
// that do not need to keep and update their own state, like simple
// animations/visualisations that depend only on the time, or the ctx.frame counter.
// Use gg.new_context() for more complex ones.
pub fn start(cfg Config) {
	mut ctx := new_context(cfg)
	ctx.run()
}

// new_context returns an initialized `Context` allocated on the heap.
pub fn new_context(cfg Config) &Context {
	mut ctx := &Context{
		user_data:        cfg.user_data
		width:            cfg.width
		height:           cfg.height
		config:           cfg
		ft:               unsafe { nil }
		ui_mode:          cfg.ui_mode
		native_rendering: cfg.native_rendering
		window:           sapp.Desc{
			init_userdata_cb:  gg_init_sokol_window
			frame_userdata_cb: gg_frame_fn
			event_userdata_cb: gg_event_fn
			// fail_userdata_cb: gg_fail_fn
			cleanup_userdata_cb: gg_cleanup_fn
			window_title:        &char(cfg.window_title.str)
			icon:                cfg.icon
			html5_canvas_name:   &char(cfg.html5_canvas_name.str)
			width:               cfg.width
			height:              cfg.height
			sample_count:        cfg.sample_count
			high_dpi:            true
			fullscreen:          cfg.fullscreen
			__v_native_render:   cfg.native_rendering
			min_width:           cfg.min_width
			min_height:          cfg.min_height
			// drag&drop
			enable_dragndrop:             cfg.enable_dragndrop
			max_dropped_files:            cfg.max_dropped_files
			max_dropped_file_path_length: cfg.max_dropped_file_path_length
			swap_interval:                cfg.swap_interval
		}
	}
	ctx.set_bg_color(cfg.bg_color)
	// C.printf('new_context() %p\n', cfg.user_data)
	return ctx
}

// run starts the main loop of the context.
pub fn (mut ctx Context) run() {
	// set context late, in case it changed (e.g., due to embedding)
	ctx.window = sapp.Desc{
		...ctx.window
		user_data: ctx
	}
	if ctx.user_data == unsafe { nil } {
		ctx.user_data = ctx
	}
	sapp.run(&ctx.window)
}

// quit closes the context window and exits the event loop for it
pub fn (ctx &Context) quit() {
	sapp.request_quit() // does not require ctx right now, but sokol multi-window might in the future
}

// set_bg_color sets the color of the window background to `c`.
pub fn (mut ctx Context) set_bg_color(c gx.Color) {
	ctx.clear_pass = gfx.create_clear_pass_action(f32(c.r) / 255.0, f32(c.g) / 255.0,
		f32(c.b) / 255.0, f32(c.a) / 255.0)
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

pub enum EndEnum {
	clear
	passthru
}

@[params]
pub struct EndOptions {
pub:
	how EndEnum
}

const dontcare_pass = gfx.PassAction{
	colors: [
		gfx.ColorAttachmentAction{
			load_action: .dontcare
			clear_value: gfx.Color{1.0, 1.0, 1.0, 1.0}
		},
		gfx.ColorAttachmentAction{
			load_action: .dontcare
			clear_value: gfx.Color{1.0, 1.0, 1.0, 1.0}
		},
		gfx.ColorAttachmentAction{
			load_action: .dontcare
			clear_value: gfx.Color{1.0, 1.0, 1.0, 1.0}
		},
		gfx.ColorAttachmentAction{
			load_action: .dontcare
			clear_value: gfx.Color{1.0, 1.0, 1.0, 1.0}
		},
	]!
}

pub fn create_default_pass(action gfx.PassAction) gfx.Pass {
	return sapp.create_default_pass(action)
}

// end finishes all the drawing for the context ctx.
// All accumulated draw calls before ctx.end(), will be done in a separate Sokol pass.
//
// Note: each Sokol pass, has a limit on the number of draw calls, that can be done in it.
// Once that limit is reached, the whole pass will not draw anything, which can be frustrating.
//
// To overcome this limitation, you may use *several passes*, when you want to make thousands
// of draw calls (for example, if you need to draw thousands of circles/rectangles/sprites etc),
// where each pass will render just a limited amount of primitives.
//
// In the context of the gg module (without dropping to using sgl and gfx directly), it means,
// that you will need a new pair of ctx.begin() and ctx.end() calls, surrounding all the draw
// calls, that should be done in each pass.
//
// The default ctx.end() is equivalent to ctx.end(how:.clear). It will erase the existing
// rendered content with the background color, before drawing anything else.
// You can call ctx.end(how:.passthru) for a pass, that *will not* erase the previously
// rendered content in the context.
pub fn (ctx &Context) end(options EndOptions) {
	$if show_fps ? {
		ctx.show_fps()
	} $else {
		if ctx.fps.show {
			ctx.show_fps()
		}
	}
	pass := match options.how {
		.clear {
			create_default_pass(ctx.clear_pass)
		}
		.passthru {
			create_default_pass(dontcare_pass)
		}
	}
	gfx.begin_pass(pass)
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
	x                int  // horizontal position on screen
	y                int  // vertical position on screen
	width            int  // minimum width
	height           int  // minimum height
	show             bool // do not show by default, use `-d show_fps` or set it manually in your app to override with: `app.gg.fps.show = true`
	text_config      gx.TextCfg = gx.TextCfg{
		color:          gx.yellow
		size:           20
		align:          .center
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
	fps_text := int(0.5 + 1.0 / frame_duration).str()
	if ctx.fps.width == 0 {
		mut fps := unsafe { &ctx.fps }
		fps.width, fps.height = ctx.text_size('00') // usual size; prevents blinking on variable width fonts
	}
	char_width := ctx.fps.text_config.size / 2
	mut full_width := ctx.fps.width
	if char_width * fps_text.len > ctx.fps.width {
		full_width += (fps_text.len - 2) * char_width
	}
	ctx.draw_rect_filled(ctx.fps.x, ctx.fps.y, full_width + 2, ctx.fps.height + 4, ctx.fps.background_color)
	ctx.draw_text(ctx.fps.x + full_width / 2 + 1, ctx.fps.y + ctx.fps.height / 2 + 2,
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
			width:  int(C.GetSystemMetrics(C.SM_CXSCREEN))
			height: int(C.GetSystemMetrics(C.SM_CYSCREEN))
		}
	}
	$if linux {
		display := C.XOpenDisplay(0)
		if display == unsafe { nil } {
			return Size{}
		}
		defer { C.XCloseDisplay(display) }
		root := C.DefaultRootWindow(display)
		resources := C.XRRGetScreenResources(display, root)
		if resources == unsafe { nil } {
			return Size{}
		}
		defer { C.XRRFreeScreenResources(resources) }
		primary_output := C.XRRGetOutputPrimary(display, root)
		if primary_output == 0 {
			return Size{}
		}
		for i := 0; i < resources.noutput; i++ {
			if unsafe { u64(resources.outputs[i]) } == primary_output {
				output_info := C.XRRGetOutputInfo(display, resources, unsafe { resources.outputs[i] })
				if output_info == unsafe { nil } {
					return Size{}
				}
				crtc_info := C.XRRGetCrtcInfo(display, resources, output_info.crtc)
				C.XRRFreeOutputInfo(output_info)
				if crtc_info == unsafe { nil } {
					return Size{}
				}
				res := Size{
					width:  unsafe { int(crtc_info.width) }
					height: unsafe { int(crtc_info.height) }
				}
				C.XRRFreeCrtcInfo(crtc_info)
				return res
			}
		}
	}
	return Size{}
}

// window_size returns the `Size` of the active window.
// Do not use for Android development, use `Context.window_size()` instead.
pub fn window_size() Size {
	s := dpi_scale()
	return Size{int(sapp.width() / s), int(sapp.height() / s)}
}

// set_window_title sets main window's title
pub fn set_window_title(title string) {
	C.sapp_set_window_title(&char(title.str))
}

// window_size_real_pixels returns the `Size` of the active window without scale
pub fn window_size_real_pixels() Size {
	return Size{sapp.width(), sapp.height()}
}

// is it fullscreen
pub fn is_fullscreen() bool {
	return sapp.is_fullscreen()
}

// toggle fullscreen
pub fn toggle_fullscreen() {
	sapp.toggle_fullscreen()
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

// memory_trace_frame creates a small allocation at the start of each frame,
// that is easy to search for in memdump.bin files, created with:
// -prealloc -d prealloc_memset -d prealloc_memset_value=65 -d prealloc_dump -d gg_memory_trace_frame
@[if gg_memory_trace_frame ?; manualfree]
fn (mut ctx Context) memory_trace_frame() {
	frame_tag_size := 61 // uneven, and easy to spot in heaptrack histograms as well
	unsafe {
		frame_tag := &u8(vcalloc(frame_tag_size))
		C.snprintf(frame_tag, frame_tag_size, c'@@ gg_memory_trace_frame: %06d ', ctx.frame)
		frame_tag[frame_tag_size - 2] = `@`
		frame_tag[frame_tag_size - 1] = `@`
		free(frame_tag)
	}
}
