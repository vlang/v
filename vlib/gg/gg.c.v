// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.

module gg

import os
import gx
import sokol
import sokol.sapp
import sokol.sgl
import sokol.gfx
import math

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
	touches            [8]C.sapp_touchpoint
	window_width       int
	window_height      int
	framebuffer_width  int
	framebuffer_height int
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
	scale f32 = 1.0
	// will get set to 2.0 for retina, will remain 1.0 for normal
	width       int
	height      int
	clear_pass  C.sg_pass_action
	window      C.sapp_desc
	timage_pip  C.sgl_pipeline
	config      Config
	user_data   voidptr
	ft          &FT
	font_inited bool
	ui_mode     bool // do not redraw everything 60 times/second, but only when the user requests
	frame       u64  // the current frame counted from the start of the application; always increasing
	//
	mbtn_mask     byte
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
}

fn gg_init_sokol_window(user_data voidptr) {
	mut g := unsafe { &Context(user_data) }
	desc := sapp.create_desc()
	/*
	desc := C.sg_desc{
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
	sgl_desc := C.sgl_desc_t{}
	sgl.setup(&sgl_desc)
	g.scale = dpi_scale()
	// is_high_dpi := sapp.high_dpi()
	// fb_w := sapp.width()
	// fb_h := sapp.height()
	// println('g.scale=$g.scale is_high_dpi=$is_high_dpi fb_w=$fb_w fb_h=$fb_h')
	// if g.config.init_text {
	// `os.is_file()` won't work on Android if the font file is embedded into the APK
	exists := $if !android { os.is_file(g.config.font_path) } $else { true }
	if g.config.font_path != '' && !exists {
		g.render_text = false
	} else if g.config.font_path != '' && exists {
		// t := time.ticks()
		g.ft = new_ft(
			font_path: g.config.font_path
			custom_bold_font_path: g.config.custom_bold_font_path
			scale: dpi_scale()
		) or { panic(err) }
		// println('FT took ${time.ticks()-t} ms')
		g.font_inited = true
	} else {
		if g.config.font_bytes_normal.len > 0 {
			g.ft = new_ft(
				bytes_normal: g.config.font_bytes_normal
				bytes_bold: g.config.font_bytes_bold
				bytes_mono: g.config.font_bytes_mono
				bytes_italic: g.config.font_bytes_italic
				scale: sapp.dpi_scale()
			) or { panic(err) }
			g.font_inited = true
		} else {
			sfont := system_font_path()
			if g.config.font_path != '' {
				eprintln('font file "$g.config.font_path" does not exist, the system font ($sfont) was used instead.')
			}

			g.ft = new_ft(
				font_path: sfont
				custom_bold_font_path: g.config.custom_bold_font_path
				scale: sapp.dpi_scale()
			) or { panic(err) }
			g.font_inited = true
		}
	}
	//
	mut pipdesc := C.sg_pipeline_desc{
		label: c'alpha_image'
	}
	unsafe { vmemset(&pipdesc, 0, int(sizeof(pipdesc))) }

	color_state := C.sg_color_state{
		blend: C.sg_blend_state{
			enabled: true
			src_factor_rgb: gfx.BlendFactor(C.SG_BLENDFACTOR_SRC_ALPHA)
			dst_factor_rgb: gfx.BlendFactor(C.SG_BLENDFACTOR_ONE_MINUS_SRC_ALPHA)
		}
	}
	pipdesc.colors[0] = color_state

	g.timage_pip = sgl.make_pipeline(&pipdesc)
	//
	if g.config.init_fn != voidptr(0) {
		g.config.init_fn(g.user_data)
	}
	// Create images now that we can do that after sg is inited
	if g.native_rendering {
		return
	}

	for i in 0 .. g.image_cache.len {
		if g.image_cache[i].simg.id == 0 {
			g.image_cache[i].init_sokol_image()
		}
	}
}

//
pub fn new_context(cfg Config) &Context {
	mut g := &Context{
		user_data: cfg.user_data
		width: cfg.width
		height: cfg.height
		config: cfg
		ft: 0
		ui_mode: cfg.ui_mode
		native_rendering: cfg.native_rendering
	}
	if isnil(cfg.user_data) {
		g.user_data = g
	}
	g.set_bg_color(cfg.bg_color)
	// C.printf('new_context() %p\n', cfg.user_data)
	window := C.sapp_desc{
		user_data: g
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
	g.window = window
	return g
}

pub fn (ctx &Context) draw_circle_line(x f32, y f32, r int, segments int, c gx.Color) {
	$if macos {
		if ctx.native_rendering {
			C.darwin_draw_circle(x - r + 1, ctx.height - (y + r + 3), r, c)
			return
		}
	}
	if c.a != 255 {
		sgl.load_pipeline(ctx.timage_pip)
	}
	sgl.c4b(c.r, c.g, c.b, c.a)
	nx := x * ctx.scale
	ny := y * ctx.scale
	nr := r * ctx.scale
	mut theta := f32(0)
	mut xx := f32(0)
	mut yy := f32(0)
	sgl.begin_line_strip()
	for i := 0; i < segments + 1; i++ {
		theta = 2.0 * f32(math.pi) * f32(i) / f32(segments)
		xx = nr * math.cosf(theta)
		yy = nr * math.sinf(theta)
		sgl.v2f(xx + nx, yy + ny)
	}
	sgl.end()
}

pub fn high_dpi() bool {
	return C.sapp_high_dpi()
}

pub fn screen_size() Size {
	$if macos {
		return C.gg_get_screen_size()
	}
	// TODO windows, linux, etc
	return Size{}
}

fn C.WaitMessage()

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

// TODO: Fix alpha
pub fn (ctx &Context) draw_rect(x f32, y f32, w f32, h f32, c gx.Color) {
	$if macos {
		if ctx.native_rendering {
			C.darwin_draw_rect(x, ctx.height - (y + h), w, h, c)
			return
		}
	}
	if c.a != 255 {
		sgl.load_pipeline(ctx.timage_pip)
	}
	sgl.c4b(c.r, c.g, c.b, c.a)
	sgl.begin_quads()
	sgl.v2f(x * ctx.scale, y * ctx.scale)
	sgl.v2f((x + w) * ctx.scale, y * ctx.scale)
	sgl.v2f((x + w) * ctx.scale, (y + h) * ctx.scale)
	sgl.v2f(x * ctx.scale, (y + h) * ctx.scale)
	sgl.end()
}
