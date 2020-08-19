// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module gg

import gx
import sokol
import sokol.sapp
import sokol.sgl
import sokol.gfx

// import time
pub type FNCb = fn (x voidptr)

pub type FNEvent = fn (e, x voidptr)

pub type FNFail = fn (msg string, x voidptr)

pub type FNKeyDown = fn (c sapp.KeyCode, m sapp.Modifier, x voidptr)

pub type FNChar = fn (c u32, x voidptr)

pub struct Config {
pub:
	width             int
	height            int
	use_ortho         bool
	retina            bool
	resizable         bool
	user_data         voidptr
	font_size         int
	create_window     bool
	// window_user_ptr voidptr
	window_title      string
	borderless_window bool
	always_on_top     bool
	bg_color          gx.Color
	init_fn           FNCb = voidptr(0)
	frame_fn          FNCb = voidptr(0)
	cleanup_fn        FNCb = voidptr(0)
	fail_fn           FNFail = voidptr(0)
	event_fn          FNEvent = voidptr(0)
	keydown_fn        FNKeyDown = voidptr(0) // special case of event_fn
	char_fn           FNChar = voidptr(0) // special case of event_fn
	wait_events       bool // set this to true for UIs, to save power
	fullscreen        bool
	scale             f32 = 1.0 // vid needs this
	// init_text bool
	font_path         string
}

pub struct Context {
	render_text bool
mut:
	// a cache with all images created by the user. used for sokol image init and to save space
	// (so that the user can store image ids, not entire Image objects)
	image_cache []Image
pub mut:
	scale       f32 = 1.0 // will get set to 2.0 for retina, will remain 1.0 for normal
	width       int
	height      int
	clear_pass  C.sg_pass_action
	window      C.sapp_desc
	timage_pip  C.sgl_pipeline
	config      Config
	ft          &FT
	font_inited bool
}

pub struct Size {
pub:
	width  int
	height int
}

fn gg_init_sokol_window(user_data voidptr) {
	mut g := &Context(user_data)
	desc := C.sg_desc{
		mtl_device: sapp.metal_get_device()
		mtl_renderpass_descriptor_cb: sapp.metal_get_renderpass_descriptor
		mtl_drawable_cb: sapp.metal_get_drawable
		d3d11_device: sapp.d3d11_get_device()
		d3d11_device_context: sapp.d3d11_get_device_context()
		d3d11_render_target_view_cb: sapp.d3d11_get_render_target_view
		d3d11_depth_stencil_view_cb: sapp.d3d11_get_depth_stencil_view
	}
	gfx.setup(&desc)
	sgl_desc := C.sgl_desc_t{}
	sgl.setup(&sgl_desc)
	g.scale = sapp.dpi_scale()
	// NB: on older X11, `Xft.dpi` from ~/.Xresources, that sokol uses,
	// may not be set which leads to sapp.dpi_scale reporting incorrectly 0.0
	if g.scale < 0.1 {
		g.scale = 1.0
	}
	// is_high_dpi := sapp.high_dpi()
	// fb_w := sapp.width()
	// fb_h := sapp.height()
	// println('g.scale=$g.scale is_high_dpi=$is_high_dpi fb_w=$fb_w fb_h=$fb_h')
	// if g.config.init_text {
	if g.config.font_path != '' {
		// t := time.ticks()
		g.ft = new_ft({
			font_path: g.config.font_path
			scale: sapp.dpi_scale()
		}) or {
			panic(err)
		}
		// println('FT took ${time.ticks()-t} ms')
		g.font_inited = true
	}
	//
	mut pipdesc := C.sg_pipeline_desc{}
	unsafe { C.memset(&pipdesc, 0, sizeof(pipdesc)) }
	pipdesc.blend.enabled = true
	pipdesc.blend.src_factor_rgb = C.SG_BLENDFACTOR_SRC_ALPHA
	pipdesc.blend.dst_factor_rgb = C.SG_BLENDFACTOR_ONE_MINUS_SRC_ALPHA
	g.timage_pip = sgl.make_pipeline(&pipdesc)
	//
	if g.config.init_fn != voidptr(0) {
		g.config.init_fn(g.config.user_data)
	}
	// Create images now that we can do that after sg is inited
	for i in 0..g.image_cache.len {
		g.image_cache[i].init_sokol_image()
	}
}

fn gg_frame_fn(user_data voidptr) {
	mut g := &Context(user_data)
	if g.config.frame_fn != voidptr(0) {
		g.config.frame_fn(g.config.user_data)
	}
}

// TODO: remove this hacky workaround...
// NB: todo_remove_this is needed to workaround a v bug,
// where it thinks that &sapp.Event(x) is a function call,
// instead of a cast, if v has not yet seen &sapp.Event used
// as a parameter type.
fn todo_remove_this(e &sapp.Event) {
}

fn gg_event_fn(ce &C.sapp_event, user_data voidptr) {
	e := &sapp.Event(ce)
	mut g := &Context(user_data)
	if g.config.event_fn != voidptr(0) {
		g.config.event_fn(e, g.config.user_data)
	}
	match e.typ {
		.key_down {
			if g.config.keydown_fn != voidptr(0) {
				kdfn := g.config.keydown_fn
				kdfn(e.key_code, e.modifiers, g.config.user_data)
			}
		}
		.char {
			if g.config.char_fn != voidptr(0) {
				cfn := g.config.char_fn
				cfn(e.char_code, g.config.user_data)
			}
		}
		else {}
	}
}

fn gg_cleanup_fn(user_data voidptr) {
	mut g := &Context(user_data)
	if g.config.cleanup_fn != voidptr(0) {
		g.config.cleanup_fn(g.config.user_data)
	}
}

fn gg_fail_fn(msg charptr, user_data voidptr) {
	mut g := &Context(user_data)
	vmsg := tos3(msg)
	if g.config.fail_fn != voidptr(0) {
		g.config.fail_fn(vmsg, g.config.user_data)
	} else {
		eprintln('gg error: $vmsg')
	}
}

//
pub fn new_context(cfg Config) &Context {
	mut g := &Context{
		width: cfg.width
		height: cfg.height
		config: cfg
		render_text: cfg.font_path != ''
		ft: 0
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
		window_title: cfg.window_title.str
		html5_canvas_name: cfg.window_title.str
		width: cfg.width
		height: cfg.height
		high_dpi: true
		fullscreen: cfg.fullscreen
	}
	if cfg.use_ortho {
	} else {
	}
	g.window = window
	return g
}

pub fn (gg &Context) run() {
	sapp.run(&gg.window)
}

pub fn (mut ctx Context) set_bg_color(c gx.Color) {
	ctx.clear_pass = gfx.create_clear_pass(f32(c.r) / 255.0, f32(c.g) / 255.0,
			f32(c.b) / 255.0, f32(c.a) / 255.0)
}

// TODO: Fix alpha
pub fn (ctx &Context) draw_rect(x, y, w, h f32, c gx.Color) {
	sgl.c4b(c.r, c.g, c.b, c.a)
	sgl.begin_quads()
	sgl.v2f(x * ctx.scale, y * ctx.scale)
	sgl.v2f((x + w) * ctx.scale, y * ctx.scale)
	sgl.v2f((x + w) * ctx.scale, (y + h) * ctx.scale)
	sgl.v2f(x * ctx.scale, (y + h) * ctx.scale)
	sgl.end()
}

pub fn (ctx &Context) draw_empty_rect(x, y, w, h f32, c gx.Color) {
	sgl.c4b(c.r, c.g, c.b, c.a)
	sgl.begin_line_strip()
	if ctx.scale == 1 {
		sgl.v2f(x, y)
		sgl.v2f(x + w, y)
		sgl.v2f(x + w, y + h)
		sgl.v2f(x, y + h)
		sgl.v2f(x, y)
	} else {
		sgl.v2f(x * ctx.scale, y * ctx.scale)
		sgl.v2f((x + w) * ctx.scale, y * ctx.scale)
		sgl.v2f((x + w) * ctx.scale, (y + h) * ctx.scale)
		sgl.v2f(x * ctx.scale, (y + h) * ctx.scale)
		sgl.v2f(x * ctx.scale, y * ctx.scale)
	}
	sgl.end()
}

pub fn (ctx &Context) draw_circle(x, y, r f32, c gx.Color) {
}

pub fn (gg &Context) begin() {
	if gg.render_text && gg.font_inited {
		gg.ft.flush()
	}
	sgl.defaults()
	sgl.matrix_mode_projection()
	sgl.ortho(0.0, f32(sapp.width()), f32(sapp.height()), 0.0, -1.0, 1.0)
}

pub fn (gg &Context) end() {
	gfx.begin_default_pass(gg.clear_pass, sapp.width(), sapp.height())
	sgl.draw()
	gfx.end_pass()
	gfx.commit()
	if gg.config.wait_events {
		// println('gg: waiting')
		wait_events()
	}
}

pub fn (ctx &Context) draw_line(x, y, x2, y2 f32, c gx.Color) {
	sgl.c4b(c.r, c.g, c.b, c.a)
	sgl.begin_line_strip()
	sgl.v2f(x * ctx.scale, y * ctx.scale)
	sgl.v2f(x2 * ctx.scale, y2 * ctx.scale)
	sgl.end()
}

pub fn (ctx &Context) draw_rounded_rect(x, y, width, height, radius f32, color gx.Color) {
}

pub fn (ctx &Context) draw_empty_rounded_rect(x, y, width, height, radius f32, border_color gx.Color) {
}

fn C.WaitMessage()

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
