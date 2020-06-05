// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module gg

import gx
import os
import sokol
import sokol.sapp
import sokol.sgl
import sokol.gfx
//import gg.ft

type FNvoidptr1 fn(voidptr)
type FNvoidptr2 fn(voidptr,voidptr)
type FNFail fn(string,voidptr)

pub struct Config {
pub:
	width         int
	height        int
	use_ortho     bool
	retina        bool
	resizable     bool
	user_data     voidptr
	font_size     int
	create_window bool
	// window_user_ptr voidptr
	window_title  string
	borderless_window bool
	always_on_top bool
	scale         f32  = 1.0
	bg_color      gx.Color
	init_fn       FNvoidptr1 = voidptr(0)
	frame_fn      FNvoidptr1 = voidptr(0)
	event_fn      FNvoidptr2 = voidptr(0)
	cleanup_fn    FNvoidptr1 = voidptr(0)
	fail_fn       FNFail = voidptr(0)
	wait_events   bool // set this to true for UIs, to save power
	font_path string
	fullscreen bool
}

pub struct Context {
	scale      f32 = 1.0// retina = 2 , normal = 1
pub mut:
	width      int
	height     int
	clear_pass C.sg_pass_action
	window     C.sapp_desc
	config     Config
}

pub struct Size { pub: width int height int }

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
	if g.config.init_fn != voidptr(0) {
		g.config.init_fn( g.config.user_data )
	}
}

fn gg_frame_fn(user_data voidptr) {
	mut g := &Context(user_data)
	if g.config.frame_fn != voidptr(0) {
		g.config.frame_fn( g.config.user_data )
	}
}

fn gg_event_fn(e &C.sapp_event, user_data voidptr){
	mut g := &Context(user_data)
	if g.config.event_fn != voidptr(0) {
		g.config.event_fn(e, g.config.user_data)
	}
}

fn gg_cleanup_fn(user_data voidptr){
	mut g := &Context(user_data)
	if g.config.cleanup_fn != voidptr(0) {
		g.config.cleanup_fn(g.config.user_data)
	}
}

fn gg_fail_fn(msg charptr, user_data voidptr){
	mut g := &Context(user_data)
	vmsg := tos3(msg)
	if g.config.fail_fn != voidptr(0) {
		g.config.fail_fn(vmsg, g.config.user_data)
	}else{
		eprintln('gg error: $vmsg')
	}
}

//

pub fn new_context(cfg Config) &Context{
	mut g := &Context{
		width: cfg.width
		height: cfg.height
		clear_pass: gfx.create_clear_pass( f32(cfg.bg_color.r) / 255.0, f32(cfg.bg_color.g) / 255.0,
f32(cfg.bg_color.b) / 255.0, 1.0)
		scale: cfg.scale //sapp.dpi_scale()// cfg.scale
		config: cfg
	}

	//C.printf('new_context() %p\n', cfg.user_data)
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
		high_dpi: cfg.scale > 1
		fullscreen: cfg.fullscreen
	}
	//b := sapp.high_dpi()
	//println('scale=$g.scale high_dpi=$b')
	if cfg.use_ortho {}
	else {}
	g.window = window
	return g
}

pub fn (gg &Context) run() {
	sapp.run(&gg.window)
}

pub fn (ctx &Context) draw_rect(x, y, w, h f32, c gx.Color) {
	sgl.c4b(c.r, c.g, c.b, 128)
	sgl.begin_quads()
	sgl.v2f(x * ctx.scale, y * ctx.scale)
	sgl.v2f((x + w) * ctx.scale, y * ctx.scale)
	sgl.v2f((x + w) * ctx.scale, (y + h) * ctx.scale)
	sgl.v2f(x * ctx.scale, (y + h) * ctx.scale)
	sgl.end()
}

pub fn (ctx &Context) draw_empty_rect(x, y, w, h f32, c gx.Color) {
	sgl.c4b(c.r, c.g, c.b, 128)
	sgl.begin_line_strip()
	if ctx.scale == 1 {
		sgl.v2f(x, y)
		sgl.v2f(x + w, y)
		sgl.v2f(x + w, y + h)
		sgl.v2f(x, y + h)
		sgl.v2f(x, y)
	}
	else {
		sgl.v2f(x * ctx.scale, y * ctx.scale)
		sgl.v2f((x + w) * ctx.scale, y * ctx.scale)
		sgl.v2f((x + w) * ctx.scale, (y + h) * ctx.scale)
		sgl.v2f(x * ctx.scale, (y + h) * ctx.scale)
		sgl.v2f(x*ctx.scale, y*ctx.scale)
	}
	sgl.end()
}

pub fn create_image(file string) u32 {
	// println('gg create image "$file"')
	if !os.exists(file) {
		println('gg create image no such file "$file"')
		return u32(0)
	}
	// img := stbi.load(file)
	// img.free()
	return 0 // texture
}

pub fn create_image_from_memory(buf byteptr) u32 {
	// texture := gl.gen_texture()
	// img := stbi.load_from_memory(buf)
	// img.free()
	return 0 // texture
}

pub fn (gg &Context) begin() {
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
		//println('gg: waiting')
		wait_events()
	}
}

pub fn (ctx &Context) draw_line(x, y, x2, y2 f32, color gx.Color) {}

fn C.WaitMessage()

pub fn wait_events() {
	unsafe {
		$if macos  {
		# NSEvent *event = [NSApp nextEventMatchingMask:NSEventMaskAny
		# untilDate:[NSDate distantFuture]
		# inMode:NSDefaultRunLoopMode
		# dequeue:YES];
		# [NSApp sendEvent:event];
		}
		$if windows {
			C.WaitMessage()
		}
	}
}
