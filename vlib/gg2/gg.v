// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module gg2

import (
	glm
	gx
	os
	sokol
	sokol.sapp
	sokol.sgl
	sokol.gfx
	sokol.sfons
)
const (
	default_font_size = 24
)

pub struct Config {
pub:
	width         int
	height        int
	use_ortho     bool
	retina        bool
	resizable     bool
	user_data     voidptr
	font_size     int
	font_path     string
	create_window bool
	// window_user_ptr voidptr
	window_title  string
	always_on_top bool
	scale         int
	frame_fn      fn(voidptr)
	bg_color      gx.Color
}

pub struct GG {
	scale      int // retina = 2 , normal = 1
pub mut:
	width      int
	height     int
	clear_pass C.sg_pass_action
	window     C.sapp_desc
	render_fn  fn()
	//////////// font fields
	fons &C.FONScontext
	font_normal int
}

// TODO remove globals
__global g_fons &C.FONScontext
__global g_font_normal int
__global g_font_path string

fn init_sokol_window() {
	desc := sg_desc{
		mtl_device: C.sapp_metal_get_device()
		mtl_renderpass_descriptor_cb: sapp_metal_get_renderpass_descriptor
		mtl_drawable_cb: sapp_metal_get_drawable
		d3d11_device: sapp_d3d11_get_device()
		d3d11_device_context: sapp_d3d11_get_device_context()
		d3d11_render_target_view_cb: sapp_d3d11_get_render_target_view
		d3d11_depth_stencil_view_cb: sapp_d3d11_get_depth_stencil_view
	}
	gfx.setup(&desc)
	sgl_desc := sgl_desc_t{}
	sgl.setup(&sgl_desc)
	g_fons = sfons.create(512, 512, 1)
	if g_font_path.len == 0 || !os.exists(g_font_path) {
		println('failed to load font "$g_font_path"')
		return
	}
	bytes := os.read_bytes(g_font_path) or {
		println('failed to load font "$g_font_path"')
		return
	}
	g_font_normal = C.fonsAddFontMem(g_fons, 'sans', bytes.data, bytes.len, false)
}

pub fn new_context(cfg Config) &GG {
	//C.printf('new_context() %p\n', cfg.user_data)
	window := sapp_desc{
		user_data: cfg.user_data
		init_userdata_cb: init_sokol_window
		frame_userdata_cb: cfg.frame_fn
		window_title: cfg.window_title.str
		html5_canvas_name: cfg.window_title.str
		width: cfg.width
		height: cfg.height
		high_dpi: true
	}
	g_font_path = cfg.font_path
	if cfg.use_ortho {}
	else {}
	return &GG{
		width: cfg.width
		height: cfg.height
		window: window
		clear_pass: gfx.create_clear_pass(f32(cfg.bg_color.r) / 255.0, f32(cfg.bg_color.g) / 255.0, f32(cfg.bg_color.b) / 255.0, 1.0)
		scale: 1 // scale
		fons:0
	}
}

pub fn (gg &GG) draw_text(x, y int, text string, cfg gx.TextCfg) {
	gg.fons.set_font(gg.font_normal)
	gg.fons.set_size(cfg.size)
	ascender := 0.0
	descender := 0.0
	lh := 0.0
	gg.fons.vert_metrics(&ascender, &descender, &lh)
	color:= C.sfons_rgba(cfg.color.r, cfg.color.g, cfg.color.b, 255)
	C.fonsSetColor(gg.fons, color)
	C.fonsDrawText(gg.fons, x, y, text.str, 0)
}

pub fn (ctx &GG) draw_text_def(x, y int, text string) {
	cfg := gx.TextCfg {
		color: gx.Black
		size: default_font_size
		align: gx.ALIGN_LEFT
	}
	ctx.draw_text(x, y, text, cfg)
}

pub fn (gg mut GG) init_font() {
	// TODO
	gg.fons =g_fons
	gg.font_normal=g_font_normal
}

pub fn (gg &GG) run() {
	sapp.run(&gg.window)
}

pub fn (ctx &GG) draw_rect(x, y, w, h f32, c gx.Color) {
	sgl.c4b(c.r, c.g, c.b, 128)
	sgl.begin_quads()
	sgl_v2f(x, y)
	sgl_v2f(x + w, y)
	sgl_v2f(x + w, y + h)
	sgl_v2f(x, y + h)
	sgl.end()
}

pub fn draw_rect(x, y, w, h f32, c gx.Color) {
	sgl.c4b(c.r, c.g, c.b, 128)
	sgl.begin_quads()
	sgl_v2f(x, y)
	sgl_v2f(x + w, y)
	sgl_v2f(x + w, y + h)
	sgl_v2f(x, y + h)
	sgl.end()
}

pub fn (gg &GG) draw_empty_rect(x, y, w, h f32, c gx.Color) {
	sgl.c4b(c.r, c.g, c.b, 128)
	sgl.begin_line_strip()
	sgl.v2f(x, y)
	sgl.v2f(x + w, y)
	sgl.v2f(x + w, y + h)
	sgl.v2f(x, y + h)
	sgl.v2f(x, y)
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

pub fn (gg &GG) begin() {
	sgl.defaults()
	sgl.matrix_mode_projection()
	sgl.ortho(0.0, f32(sapp.width()), f32(sapp.height()), 0.0, -1.0, 1.0)
}

pub fn (gg &GG) end() {
	gfx.begin_default_pass(gg.clear_pass, sapp.width(), sapp.height())
	sgl.draw()
	gfx.end_pass()
	gfx.commit()
	wait_events()
}

pub fn (ctx &GG) draw_line(x, y, x2, y2 f32, color gx.Color) {}

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
