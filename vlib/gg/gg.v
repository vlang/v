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

// import time
pub type FNCb = fn (x voidptr)

pub type FNEvent = fn (e &Event, x voidptr)

pub type FNFail = fn (msg string, x voidptr)

pub type FNKeyDown = fn (c KeyCode, m Modifier, x voidptr)

pub type FNMove = fn (x f32, y f32, z voidptr)

pub type FNChar = fn (c u32, x voidptr)

pub struct Event {
pub mut:
	frame_count        u64
	typ                sapp.EventType
	key_code           KeyCode
	char_code          u32
	key_repeat         bool
	modifiers          u32
	mouse_button       sapp.MouseButton
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

pub enum Modifier {
	shift = 1 //(1<<0)
	ctrl = 2 //(1<<1)
	alt = 4 //(1<<2)
	super = 8 //(1<<3)
}

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
	window_title      string
	borderless_window bool
	always_on_top     bool
	bg_color          gx.Color
	init_fn           FNCb      = voidptr(0)
	frame_fn          FNCb      = voidptr(0)
	native_frame_fn   FNCb      = voidptr(0)
	cleanup_fn        FNCb      = voidptr(0)
	fail_fn           FNFail    = voidptr(0)
	event_fn          FNEvent   = voidptr(0)
	keydown_fn        FNKeyDown = voidptr(0)
	// special case of event_fn
	char_fn FNChar = voidptr(0)
	// special case of event_fn
	move_fn FNMove = voidptr(0)
	// special case of event_fn
	click_fn FNMove = voidptr(0)
	// special case of event_fn
	// wait_events       bool // set this to true for UIs, to save power
	fullscreen   bool
	scale        f32 = 1.0
	sample_count int
	// vid needs this
	// init_text bool
	font_path             string
	custom_bold_font_path string
	ui_mode               bool // refreshes only on events to save CPU usage
	// font bytes for embedding
	font_bytes_normal []byte
	font_bytes_bold   []byte
	font_bytes_mono   []byte
	font_bytes_italic []byte
	native_rendering  bool // Cocoa on macOS/iOS, GDI+ on Windows
}

pub struct Context {
	render_text bool
mut:
	// a cache with all images created by the user. used for sokol image init and to save space
	// (so that the user can store image ids, not entire Image objects)
	image_cache   []Image
	needs_refresh bool = true
	ticks         int
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
	ft          &FT
	font_inited bool
	ui_mode     bool // do not redraw everything 60 times/second, but only when the user requests
	mbtn_mask   byte
}

pub struct Size {
pub:
	width  int
	height int
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
	if g.config.font_path != '' && exists {
		// t := time.ticks()
		g.ft = new_ft(
			font_path: g.config.font_path
			custom_bold_font_path: g.config.custom_bold_font_path
			scale: dpi_scale()
		) or { panic(err) }
		// println('FT took ${time.ticks()-t} ms')
		g.font_inited = true
	} else {
		if !exists {
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
				eprintln('font file "$g.config.font_path" does not exist, the system font was used instead.')
				g.ft = new_ft(
					font_path: sfont
					custom_bold_font_path: g.config.custom_bold_font_path
					scale: sapp.dpi_scale()
				) or { panic(err) }
				g.font_inited = true
			}
		}
	}
	//
	mut pipdesc := C.sg_pipeline_desc{}
	unsafe { C.memset(&pipdesc, 0, sizeof(pipdesc)) }
	pipdesc.blend.enabled = true
	pipdesc.blend.src_factor_rgb = gfx.BlendFactor(C.SG_BLENDFACTOR_SRC_ALPHA)
	pipdesc.blend.dst_factor_rgb = gfx.BlendFactor(C.SG_BLENDFACTOR_ONE_MINUS_SRC_ALPHA)
	g.timage_pip = sgl.make_pipeline(&pipdesc)
	//
	if g.config.init_fn != voidptr(0) {
		g.config.init_fn(g.config.user_data)
	}
	// Create images now that we can do that after sg is inited
	if g.native_rendering {
		return
	}
	for i in 0 .. g.image_cache.len {
		g.image_cache[i].init_sokol_image()
	}
}

fn gg_frame_fn(user_data voidptr) {
	mut ctx := unsafe { &Context(user_data) }
	if ctx.config.frame_fn == voidptr(0) {
		return
	}
	if ctx.native_rendering {
		// return
	}
	if ctx.ui_mode && !ctx.needs_refresh {
		// Draw 3 more frames after the "stop refresh" command
		ctx.ticks++
		if ctx.ticks > 3 {
			return
		}
	}
	ctx.config.frame_fn(ctx.config.user_data)
	ctx.needs_refresh = false
}

pub fn (mut ctx Context) refresh_ui() {
	ctx.needs_refresh = true
	ctx.ticks = 0
}

fn gg_event_fn(ce &C.sapp_event, user_data voidptr) {
	// e := unsafe { &sapp.Event(ce) }
	mut e := unsafe { &Event(ce) }
	mut g := unsafe { &Context(user_data) }
	if e.typ == .mouse_down {
		bitplace := int(e.mouse_button)
		g.mbtn_mask |= byte(1 << bitplace)
	}
	if e.typ == .mouse_up {
		bitplace := int(e.mouse_button)
		g.mbtn_mask &= ~(byte(1 << bitplace))
	}
	if e.typ == .mouse_move && e.mouse_button == .invalid {
		if g.mbtn_mask & 0x01 > 0 {
			e.mouse_button = .left
		}
		if g.mbtn_mask & 0x02 > 0 {
			e.mouse_button = .right
		}
		if g.mbtn_mask & 0x04 > 0 {
			e.mouse_button = .middle
		}
	}
	if g.config.event_fn != voidptr(0) {
		g.config.event_fn(e, g.config.user_data)
	}
	match e.typ {
		.key_down {
			if g.config.keydown_fn != voidptr(0) {
				kdfn := g.config.keydown_fn
				kdfn(e.key_code, Modifier(e.modifiers), g.config.user_data)
			}
		}
		.char {
			if g.config.char_fn != voidptr(0) {
				cfn := g.config.char_fn
				cfn(e.char_code, g.config.user_data)
			}
		}
		.mouse_move {
			if g.config.move_fn != voidptr(0) {
				cfn := g.config.move_fn
				cfn(e.mouse_x / g.scale, e.mouse_y / g.scale, g.config.user_data)
			}
		}
		.mouse_down {
			if g.config.click_fn != voidptr(0) {
				cfn := g.config.click_fn
				cfn(e.mouse_x / g.scale, e.mouse_y / g.scale, g.config.user_data)
			}
		}
		else {}
	}
}

fn gg_cleanup_fn(user_data voidptr) {
	mut g := unsafe { &Context(user_data) }
	if g.config.cleanup_fn != voidptr(0) {
		g.config.cleanup_fn(g.config.user_data)
	}
}

fn gg_fail_fn(msg charptr, user_data voidptr) {
	mut g := unsafe { &Context(user_data) }
	vmsg := unsafe { tos3(msg) }
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
		render_text: cfg.font_path != '' || cfg.font_bytes_normal.len > 0
		ft: 0
		ui_mode: cfg.ui_mode
		native_rendering: cfg.native_rendering
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
		sample_count: cfg.sample_count
		high_dpi: true
		fullscreen: cfg.fullscreen
		native_render: cfg.native_rendering
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
	ctx.clear_pass = gfx.create_clear_pass(f32(c.r) / 255.0, f32(c.g) / 255.0, f32(c.b) / 255.0,
		f32(c.a) / 255.0)
}

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

[inline]
pub fn (ctx &Context) draw_square(x f32, y f32, s f32, c gx.Color) {
	ctx.draw_rect(x, y, s, s, c)
}

[inline]
pub fn (ctx &Context) set_pixel(x f32, y f32, c gx.Color) {
	ctx.draw_square(x, y, 1, c)
}

pub fn (ctx &Context) draw_triangle(x f32, y f32, x2 f32, y2 f32, x3 f32, y3 f32, c gx.Color) {
	if c.a != 255 {
		sgl.load_pipeline(ctx.timage_pip)
	}
	sgl.c4b(c.r, c.g, c.b, c.a)
	sgl.begin_quads()
	sgl.v2f(x * ctx.scale, y * ctx.scale)
	sgl.v2f(x2 * ctx.scale, y2 * ctx.scale)
	sgl.v2f(x3 * ctx.scale, y3 * ctx.scale)
	sgl.end()
}

pub fn (ctx &Context) draw_empty_rect(x f32, y f32, w f32, h f32, c gx.Color) {
	if c.a != 255 {
		sgl.load_pipeline(ctx.timage_pip)
	}
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

[inline]
pub fn (ctx &Context) draw_empty_square(x f32, y f32, s f32, c gx.Color) {
	ctx.draw_empty_rect(x, y, s, s, c)
}

pub fn (ctx &Context) draw_circle_line(x f32, y f32, r int, segments int, c gx.Color) {
	if c.a != 255 {
		sgl.load_pipeline(ctx.timage_pip)
	}
	sgl.c4b(c.r, c.g, c.b, c.a)
	mut theta := f32(0)
	mut xx := f32(0)
	mut yy := f32(0)
	sgl.begin_line_strip()
	for i := 0; i < segments + 1; i++ {
		theta = 2.0 * f32(math.pi) * f32(i) / f32(segments)
		xx = r * math.cosf(theta)
		yy = r * math.sinf(theta)
		sgl.v2f(xx + x, yy + y)
	}
	sgl.end()
}

pub fn (ctx &Context) draw_circle(x f32, y f32, r f32, c gx.Color) {
	$if macos {
		if ctx.native_rendering {
			C.darwin_draw_circle(x - r + 1, ctx.height - (y + r + 3), r, c)
			return
		}
	}
	if ctx.scale == 1 {
		ctx.draw_circle_with_segments(x, y, r, 10, c)
	} else {
		ctx.draw_circle_with_segments(x * f32(ctx.scale), y * f32(ctx.scale), r * ctx.scale,
			10, c)
	}
}

pub fn (ctx &Context) draw_circle_with_segments(x f32, y f32, r f32, segments int, c gx.Color) {
	if c.a != 255 {
		sgl.load_pipeline(ctx.timage_pip)
	}
	sgl.c4b(c.r, c.g, c.b, c.a)
	mut theta := f32(0)
	mut xx := f32(0)
	mut yy := f32(0)
	sgl.begin_triangle_strip()
	for i := 0; i < segments + 1; i++ {
		theta = 2.0 * f32(math.pi) * f32(i) / f32(segments)
		xx = r * math.cosf(theta)
		yy = r * math.sinf(theta)
		sgl.v2f(xx + x, yy + y)
		sgl.v2f(x, y)
	}
	sgl.end()
}

pub fn (ctx &Context) draw_arc_line(x f32, y f32, r int, start_angle f32, arc_angle f32, segments int, c gx.Color) {
	if c.a != 255 {
		sgl.load_pipeline(ctx.timage_pip)
	}
	sgl.c4b(c.r, c.g, c.b, c.a)
	theta := f32(arc_angle / f32(segments))
	tan_factor := math.tanf(theta)
	rad_factor := math.cosf(theta)
	mut xx := f32(r * math.cosf(start_angle))
	mut yy := f32(r * math.sinf(start_angle))
	sgl.begin_line_strip()
	for i := 0; i < segments + 1; i++ {
		sgl.v2f(xx + x, yy + y)
		tx := -yy
		ty := xx
		xx += tx * tan_factor
		yy += ty * tan_factor
		xx *= rad_factor
		yy *= rad_factor
	}
	sgl.end()
}

pub fn (ctx &Context) draw_arc(x f32, y f32, r int, start_angle f32, arc_angle f32, segments int, c gx.Color) {
	if c.a != 255 {
		sgl.load_pipeline(ctx.timage_pip)
	}
	sgl.c4b(c.r, c.g, c.b, c.a)
	theta := f32(arc_angle / f32(segments))
	tan_factor := math.tanf(theta)
	rad_factor := math.cosf(theta)
	mut xx := f32(r * math.cosf(start_angle))
	mut yy := f32(r * math.sinf(start_angle))
	sgl.begin_triangle_strip()
	for i := 0; i < segments + 1; i++ {
		sgl.v2f(xx + x, yy + y)
		sgl.v2f(x, y)
		tx := -yy
		ty := xx
		xx += tx * tan_factor
		yy += ty * tan_factor
		xx *= rad_factor
		yy *= rad_factor
	}
	sgl.end()
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
	/*
	if gg.config.wait_events {
		// println('gg: waiting')
		wait_events()
	}
	*/
}

fn abs(a f32) f32 {
	if a >= 0 {
		return a
	}
	return -a
}

pub fn (mut ctx Context) resize(width int, height int) {
	ctx.width = width
	ctx.height = height
}

pub fn (ctx &Context) draw_line(x f32, y f32, x2 f32, y2 f32, c gx.Color) {
	if c.a != 255 {
		sgl.load_pipeline(ctx.timage_pip)
	}
	if ctx.scale > 1 {
		// Make the line more clear on hi dpi screens: draw a rectangle
		mut width := abs(x2 - x)
		mut height := abs(y2 - y)
		if width == 0 {
			width = 1
		} else if height == 0 {
			height = 1
		}
		ctx.draw_rect(x, y, width, height, c)
		return
	}
	sgl.c4b(c.r, c.g, c.b, c.a)
	sgl.begin_line_strip()
	sgl.v2f(x * ctx.scale, y * ctx.scale)
	sgl.v2f(x2 * ctx.scale, y2 * ctx.scale)
	sgl.end()
}

pub fn (ctx &Context) draw_rounded_rect(x f32, y f32, w f32, h f32, radius f32, color gx.Color) {
	sgl.c4b(color.r, color.g, color.b, color.a)
	sgl.begin_triangle_strip()
	mut theta := f32(0)
	mut xx := f32(0)
	mut yy := f32(0)
	r := radius * f32(ctx.scale)
	nx := x * f32(ctx.scale)
	ny := y * f32(ctx.scale)
	width := w * f32(ctx.scale)
	height := h * f32(ctx.scale)
	segments := 2 * math.pi * r
	segdiv := segments / 4
	rb := 0
	lb := int(rb + segdiv)
	lt := int(lb + segdiv)
	rt := int(lt + segdiv)
	// left top
	lx := nx + r
	ly := ny + r
	for i in lt .. rt {
		theta = 2 * f32(math.pi) * f32(i) / segments
		xx = r * math.cosf(theta)
		yy = r * math.sinf(theta)
		sgl.v2f(xx + lx, yy + ly)
		sgl.v2f(lx, ly)
	}
	// right top
	mut rx := nx + 2 * width - r
	mut ry := ny + r
	for i in rt .. int(segments) {
		theta = 2 * f32(math.pi) * f32(i) / segments
		xx = r * math.cosf(theta)
		yy = r * math.sinf(theta)
		sgl.v2f(xx + rx, yy + ry)
		sgl.v2f(rx, ry)
	}
	// right bottom
	mut rbx := rx
	mut rby := ny + 2 * height - r
	for i in rb .. lb {
		theta = 2 * f32(math.pi) * f32(i) / segments
		xx = r * math.cosf(theta)
		yy = r * math.sinf(theta)
		sgl.v2f(xx + rbx, yy + rby)
		sgl.v2f(rbx, rby)
	}
	// left bottom
	mut lbx := lx
	mut lby := ny + 2 * height - r
	for i in lb .. lt {
		theta = 2 * f32(math.pi) * f32(i) / segments
		xx = r * math.cosf(theta)
		yy = r * math.sinf(theta)
		sgl.v2f(xx + lbx, yy + lby)
		sgl.v2f(lbx, lby)
	}
	sgl.v2f(lx + xx, ly)
	sgl.v2f(lx, ly)
	sgl.end()
	sgl.begin_quads()
	sgl.v2f(lx, ly)
	sgl.v2f(rx, ry)
	sgl.v2f(rbx, rby)
	sgl.v2f(lbx, lby)
	sgl.end()
}

pub fn (ctx &Context) draw_empty_rounded_rect(x f32, y f32, w f32, h f32, radius f32, border_color gx.Color) {
	mut theta := f32(0)
	mut xx := f32(0)
	mut yy := f32(0)
	r := radius * f32(ctx.scale)
	nx := x * f32(ctx.scale)
	ny := y * f32(ctx.scale)
	width := w * f32(ctx.scale)
	height := h * f32(ctx.scale)
	segments := 2 * math.pi * r
	segdiv := segments / 4
	rb := 0
	lb := int(rb + segdiv)
	lt := int(lb + segdiv)
	rt := int(lt + segdiv)
	sgl.c4b(border_color.r, border_color.g, border_color.b, border_color.a)
	sgl.begin_line_strip()
	// left top
	lx := nx + r
	ly := ny + r
	for i in lt .. rt {
		theta = 2 * f32(math.pi) * f32(i) / segments
		xx = r * math.cosf(theta)
		yy = r * math.sinf(theta)
		sgl.v2f(xx + lx, yy + ly)
	}
	// right top
	mut rx := nx + 2 * width - r
	mut ry := ny + r
	for i in rt .. int(segments) {
		theta = 2 * f32(math.pi) * f32(i) / segments
		xx = r * math.cosf(theta)
		yy = r * math.sinf(theta)
		sgl.v2f(xx + rx, yy + ry)
	}
	// right bottom
	mut rbx := rx
	mut rby := ny + 2 * height - r
	for i in rb .. lb {
		theta = 2 * f32(math.pi) * f32(i) / segments
		xx = r * math.cosf(theta)
		yy = r * math.sinf(theta)
		sgl.v2f(xx + rbx, yy + rby)
	}
	// left bottom
	mut lbx := lx
	mut lby := ny + 2 * height - r
	for i in lb .. lt {
		theta = 2 * f32(math.pi) * f32(i) / segments
		xx = r * math.cosf(theta)
		yy = r * math.sinf(theta)
		sgl.v2f(xx + lbx, yy + lby)
	}
	sgl.v2f(lx + xx, ly)
	sgl.end()
}

// draw_convex_poly draws a convex polygon, given an array of points, and a color.
// Note that the points must be given in clockwise order.
pub fn (ctx &Context) draw_convex_poly(points []f32, c gx.Color) {
	assert points.len % 2 == 0
	len := points.len / 2
	assert len >= 3

	if c.a != 255 {
		sgl.load_pipeline(ctx.timage_pip)
	}
	sgl.c4b(c.r, c.g, c.b, c.a)

	sgl.begin_triangle_strip()
	x0 := points[0]
	y0 := points[1]
	for i in 1 .. (len / 2 + 1) {
		sgl.v2f(x0, y0)
		sgl.v2f(points[i * 4 - 2], points[i * 4 - 1])
		sgl.v2f(points[i * 4], points[i * 4 + 1])
	}

	if len % 2 == 0 {
		sgl.v2f(points[2 * len - 2], points[2 * len - 1])
	}
	sgl.end()
}

// draw_empty_poly - draws the borders of a polygon, given an array of points, and a color.
// Note that the points must be given in clockwise order.
pub fn (ctx &Context) draw_empty_poly(points []f32, c gx.Color) {
	assert points.len % 2 == 0
	len := points.len / 2
	assert len >= 3

	if c.a != 255 {
		sgl.load_pipeline(ctx.timage_pip)
	}
	sgl.c4b(c.r, c.g, c.b, c.a)

	sgl.begin_line_strip()
	for i in 0 .. len {
		sgl.v2f(points[2 * i], points[2 * i + 1])
	}
	sgl.v2f(points[0], points[1])
	sgl.end()
}

pub fn screen_size() Size {
	$if macos {
		return C.gg_get_screen_size()
	}
	// TODO windows, linux, etc
	return Size{}
}

// window_size returns the `Size` of the active window
pub fn window_size() Size {
	s := dpi_scale()
	return Size{int(sapp.width() / s), int(sapp.height() / s)}
}

// window_size_real_pixels returns the `Size` of the active window without scale
pub fn window_size_real_pixels() Size {
	return Size{sapp.width(), sapp.height()}
}

pub fn dpi_scale() f32 {
	mut s := sapp.dpi_scale()
	$if android {
		s *= android_dpi_scale()
	}
	// NB: on older X11, `Xft.dpi` from ~/.Xresources, that sokol uses,
	// may not be set which leads to sapp.dpi_scale reporting incorrectly 0.0
	if s < 0.1 {
		s = 1.
	}
	return s
}

pub fn high_dpi() bool {
	return C.sapp_high_dpi()
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
