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
	init_fn           FNCb   = voidptr(0)
	frame_fn          FNCb   = voidptr(0)
	native_frame_fn   FNCb   = voidptr(0)
	cleanup_fn        FNCb   = voidptr(0)
	fail_fn           FNFail = voidptr(0)
	//
	event_fn FNEvent = voidptr(0)
	quit_fn  FNEvent = voidptr(0)
	//
	keydown_fn FNKeyDown = voidptr(0)
	keyup_fn   FNKeyUp   = voidptr(0)
	char_fn    FNChar    = voidptr(0)
	//
	move_fn    FNMove    = voidptr(0)
	click_fn   FNClick   = voidptr(0)
	unclick_fn FNUnClick = voidptr(0)
	leave_fn   FNEvent   = voidptr(0)
	enter_fn   FNEvent   = voidptr(0)
	resized_fn FNEvent   = voidptr(0)
	scroll_fn  FNEvent   = voidptr(0)
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
	font_bytes_normal []byte
	font_bytes_bold   []byte
	font_bytes_mono   []byte
	font_bytes_italic []byte
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
[deprecated: 'use draw_rect_filled() instead']
pub fn (ctx &Context) draw_rect(x f32, y f32, w f32, h f32, c gx.Color) {
	ctx.draw_rect_filled(x, y, w, h, c)
}

pub fn (ctx &Context) draw_rect_filled(x f32, y f32, w f32, h f32, c gx.Color) {
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

fn gg_frame_fn(user_data voidptr) {
	mut ctx := unsafe { &Context(user_data) }
	ctx.frame++
	if ctx.config.frame_fn == voidptr(0) {
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

pub fn (mut ctx Context) refresh_ui() {
	ctx.needs_refresh = true
	ctx.ticks = 0
}

fn gg_event_fn(ce voidptr, user_data voidptr) {
	// e := unsafe { &sapp.Event(ce) }
	mut e := unsafe { &Event(ce) }
	mut g := unsafe { &Context(user_data) }
	if g.ui_mode {
		g.refresh_ui()
	}
	if e.typ == .mouse_down {
		bitplace := int(e.mouse_button)
		g.mbtn_mask |= byte(1 << bitplace)
		g.mouse_buttons = MouseButtons(g.mbtn_mask)
	}
	if e.typ == .mouse_up {
		bitplace := int(e.mouse_button)
		g.mbtn_mask &= ~(byte(1 << bitplace))
		g.mouse_buttons = MouseButtons(g.mbtn_mask)
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
	g.mouse_pos_x = int(e.mouse_x / g.scale)
	g.mouse_pos_y = int(e.mouse_y / g.scale)
	g.mouse_dx = int(e.mouse_dx / g.scale)
	g.mouse_dy = int(e.mouse_dy / g.scale)
	g.scroll_x = int(e.scroll_x / g.scale)
	g.scroll_y = int(e.scroll_y / g.scale)
	g.key_modifiers = Modifier(e.modifiers)
	g.key_repeat = e.key_repeat
	if e.typ in [.key_down, .key_up] {
		key_idx := int(e.key_code) % key_code_max
		prev := g.pressed_keys[key_idx]
		next := e.typ == .key_down
		g.pressed_keys[key_idx] = next
		g.pressed_keys_edge[key_idx] = prev != next
	}
	if g.config.event_fn != voidptr(0) {
		g.config.event_fn(e, g.config.user_data)
	}
	match e.typ {
		.mouse_move {
			if g.config.move_fn != voidptr(0) {
				g.config.move_fn(e.mouse_x / g.scale, e.mouse_y / g.scale, g.config.user_data)
			}
		}
		.mouse_down {
			if g.config.click_fn != voidptr(0) {
				g.config.click_fn(e.mouse_x / g.scale, e.mouse_y / g.scale, e.mouse_button,
					g.config.user_data)
			}
		}
		.mouse_up {
			if g.config.unclick_fn != voidptr(0) {
				g.config.unclick_fn(e.mouse_x / g.scale, e.mouse_y / g.scale, e.mouse_button,
					g.config.user_data)
			}
		}
		.mouse_leave {
			if g.config.leave_fn != voidptr(0) {
				g.config.leave_fn(e, g.config.user_data)
			}
		}
		.mouse_enter {
			if g.config.enter_fn != voidptr(0) {
				g.config.enter_fn(e, g.config.user_data)
			}
		}
		.mouse_scroll {
			if g.config.scroll_fn != voidptr(0) {
				g.config.scroll_fn(e, g.config.user_data)
			}
		}
		.key_down {
			if g.config.keydown_fn != voidptr(0) {
				g.config.keydown_fn(e.key_code, Modifier(e.modifiers), g.config.user_data)
			}
		}
		.key_up {
			if g.config.keyup_fn != voidptr(0) {
				g.config.keyup_fn(e.key_code, Modifier(e.modifiers), g.config.user_data)
			}
		}
		.char {
			if g.config.char_fn != voidptr(0) {
				g.config.char_fn(e.char_code, g.config.user_data)
			}
		}
		.resized {
			if g.config.resized_fn != voidptr(0) {
				g.config.resized_fn(e, g.config.user_data)
			}
		}
		.quit_requested {
			if g.config.quit_fn != voidptr(0) {
				g.config.quit_fn(e, g.config.user_data)
			}
		}
		else {
			// dump(e)
		}
	}
}

fn gg_cleanup_fn(user_data voidptr) {
	mut g := unsafe { &Context(user_data) }
	if g.config.cleanup_fn != voidptr(0) {
		g.config.cleanup_fn(g.config.user_data)
	}
	gfx.shutdown()
}

fn gg_fail_fn(msg &char, user_data voidptr) {
	mut g := unsafe { &Context(user_data) }
	vmsg := unsafe { tos3(msg) }
	if g.config.fail_fn != voidptr(0) {
		g.config.fail_fn(vmsg, g.config.user_data)
	} else {
		eprintln('gg error: $vmsg')
	}
}

pub fn (ctx &Context) run() {
	sapp.run(&ctx.window)
}

// Prepares the context for drawing
pub fn (gg &Context) begin() {
	if gg.render_text && gg.font_inited {
		gg.ft.flush()
	}
	sgl.defaults()
	sgl.matrix_mode_projection()
	sgl.ortho(0.0, f32(sapp.width()), f32(sapp.height()), 0.0, -1.0, 1.0)
}

// Finishes drawing for the context
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

// quit closes the context window and exits the event loop for it
pub fn (ctx &Context) quit() {
	sapp.request_quit() // does not require ctx right now, but sokol multi-window might in the future
}

pub fn (mut ctx Context) set_bg_color(c gx.Color) {
	ctx.clear_pass = gfx.create_clear_pass(f32(c.r) / 255.0, f32(c.g) / 255.0, f32(c.b) / 255.0,
		f32(c.a) / 255.0)
}

// Sets a pixel
[deprecated: 'use draw_pixel() instead']
pub fn (ctx &Context) set_pixel(x f32, y f32, c gx.Color) {
	ctx.draw_pixel(x, y, c)
}

[inline]
pub fn (ctx &Context) draw_pixel(x f32, y f32, c gx.Color) {
	if c.a != 255 {
		sgl.load_pipeline(ctx.timage_pip)
	}

	sgl.c4b(c.r, c.g, c.b, c.a)
	sgl.begin_points()
	sgl.v2f(x * ctx.scale, y * ctx.scale)
	sgl.end()
}

[deprecated: 'use draw_pixels() instead']
pub fn (ctx &Context) set_pixels(points []f32, c gx.Color) {
	ctx.draw_pixels(points, c)
}

// Sets pixels from an array of points [x, y, x2, y2, etc...]
[direct_array_access; inline]
pub fn (ctx &Context) draw_pixels(points []f32, c gx.Color) {
	assert points.len % 2 == 0
	len := points.len / 2

	if c.a != 255 {
		sgl.load_pipeline(ctx.timage_pip)
	}

	sgl.c4b(c.r, c.g, c.b, c.a)
	sgl.begin_points()
	for i in 0 .. len {
		x, y := points[i * 2], points[i * 2 + 1]
		sgl.v2f(x * ctx.scale, y * ctx.scale)
	}
	sgl.end()
}

// Draws a filled triangle
[deprecated: 'use draw_triangle_filled() instead']
pub fn (ctx &Context) draw_triangle(x f32, y f32, x2 f32, y2 f32, x3 f32, y3 f32, c gx.Color) {
	ctx.draw_triangle_filled(x, y, x2, y2, x3, y3, c)
}

pub fn (ctx &Context) draw_triangle_filled(x f32, y f32, x2 f32, y2 f32, x3 f32, y3 f32, c gx.Color) {
	if c.a != 255 {
		sgl.load_pipeline(ctx.timage_pip)
	}
	sgl.c4b(c.r, c.g, c.b, c.a)
	sgl.begin_triangles()
	sgl.v2f(x * ctx.scale, y * ctx.scale)
	sgl.v2f(x2 * ctx.scale, y2 * ctx.scale)
	sgl.v2f(x3 * ctx.scale, y3 * ctx.scale)
	sgl.end()
}

// Draws the outline of a triangle
[deprecated: 'use draw_triangle_empty() instead']
pub fn (ctx &Context) draw_empty_triangle(x f32, y f32, x2 f32, y2 f32, x3 f32, y3 f32, c gx.Color) {
	ctx.draw_triangle_empty(x, y, x2, y2, x3, y3, c)
}

pub fn (ctx &Context) draw_triangle_empty(x f32, y f32, x2 f32, y2 f32, x3 f32, y3 f32, c gx.Color) {
	if c.a != 255 {
		sgl.load_pipeline(ctx.timage_pip)
	}

	sgl.c4b(c.r, c.g, c.b, c.a)
	sgl.begin_line_strip()
	sgl.v2f(x * ctx.scale, y * ctx.scale)
	sgl.v2f(x2 * ctx.scale, y2 * ctx.scale)
	sgl.v2f(x3 * ctx.scale, y3 * ctx.scale)
	sgl.v2f(x * ctx.scale, y * ctx.scale)
	sgl.end()
}

// Draws a filled square
[deprecated: 'use draw_square_filled() instead']
pub fn (ctx &Context) draw_square(x f32, y f32, s f32, c gx.Color) {
	ctx.draw_square_filled(x, y, s, c)
}

[inline]
pub fn (ctx &Context) draw_square_filled(x f32, y f32, s f32, c gx.Color) {
	ctx.draw_rect(x, y, s, s, c)
}

// Draws the outline of a square
[deprecated: 'use draw_square_empty() instead']
pub fn (ctx &Context) draw_empty_square(x f32, y f32, s f32, c gx.Color) {
	ctx.draw_square_empty(x, y, s, c)
}

[inline]
pub fn (ctx &Context) draw_square_empty(x f32, y f32, s f32, c gx.Color) {
	ctx.draw_rect_empty(x, y, s, s, c)
}

// Draws the outline of a rectangle
[deprecated: 'use draw_rect_empty() instead']
pub fn (ctx &Context) draw_empty_rect(x f32, y f32, w f32, h f32, c gx.Color) {
	ctx.draw_rect_empty(x, y, w, h, c)
}

pub fn (ctx &Context) draw_rect_empty(x f32, y f32, w f32, h f32, c gx.Color) {
	if c.a != 255 {
		sgl.load_pipeline(ctx.timage_pip)
	}
	sgl.c4b(c.r, c.g, c.b, c.a)
	sgl.begin_line_strip()
	sgl.v2f(x * ctx.scale, y * ctx.scale)
	sgl.v2f((x + w) * ctx.scale, y * ctx.scale)
	sgl.v2f((x + w) * ctx.scale, (y + h) * ctx.scale)
	sgl.v2f(x * ctx.scale, (y + h) * ctx.scale)
	sgl.v2f(x * ctx.scale, (y - 1) * ctx.scale)
	sgl.end()
}

// Draws a filled circle
[deprecated: 'use draw_circle_filled() instead']
pub fn (ctx &Context) draw_circle(x f32, y f32, r f32, c gx.Color) {
	ctx.draw_circle_filled(x, y, r, c)
}

pub fn (ctx &Context) draw_circle_filled(x f32, y f32, r f32, c gx.Color) {
	ctx.draw_circle_with_segments(x, y, r, 10, c)
}

// Draws a circle with a specific number of segments (affects how smooth/round the circle is)
pub fn (ctx &Context) draw_circle_with_segments(x f32, y f32, r f32, segments int, c gx.Color) {
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
	sgl.begin_triangle_strip()
	for i := 0; i < segments + 1; i++ {
		theta = 2.0 * f32(math.pi) * f32(i) / f32(segments)
		xx = nr * math.cosf(theta)
		yy = nr * math.sinf(theta)
		sgl.v2f(xx + nx, yy + ny)
		sgl.v2f(nx, ny)
	}
	sgl.end()
}

// Draws a filled circle slice/pie.
[deprecated: 'use draw_slice_filled() instead']
pub fn (ctx &Context) draw_slice(x f32, y f32, r f32, start_angle f32, arc_angle f32, segments int, c gx.Color) {
	ctx.draw_slice_filled(x, y, r, start_angle, arc_angle, segments, c)
}

pub fn (ctx &Context) draw_slice_filled(x f32, y f32, r f32, start_angle f32, arc_angle f32, segments int, c gx.Color) {
	if c.a != 255 {
		sgl.load_pipeline(ctx.timage_pip)
	}
	sgl.c4b(c.r, c.g, c.b, c.a)
	nx := x * ctx.scale
	ny := y * ctx.scale
	theta := f32(arc_angle / f32(segments))
	tan_factor := math.tanf(theta)
	rad_factor := math.cosf(theta)
	mut xx := r * math.cosf(start_angle)
	mut yy := r * math.sinf(start_angle)
	sgl.begin_triangle_strip()
	for i := 0; i < segments + 1; i++ {
		sgl.v2f(xx + nx, yy + ny)
		sgl.v2f(nx, ny)
		tx := -yy
		ty := xx
		xx += tx * tan_factor
		yy += ty * tan_factor
		xx *= rad_factor
		yy *= rad_factor
	}
	sgl.end()
}

// Draws the outline of a circle slice/pie.
[deprecated: 'use draw_slice_empty() instead']
pub fn (ctx &Context) draw_empty_slice(x f32, y f32, r f32, start_angle f32, arc_angle f32, segments int, c gx.Color) {
	ctx.draw_slice_empty(x, y, r, start_angle, arc_angle, segments, c)
}

// TODO: Add inner angle to empty shape
pub fn (ctx &Context) draw_slice_empty(x f32, y f32, r f32, start_angle f32, arc_angle f32, segments int, c gx.Color) {
	if c.a != 255 {
		sgl.load_pipeline(ctx.timage_pip)
	}
	sgl.c4b(c.r, c.g, c.b, c.a)
	theta := f32(arc_angle / f32(segments))
	tan_factor := math.tanf(theta)
	rad_factor := math.cosf(theta)
	nx := x * ctx.scale
	ny := y * ctx.scale
	mut xx := r * math.cosf(start_angle)
	mut yy := r * math.sinf(start_angle)
	sgl.begin_line_strip()
	for i := 0; i < segments + 1; i++ {
		sgl.v2f(xx + nx, yy + ny)
		tx := -yy
		ty := xx
		xx += tx * tan_factor
		yy += ty * tan_factor
		xx *= rad_factor
		yy *= rad_factor
	}
	sgl.end()
}

// Resize the context's Window
pub fn (mut ctx Context) resize(width int, height int) {
	ctx.width = width
	ctx.height = height
	// C.sapp_resize_window(width, height)
}

// Draws a line between the points provided
pub fn (ctx &Context) draw_line(x f32, y f32, x2 f32, y2 f32, c gx.Color) {
	$if macos {
		if ctx.native_rendering {
			// Make the line more clear on hi dpi screens: draw a rectangle
			mut width := math.abs(x2 - x)
			mut height := math.abs(y2 - y)
			if width == 0 {
				width = 1
			} else if height == 0 {
				height = 1
			}
			ctx.draw_rect(x, y, f32(width), f32(height), c)
			return
		}
	}
	if c.a != 255 {
		sgl.load_pipeline(ctx.timage_pip)
	}
	sgl.c4b(c.r, c.g, c.b, c.a)
	sgl.begin_line_strip()
	sgl.v2f(x * ctx.scale, y * ctx.scale)
	sgl.v2f(x2 * ctx.scale, y2 * ctx.scale)
	sgl.end()
}

// Draws a line between the points provided with the PenConfig
pub fn (ctx &Context) draw_line_with_config(x f32, y f32, x2 f32, y2 f32, config PenConfig) {
	if config.color.a != 255 {
		sgl.load_pipeline(ctx.timage_pip)
	}

	if config.thickness <= 0 {
		return
	}

	nx := x * ctx.scale
	ny := y * ctx.scale
	nx2 := x2 * ctx.scale
	ny2 := y2 * ctx.scale

	dx := nx2 - nx
	dy := ny2 - ny
	length := math.sqrtf(math.powf(x2 - x, 2) + math.powf(y2 - y, 2))
	theta := f32(math.atan2(dy, dx))

	sgl.push_matrix()

	sgl.translate(nx, ny, 0)
	sgl.rotate(theta, 0, 0, 1)
	sgl.translate(-nx, -ny, 0)

	if config.line_type == .solid {
		ctx.draw_rect(x, y, length, config.thickness, config.color)
	} else {
		size := if config.line_type == .dotted { config.thickness } else { config.thickness * 3 }
		space := if size == 1 { 2 } else { size }

		mut available := length
		mut start_x := x

		for i := 0; available > 0; i++ {
			if i % 2 == 0 {
				ctx.draw_rect(start_x, y, size, config.thickness, config.color)
				available -= size
				start_x += size
				continue
			}

			available -= space
			start_x += space
		}
	}

	sgl.pop_matrix()
}

// Draws a filled arc
[deprecated: 'use draw_arc_filled() instead']
pub fn (ctx &Context) draw_arc(x f32, y f32, inner_r f32, outer_r f32, start_angle f32, end_angle f32, segments int, c gx.Color) {
	ctx.draw_arc_filled(x, y, inner_r, outer_r, start_angle, end_angle, segments, c)
}

pub fn (ctx &Context) draw_arc_filled(x f32, y f32, inner_r f32, outer_r f32, start_angle f32, end_angle f32, segments int, c gx.Color) {
	if start_angle == end_angle || outer_r <= 0.0 {
		return
	}

	mut r1 := inner_r
	mut r2 := outer_r
	mut a1 := start_angle
	mut a2 := end_angle

	// TODO: Maybe this does not make since inner_r and outer_r is actually integers?
	if outer_r < inner_r {
		r1, r2 = r2, r1

		if r2 <= 0.0 {
			r2 = 0.1
		}
	}

	if a2 < a1 {
		a1, a2 = a2, a1
	}

	if r1 <= 0.0 {
		ctx.draw_slice(x, y, int(r2), a1, a2, segments, c)
		return
	}

	mut step_length := (a2 - a1) / f32(segments)
	mut angle := a1

	sgl.begin_quads()
	sgl.c4b(c.r, c.g, c.b, c.a)
	for _ in 0 .. segments {
		sgl.v2f(x + f32(math.sin(angle)) * r1, y + f32(math.cos(angle) * r1))
		sgl.v2f(x + f32(math.sin(angle)) * r2, y + f32(math.cos(angle) * r2))

		sgl.v2f(x + f32(math.sin(angle + step_length)) * r2, y + f32(math.cos(angle +
			step_length) * r2))
		sgl.v2f(x + f32(math.sin(angle + step_length)) * r1, y + f32(math.cos(angle +
			step_length) * r1))

		angle += step_length
	}
	sgl.end()
}

// Draws the outline of an arc
[deprecated: 'use draw_arc_empty() instead']
pub fn (ctx &Context) draw_empty_arc(x f32, y f32, inner_r f32, outer_r f32, start_angle f32, end_angle f32, segments int, c gx.Color) {
	ctx.draw_arc_empty(x, y, inner_r, outer_r, start_angle, end_angle, segments, c)
}

pub fn (ctx &Context) draw_arc_empty(x f32, y f32, inner_r f32, outer_r f32, start_angle f32, end_angle f32, segments int, c gx.Color) {
	if start_angle == end_angle || outer_r <= 0.0 {
		return
	}

	mut r1 := inner_r
	mut r2 := outer_r
	mut a1 := start_angle
	mut a2 := end_angle

	if outer_r < inner_r {
		r1, r2 = r2, r1

		if r2 <= 0.0 {
			r2 = 0.1
		}
	}

	if a2 < a1 {
		a1, a2 = a2, a1
	}

	if r1 <= 0.0 {
		ctx.draw_empty_slice(x, y, int(r2), a1, a2, segments, c)
		return
	}

	mut step_length := (a2 - a1) / f32(segments)
	mut angle := a1

	sgl.begin_line_strip()
	sgl.c4b(c.r, c.g, c.b, c.a)

	// Outer circle
	for _ in 0 .. segments {
		sgl.v2f(x + f32(math.sin(angle)) * r2, y + f32(math.cos(angle) * r2))
		sgl.v2f(x + f32(math.sin(angle + step_length)) * r2, y + f32(math.cos(angle +
			step_length) * r2))

		angle += step_length
	}

	// Inner circle
	for _ in 0 .. segments {
		sgl.v2f(x + f32(math.sin(angle)) * r1, y + f32(math.cos(angle) * r1))
		sgl.v2f(x + f32(math.sin(angle - step_length)) * r1, y +
			f32(math.cos(angle - step_length) * r1))

		angle -= step_length
	}

	// Closing end
	sgl.v2f(x + f32(math.sin(angle)) * r1, y + f32(math.cos(angle) * r1))
	sgl.v2f(x + f32(math.sin(angle)) * r2, y + f32(math.cos(angle) * r2))
	sgl.end()
}

// Draws a filled rounded rectangle
[deprecated: 'use draw_rounded_rect_filled()']
pub fn (ctx &Context) draw_rounded_rect(x f32, y f32, w f32, h f32, radius f32, c gx.Color) {
	ctx.draw_rounded_rect_filled(x, y, w, h, radius, c)
}

pub fn (ctx &Context) draw_rounded_rect_filled(x f32, y f32, w f32, h f32, radius f32, c gx.Color) {
	sgl.c4b(c.r, c.g, c.b, c.a)
	sgl.begin_triangle_strip()
	mut theta := f32(0)
	mut xx := f32(0)
	mut yy := f32(0)
	r := radius * ctx.scale
	nx := x * ctx.scale
	ny := y * ctx.scale
	width := w * ctx.scale
	height := h * ctx.scale
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
	mut rx := nx + width - r
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
	mut rby := ny + height - r
	for i in rb .. lb {
		theta = 2 * f32(math.pi) * f32(i) / segments
		xx = r * math.cosf(theta)
		yy = r * math.sinf(theta)
		sgl.v2f(xx + rbx, yy + rby)
		sgl.v2f(rbx, rby)
	}
	// left bottom
	mut lbx := lx
	mut lby := ny + height - r
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

// Draws the outline of a rounded rectangle
[deprecated: 'use draw_rounded_rect_empty()']
pub fn (ctx &Context) draw_empty_rounded_rect(x f32, y f32, w f32, h f32, radius f32, c gx.Color) {
	ctx.draw_rounded_rect_empty(x, y, w, h, radius, c)
}

pub fn (ctx &Context) draw_rounded_rect_empty(x f32, y f32, w f32, h f32, radius f32, c gx.Color) {
	mut theta := f32(0)
	mut xx := f32(0)
	mut yy := f32(0)
	r := radius * ctx.scale
	nx := x * ctx.scale
	ny := y * ctx.scale
	width := w * ctx.scale
	height := h * ctx.scale
	segments := 2 * math.pi * r
	segdiv := segments / 4
	rb := 0
	lb := int(rb + segdiv)
	lt := int(lb + segdiv)
	rt := int(lt + segdiv)
	sgl.c4b(c.r, c.g, c.b, c.a)
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
	mut rx := nx + width - r
	mut ry := ny + r
	for i in rt .. int(segments) {
		theta = 2 * f32(math.pi) * f32(i) / segments
		xx = r * math.cosf(theta)
		yy = r * math.sinf(theta)
		sgl.v2f(xx + rx, yy + ry)
	}
	// right bottom
	mut rbx := rx
	mut rby := ny + height - r
	for i in rb .. lb {
		theta = 2 * f32(math.pi) * f32(i) / segments
		xx = r * math.cosf(theta)
		yy = r * math.sinf(theta)
		sgl.v2f(xx + rbx, yy + rby)
	}
	// left bottom
	mut lbx := lx
	mut lby := ny + height - r
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
	x0 := points[0] * ctx.scale
	y0 := points[1] * ctx.scale
	for i in 1 .. (len / 2 + 1) {
		sgl.v2f(x0, y0)
		sgl.v2f(points[i * 4 - 2] * ctx.scale, points[i * 4 - 1] * ctx.scale)
		sgl.v2f(points[i * 4] * ctx.scale, points[i * 4 + 1] * ctx.scale)
	}

	if len % 2 == 0 {
		sgl.v2f(points[2 * len - 2] * ctx.scale, points[2 * len - 1] * ctx.scale)
	}
	sgl.end()
}

// draw_empty_poly - draws the borders of a polygon, given an array of points, and a color.
// Note that the points must be given in clockwise order.
[deprecated: 'use draw_poly_empty() instead']
pub fn (ctx &Context) draw_empty_poly(points []f32, c gx.Color) {
	ctx.draw_poly_empty(points, c)
}

pub fn (ctx &Context) draw_poly_empty(points []f32, c gx.Color) {
	assert points.len % 2 == 0
	len := points.len / 2
	assert len >= 3

	if c.a != 255 {
		sgl.load_pipeline(ctx.timage_pip)
	}
	sgl.c4b(c.r, c.g, c.b, c.a)

	sgl.begin_line_strip()
	for i in 0 .. len {
		sgl.v2f(points[2 * i] * ctx.scale, points[2 * i + 1] * ctx.scale)
	}
	sgl.v2f(points[0] * ctx.scale, points[1] * ctx.scale)
	sgl.end()
}

// draw_cubic_bezier draws a cubic Bézier curve, also known as a spline, from four points.
// The four points is provided as one `points` array which contains a stream of point pairs (x and y coordinates).
// Thus a cubic Bézier could be declared as: `points := [x1, y1, control_x1, control_y1, control_x2, control_y2, x2, y2]`.
// Please see `draw_cubic_bezier_in_steps` to control the amount of steps (segments) used to draw the curve.
pub fn (ctx &Context) draw_cubic_bezier(points []f32, c gx.Color) {
	ctx.draw_cubic_bezier_in_steps(points, u32(30 * ctx.scale), c)
}

// draw_cubic_bezier_in_steps draws a cubic Bézier curve, also known as a spline, from four points.
// The smoothness of the curve can be controlled with the `steps` parameter. `steps` determines how many iterations is
// taken to draw the curve.
// The four points is provided as one `points` array which contains a stream of point pairs (x and y coordinates).
// Thus a cubic Bézier could be declared as: `points := [x1, y1, control_x1, control_y1, control_x2, control_y2, x2, y2]`.
pub fn (ctx &Context) draw_cubic_bezier_in_steps(points []f32, steps u32, c gx.Color) {
	assert steps > 0
	assert points.len == 8

	if c.a != 255 {
		sgl.load_pipeline(ctx.timage_pip)
	}
	sgl.c4b(c.r, c.g, c.b, c.a)

	sgl.begin_line_strip()

	p1_x, p1_y := points[0], points[1]
	p2_x, p2_y := points[6], points[7]

	ctrl_p1_x, ctrl_p1_y := points[2], points[3]
	ctrl_p2_x, ctrl_p2_y := points[4], points[5]

	// The constant 3 is actually points.len() - 1;

	step := f32(1.0) / steps
	sgl.v2f(p1_x * ctx.scale, p1_y * ctx.scale)
	for u := f32(0.0); u <= f32(1.0); u += step {
		pow_2_u := u * u
		pow_3_u := pow_2_u * u

		x := pow_3_u * (p2_x + 3 * (ctrl_p1_x - ctrl_p2_x) - p1_x) +
			3 * pow_2_u * (p1_x - 2 * ctrl_p1_x + ctrl_p2_x) + 3 * u * (ctrl_p1_x - p1_x) + p1_x

		y := pow_3_u * (p2_y + 3 * (ctrl_p1_y - ctrl_p2_y) - p1_y) +
			3 * pow_2_u * (p1_y - 2 * ctrl_p1_y + ctrl_p2_y) + 3 * u * (ctrl_p1_y - p1_y) + p1_y

		sgl.v2f(x * ctx.scale, y * ctx.scale)
	}
	sgl.v2f(p2_x * ctx.scale, p2_y * ctx.scale)

	sgl.end()
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
		s = 1.0
	}
	return s
}

[deprecated: 'use draw_ellipse_filled() instead']
pub fn (ctx &Context) draw_ellipse(x f32, y f32, r_horizontal f32, r_vertical f32, c gx.Color) {
	ctx.draw_ellipse_filled(x, y, r_horizontal, r_vertical, c)
}

// draw_ellipse_filled draws an opaque elipse, with a center at x,y , filled with the color `c`
pub fn (ctx &Context) draw_ellipse_filled(x f32, y f32, r_horizontal f32, r_vertical f32, c gx.Color) {
	if c.a != 255 {
		sgl.load_pipeline(ctx.timage_pip)
	}

	sgl.c4b(c.r, c.g, c.b, c.a)
	sgl.begin_triangle_strip()
	for i := 0; i < 360; i += 10 {
		sgl.v2f(x, y)
		sgl.v2f(x + math.sinf(f32(math.radians(i))) * r_horizontal, y +
			math.cosf(f32(math.radians(i))) * r_vertical)
		sgl.v2f(x + math.sinf(f32(math.radians(i + 10))) * r_horizontal, y +
			math.cosf(f32(math.radians(i + 10))) * r_vertical)
	}
	sgl.end()
}

[deprecated: 'use draw_ellipse_empty() instead']
pub fn (ctx &Context) draw_empty_ellipse(x f32, y f32, r_horizontal f32, r_vertical f32, c gx.Color) {
	ctx.draw_ellipse_empty(x, y, r_horizontal, r_vertical, c)
}

// draw_ellipse_empty draws the outline of an ellipse, with a center at x,y
pub fn (ctx &Context) draw_ellipse_empty(x f32, y f32, r_horizontal f32, r_vertical f32, c gx.Color) {
	if c.a != 255 {
		sgl.load_pipeline(ctx.timage_pip)
	}

	sgl.c4b(c.r, c.g, c.b, c.a)
	sgl.begin_line_strip()
	for i := 0; i < 360; i += 10 {
		sgl.v2f(x + math.sinf(f32(math.radians(i))) * r_horizontal, y +
			math.cosf(f32(math.radians(i))) * r_vertical)
		sgl.v2f(x + math.sinf(f32(math.radians(i + 10))) * r_horizontal, y +
			math.cosf(f32(math.radians(i + 10))) * r_vertical)
	}
	sgl.end()
}
