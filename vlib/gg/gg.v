// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.

module gg

import gx
import sokol.sapp
import sokol.sgl
import sokol.gfx
import math

pub type FNCb = fn (data voidptr)

pub type FNEvent = fn (e &Event, data voidptr)

pub type FNFail = fn (msg string, data voidptr)

pub type FNKeyDown = fn (c KeyCode, m Modifier, data voidptr)

pub type FNKeyUp = fn (c KeyCode, m Modifier, data voidptr)

pub type FNMove = fn (x f32, y f32, data voidptr)

pub type FNClick = fn (x f32, y f32, button MouseButton, data voidptr)

pub type FNUnClick = fn (x f32, y f32, button MouseButton, data voidptr)

pub type FNChar = fn (c u32, data voidptr)

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

pub struct PenConfig {
	color     gx.Color
	line_type PenLineType = .solid
	thickness int = 1
}

pub struct Size {
pub:
	width  int
	height int
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

// quit closes the context window and exits the event loop for it
pub fn (ctx &Context) quit() {
	sapp.request_quit() // does not require ctx right now, but sokol multi-window might in the future
}

pub fn (mut ctx Context) set_bg_color(c gx.Color) {
	ctx.clear_pass = gfx.create_clear_pass(f32(c.r) / 255.0, f32(c.g) / 255.0, f32(c.b) / 255.0,
		f32(c.a) / 255.0)
}

[inline]
pub fn (ctx &Context) draw_square(x f32, y f32, s f32, c gx.Color) {
	ctx.draw_rect(x, y, s, s, c)
}

[inline]
pub fn (ctx &Context) set_pixel(x f32, y f32, c gx.Color) {
	if c.a != 255 {
		sgl.load_pipeline(ctx.timage_pip)
	}

	sgl.c4b(c.r, c.g, c.b, c.a)
	sgl.begin_points()
	sgl.v2f(x * ctx.scale, y * ctx.scale)
	sgl.end()
}

[direct_array_access; inline]
pub fn (ctx &Context) set_pixels(points []f32, c gx.Color) {
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

pub fn (ctx &Context) draw_triangle(x f32, y f32, x2 f32, y2 f32, x3 f32, y3 f32, c gx.Color) {
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

pub fn (ctx &Context) draw_empty_rect(x f32, y f32, w f32, h f32, c gx.Color) {
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

[inline]
pub fn (ctx &Context) draw_empty_square(x f32, y f32, s f32, c gx.Color) {
	ctx.draw_empty_rect(x, y, s, s, c)
}

pub fn (ctx &Context) draw_circle(x f32, y f32, r f32, c gx.Color) {
	ctx.draw_circle_with_segments(x, y, r, 10, c)
}

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

pub fn (ctx &Context) draw_arc_line(x f32, y f32, r int, start_angle f32, arc_angle f32, segments int, c gx.Color) {
	if c.a != 255 {
		sgl.load_pipeline(ctx.timage_pip)
	}
	sgl.c4b(c.r, c.g, c.b, c.a)
	theta := f32(arc_angle / f32(segments))
	tan_factor := math.tanf(theta)
	rad_factor := math.cosf(theta)
	nx := x * ctx.scale
	ny := y * ctx.scale
	mut xx := f32(r * math.cosf(start_angle))
	mut yy := f32(r * math.sinf(start_angle))
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

pub fn (ctx &Context) draw_arc(x f32, y f32, r int, start_angle f32, arc_angle f32, segments int, c gx.Color) {
	if c.a != 255 {
		sgl.load_pipeline(ctx.timage_pip)
	}
	sgl.c4b(c.r, c.g, c.b, c.a)
	nx := x * ctx.scale
	ny := y * ctx.scale
	theta := f32(arc_angle / f32(segments))
	tan_factor := math.tanf(theta)
	rad_factor := math.cosf(theta)
	mut xx := f32(r * math.cosf(start_angle))
	mut yy := f32(r * math.sinf(start_angle))
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

// resize the context's Window
pub fn (mut ctx Context) resize(width int, height int) {
	ctx.width = width
	ctx.height = height
	// C.sapp_resize_window(width, height)
}

// draw_line draws a line between the points provided
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

// draw_line_with_config draws a line between the points provided with the PenConfig
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

pub fn (ctx &Context) draw_rounded_rect(x f32, y f32, w f32, h f32, radius f32, color gx.Color) {
	sgl.c4b(color.r, color.g, color.b, color.a)
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

pub fn (ctx &Context) draw_empty_rounded_rect(x f32, y f32, w f32, h f32, radius f32, border_color gx.Color) {
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
