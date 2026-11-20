module main

// raylib [shapes] example - rlgl color wheel, ported to gg.
// PORT NOTE: per-vertex rlgl gradients are approximated with a fan of solid triangles.
import gg
import math

const raywhite = gg.Color{245, 245, 245, 255}
const black_c = gg.Color{0, 0, 0, 255}
const darkgray = gg.Color{80, 80, 80, 255}
const darkgreen = gg.Color{0, 117, 44, 255}

fn hsv(h f32, s f32, v f32) gg.Color {
	c := v * s
	x := c * (1 - math.abs(f32(math.fmod(f64(h / 60), 2.0)) - 1))
	m := v - c
	mut r, mut g, mut b := f32(0), f32(0), f32(0)
	if h < 60 {
		r, g = c, x
	} else if h < 120 {
		r, g = x, c
	} else if h < 180 {
		g, b = c, x
	} else if h < 240 {
		g, b = x, c
	} else if h < 300 {
		r, b = x, c
	} else {
		r, b = c, x
	}
	return gg.Color{u8((r + m) * 255), u8((g + m) * 255), u8((b + m) * 255), 255}
}

struct App {
mut:
	ctx       &gg.Context = unsafe { nil }
	triangles int         = 64
	scale     f32         = 150
	value     f32         = 1
	cx        f32         = 400
	cy        f32         = 225
	handle_x  f32         = 400
	handle_y  f32         = 225
	setting   bool
	color     gg.Color = gg.Color{255, 255, 255, 255}
}

fn on_frame(data voidptr) {
	mut app := unsafe { &App(data) }
	ctx := app.ctx
	ctx.begin()
	n := app.triangles
	for i in 0 .. n {
		a0 := f32(math.tau) * f32(i) / f32(n)
		a1 := f32(math.tau) * f32(i + 1) / f32(n)
		x1 := app.cx + math.sinf(a0) * app.scale
		y1 := app.cy - math.cosf(a0) * app.scale
		x2 := app.cx + math.sinf(a1) * app.scale
		y2 := app.cy - math.cosf(a1) * app.scale
		c := hsv(f32(i) / f32(n) * 360, 1, 1)
		ctx.draw_triangle_filled(x1, y1, app.cx, app.cy, x2, y2, c)
	}
	// value slider dims the wheel via an overlay
	if app.value < 1 {
		ctx.draw_circle_filled(app.cx, app.cy, app.scale, gg.Color{255, 255, 255, u8((1 - app.value) * 255)})
	}
	ctx.draw_circle_empty(app.handle_x, app.handle_y, 4, black_c)
	ctx.draw_rect_filled(8, 8, 64, 64, app.color)
	ctx.draw_rect_empty(8, 8, 64, 64, black_c)
	ctx.draw_text(8, 80, '# color preview', size: 20, color: darkgray)
	ctx.draw_text(8, 110, '(${app.color.r}, ${app.color.g}, ${app.color.b})',
		size:  20
		color: darkgray
	)
	ctx.draw_text(8, 395, 'triangle count: ${n}', size: 20, color: darkgray)
	cc := if app.ctx.pressed_keys[gg.KeyCode.left_control] && app.ctx.pressed_keys[gg.KeyCode.c] {
		darkgreen
	} else {
		darkgray
	}
	ctx.draw_text(8, 425, 'press ctrl+c to copy!', size: 20, color: cc)
	ctx.draw_text(120, 20, 'value: ${app.value:.2f} [+/]', size: 16, color: darkgray)
	ctx.end()
}

fn on_event(e &gg.Event, data voidptr) {
	mut app := unsafe { &App(data) }
	if e.typ == .mouse_down && e.mouse_button == .left {
		dx, dy := e.mouse_x - app.cx, e.mouse_y - app.cy
		if math.sqrtf(dx * dx + dy * dy) <= app.scale + 10 {
			app.setting = true
		}
		if e.mouse_x >= 42 && e.mouse_x <= 106 && e.mouse_y >= 125 && e.mouse_y <= 141 {
			app.value = (e.mouse_x - 42) / 64
		}
	}
	if e.typ == .mouse_up && e.mouse_button == .left { app.setting = false }
	if e.typ == .mouse_scroll {
		app.triangles += int(e.scroll_y)
		if app.triangles < 3 { app.triangles = 3 }
		if app.triangles > 256 { app.triangles = 256 }
	}
	if e.typ == .key_down {
		match e.key_code {
			.equal, .kp_add {
				if app.value < 1 { app.value += 0.05 }
			}
			.minus, .kp_subtract {
				if app.value > 0 { app.value -= 0.05 }
			}
			else {}
		}
	}
}

fn on_update(dt f32, data voidptr) {
	mut app := unsafe { &App(data) }
	_ = dt
	if app.ctx.pressed_keys[gg.KeyCode.up] && app.scale < 225 { app.scale *= 1.025 }
	if app.ctx.pressed_keys[gg.KeyCode.down] && app.scale > 32 { app.scale *= 0.975 }
	if app.setting {
		app.handle_x, app.handle_y = f32(app.ctx.mouse_pos_x), f32(app.ctx.mouse_pos_y)
		dx, dy := app.handle_x - app.cx, app.handle_y - app.cy
		dist := math.sqrtf(dx * dx + dy * dy)
		if dist > app.scale {
			app.handle_x = app.cx + dx / dist * app.scale
			app.handle_y = app.cy + dy / dist * app.scale
		}
		ang := f32(math.fmod(f64(f32(math.degrees(math.atan2(dy, dx))) + 90 + 360), 360.0))
		app.color = hsv(ang, math.min(f32(1), dist / app.scale), 1)
	}
}

fn main() {
	mut app := &App{}
	app.ctx = gg.new_context(
		width:        800
		height:       450
		window_title: 'raylib [shapes] example - rlgl color wheel'
		user_data:    app
		frame_fn:     on_frame
		event_fn:     on_event
		update_fn:    on_update
		bg_color:     raywhite
	)
	app.ctx.run()
}
