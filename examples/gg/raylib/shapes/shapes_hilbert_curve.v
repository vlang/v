module main

// raylib [shapes] example - hilbert curve, ported to gg.
import gg
import math

const raywhite = gg.Color{245, 245, 245, 255}
const maroon = gg.Color{190, 33, 55, 255}
const gray_c = gg.Color{130, 130, 130, 255}

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

fn hilbert_step(order int, index int) (f32, f32) {
	px := [4]int[0, 0, 1, 1]
	py := [4]int[0, 1, 1, 0]
	mut hi := index & 3
	mut vx, mut vy := f32(px[hi]), f32(py[hi])
	mut idx := index
	for j := 1; j < order; j++ {
		idx >>= 2
		hi = idx & 3
		len := 1 << j
		match hi {
			0 {
				t := vx
				vx, vy = vy, t
			}
			2 {
				vx += len
				vy += len
			}
			1 {
				vy += len
			}
			3 {
				t := f32(len) - 1 - vx
				vx = 2 * f32(len) - 1 - vy
				vy = t
			}
			else {}
		}
	}
	return vx, vy
}

struct App {
mut:
	ctx        &gg.Context = unsafe { nil }
	order      int         = 2
	size       f32         = 450
	thick      f32         = 2
	count      int
	animate    bool = true
	path       []gg.Vec2
	prev_order int
	prev_size  f32
}

fn (mut app App) rebuild() {
	n := 1 << app.order
	app.path = []gg.Vec2{len: n * n, cap: n * n}
	len := app.size / f32(n)
	for i in 0 .. n * n {
		x, y := hilbert_step(app.order, i)
		app.path[i] = gg.Vec2{x * len + len / 2, y * len + len / 2}
	}
	app.count = if app.animate { 0 } else { n * n }
	app.prev_order = app.order
	app.prev_size = app.size
}

fn on_frame(data voidptr) {
	mut app := unsafe { &App(data) }
	ctx := app.ctx
	ctx.begin()
	n := app.path.len
	if app.count < n {
		for i in 1 .. app.count {
			ctx.draw_line(app.path[i].x, app.path[i].y, app.path[i - 1].x, app.path[i - 1].y,
				hsv(f32(i) / f32(n) * 360, 1, 1))
		}
		app.count++
	} else {
		for i in 1 .. n {
			ctx.draw_line(app.path[i].x, app.path[i].y, app.path[i - 1].x, app.path[i - 1].y,
				hsv(f32(i) / f32(n) * 360, 1, 1))
		}
	}
	// PORT NOTE: raygui controls are replaced by simple keyboard controls.
	ctx.draw_text(450, 50, 'ANIMATE: ${app.animate} [A]', size: 16, color: maroon)
	ctx.draw_text(450, 90, 'ORDER: ${app.order} ([+]/[-])', size: 16, color: maroon)
	ctx.draw_text(450, 130, 'THICKNESS: ${int(app.thick)} ([T]/[G])', size: 16, color: maroon)
	ctx.draw_text(450, 170, 'SIZE: ${int(app.size)} ([Z]/[X])', size: 16, color: maroon)
	ctx.draw_text(10, ctx.height - 30, 'LINES: ${app.count}/${n}', size: 20, color: maroon)
	ctx.end()
}

fn on_event(e &gg.Event, data voidptr) {
	mut app := unsafe { &App(data) }
	if e.typ != .key_down {
		return
	}
	match e.key_code {
		.a { app.animate = !app.animate }
		.equal, .kp_add {
			if app.order < 8 { app.order++ }
		}
		.minus, .kp_subtract {
			if app.order > 2 { app.order-- }
		}
		.t { app.thick += 1 }
		.g {
			if app.thick > 1 { app.thick-- }
		}
		.z { app.size -= 10 }
		.x { app.size += 10 }
		else {}
	}
}

fn on_update(dt f32, data voidptr) {
	mut app := unsafe { &App(data) }
	_ = dt
	if app.order != app.prev_order || int(app.size) != int(app.prev_size) {
		app.rebuild()
	}
}

fn main() {
	mut app := &App{}
	app.rebuild()
	app.ctx = gg.new_context(
		width:        800
		height:       450
		window_title: 'raylib [shapes] example - hilbert curve'
		user_data:    app
		frame_fn:     on_frame
		event_fn:     on_event
		update_fn:    on_update
		bg_color:     raywhite
	)
	app.ctx.run()
}
