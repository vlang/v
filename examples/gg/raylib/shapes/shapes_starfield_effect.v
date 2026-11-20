module main

// raylib [shapes] example - starfield effect, ported to gg.
import gg
import math
import rand

const star_count = 420
const white_c = gg.Color{255, 255, 255, 255}
const bg = gg.Color{25, 26, 56, 255}

struct App {
mut:
	ctx   &gg.Context = unsafe { nil }
	sx    []f32
	sy    []f32
	sz    []f32
	scr_x []f32
	scr_y []f32
	speed f32  = 10.0 / 9.0
	lines bool = true
}

fn (mut app App) reset(i int) {
	app.sx[i] = f32(rand.intn(800) or { 0 } - 400)
	app.sy[i] = f32(rand.intn(450) or { 0 } - 225)
	app.sz[i] = 1
}

fn on_frame(data voidptr) {
	mut app := unsafe { &App(data) }
	ctx := app.ctx
	ctx.begin()
	for i in 0 .. star_count {
		if app.lines {
			t := math.max(f32(0), math.min(f32(1), app.sz[i] + 1.0 / 32))
			if t - app.sz[i] > 0.001 {
				x0 := 400 + app.sx[i] / t
				y0 := 225 + app.sy[i] / t
				ctx.draw_line(x0, y0, app.scr_x[i], app.scr_y[i], white_c)
			}
		} else {
			r := app.sz[i] * 1 + 5 * (1 - app.sz[i])
			ctx.draw_circle_filled(app.scr_x[i], app.scr_y[i], r, white_c)
		}
	}
	ctx.draw_text(10, 40, '[MOUSE WHEEL] Current Speed: ${int(9 * app.speed / 2)}',
		size:  20
		color: white_c
	)
	mode := if app.lines { 'Lines' } else { 'Circles' }
	ctx.draw_text(10, 70, '[SPACE] Current draw mode: ${mode}', size: 20, color: white_c)
	ctx.end()
}

fn on_event(e &gg.Event, data voidptr) {
	mut app := unsafe { &App(data) }
	if e.typ == .key_down && e.key_code == .space {
		app.lines = !app.lines
	}
	if e.typ == .mouse_scroll {
		app.speed += 2 * e.scroll_y / 9
		if app.speed < 0 { app.speed = 0.1 }
		if app.speed > 2 { app.speed = 2 }
	}
}

fn on_update(dt f32, data voidptr) {
	mut app := unsafe { &App(data) }
	for i in 0 .. star_count {
		app.sz[i] -= dt * app.speed
		app.scr_x[i] = 400 + app.sx[i] / app.sz[i]
		app.scr_y[i] = 225 + app.sy[i] / app.sz[i]
		if app.sz[i] < 0 || app.scr_x[i] < 0 || app.scr_y[i] < 0
			|| app.scr_x[i] > f32(app.ctx.width) || app.scr_y[i] > f32(app.ctx.height) {
			app.reset(i)
		}
	}
}

fn main() {
	mut app := &App{
		sx:    []f32{len: star_count}
		sy:    []f32{len: star_count}
		sz:    []f32{len: star_count}
		scr_x: []f32{len: star_count}
		scr_y: []f32{len: star_count}
	}
	for i in 0 .. star_count { app.reset(i) }
	app.ctx = gg.new_context(
		width:        800
		height:       450
		window_title: 'raylib [shapes] example - starfield effect'
		user_data:    app
		frame_fn:     on_frame
		event_fn:     on_event
		update_fn:    on_update
		bg_color:     bg
	)
	app.ctx.run()
}
