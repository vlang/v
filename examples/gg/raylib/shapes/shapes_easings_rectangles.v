module main

// raylib [shapes] example - easings rectangles, ported to gg.
import gg
import math
import math.easing

const recs_width = 50
const recs_height = 50
const max_x = 16
const max_y = 9
const play_time = 240
const raywhite = gg.Color{245, 245, 245, 255}
const gray_c = gg.Color{130, 130, 130, 255}
const red_c = gg.Color{230, 41, 55, 255}

struct App {
mut:
	ctx    &gg.Context = unsafe { nil }
	w      []f32
	h      []f32
	rot    f32
	frames int
	state  int
}

fn draw_rot_rect(ctx &gg.Context, x f32, y f32, w f32, h f32, angle f32, c gg.Color) {
	rad := f32(math.radians(angle))
	ux, uy := math.cosf(rad), math.sinf(rad)
	ctx.draw_convex_poly([
		x - ux * w / 2 - uy * h / 2,
		y - uy * w / 2 + ux * h / 2,
		x + ux * w / 2 - uy * h / 2,
		y + uy * w / 2 + ux * h / 2,
		x + ux * w / 2 + uy * h / 2,
		y + uy * w / 2 - ux * h / 2,
		x - ux * w / 2 + uy * h / 2,
		y - uy * w / 2 - ux * h / 2,
	], c)
}

fn on_frame(data voidptr) {
	mut app := unsafe { &App(data) }
	ctx := app.ctx
	ctx.begin()
	if app.state == 0 {
		for gy in 0 .. max_y {
			for gx in 0 .. max_x {
				i := gy * max_x + gx
				draw_rot_rect(ctx, 25 + f32(gx) * recs_width, 25 + f32(gy) * recs_height,
					app.w[i], app.h[i], app.rot, red_c)
			}
		}
	} else {
		ctx.draw_text(240, 200, 'PRESS [SPACE] TO PLAY AGAIN!', size: 20, color: gray_c)
	}
	ctx.end()
}

fn on_event(e &gg.Event, data voidptr) {
	mut app := unsafe { &App(data) }
	if e.typ == .key_down && e.key_code == .space && app.state == 1 {
		app.frames = 0
		for i in 0 .. app.w.len {
			app.w[i] = recs_width
			app.h[i] = recs_height
		}
		app.state = 0
	}
}

fn on_update(dt f32, data voidptr) {
	mut app := unsafe { &App(data) }
	_ = dt
	if app.state != 0 {
		return
	}
	app.frames++
	e := f32(easing.out_circ(f64(app.frames) / play_time))
	for i in 0 .. app.w.len {
		app.w[i] = math.max(f32(0), recs_width + e * -recs_width)
		app.h[i] = math.max(f32(0), recs_height + e * -recs_height)
	}
	app.rot = f32(easing.linear(f64(app.frames) / play_time)) * 360
	if app.w[0] == 0 && app.h[0] == 0 {
		app.state = 1
	}
}

fn main() {
	mut app := &App{
		w: []f32{len: max_x * max_y, init: recs_width}
		h: []f32{len: max_x * max_y, init: recs_height}
	}
	app.ctx = gg.new_context(
		width:        800
		height:       450
		window_title: 'raylib [shapes] example - easings rectangles'
		user_data:    app
		frame_fn:     on_frame
		event_fn:     on_event
		update_fn:    on_update
		bg_color:     raywhite
	)
	app.ctx.run()
}
