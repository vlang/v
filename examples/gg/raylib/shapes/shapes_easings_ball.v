module main

// raylib [shapes] example - easings ball, ported to gg.
import gg
import math.easing

const raywhite = gg.Color{245, 245, 245, 255}
const black_c = gg.Color{0, 0, 0, 255}
const green_c = gg.Color{0, 158, 47, 255}
const red_c = gg.Color{230, 41, 55, 255}

struct App {
mut:
	ctx    &gg.Context = unsafe { nil }
	ball_x f32         = -100
	radius f32         = 20
	alpha  f32         = 1
	state  int
	frames int
}

fn on_frame(data voidptr) {
	mut app := unsafe { &App(data) }
	ctx := app.ctx
	ctx.begin()
	if app.state >= 2 {
		ctx.draw_rect_filled(0, 0, f32(ctx.width), f32(ctx.height), green_c)
	}
	col := red_c
	col.a = u8(255 * (1 - app.alpha))
	ctx.draw_circle_filled(app.ball_x, 200, app.radius, col)
	if app.state == 3 {
		ctx.draw_text(240, 200, 'PRESS [ENTER] TO PLAY AGAIN!', size: 20, color: black_c)
	}
	ctx.end()
}

fn on_event(e &gg.Event, data voidptr) {
	mut app := unsafe { &App(data) }
	if e.typ != .key_down {
		return
	}
	if e.key_code == .r {
		app.frames = 0
	}
	if e.key_code == .enter && app.state == 3 {
		app.ball_x = -100
		app.radius = 20
		app.alpha = 0
		app.state = 0
		app.frames = 0
	}
}

fn on_update(dt f32, data voidptr) {
	mut app := unsafe { &App(data) }
	_ = dt
	app.frames++
	f := f64(app.frames)
	match app.state {
		0 {
			app.ball_x = -100 + f32(easing.out_elastic(f / 120.0)) * 500
			if app.frames >= 120 {
				app.frames = 0
				app.state = 1
			}
		}
		1 {
			app.radius = 20 + f32(easing.in_elastic(f / 200.0)) * 480
			if app.frames >= 200 {
				app.frames = 0
				app.state = 2
			}
		}
		2 {
			app.alpha = f32(easing.out_cubic(f / 200.0))
			if app.frames >= 200 {
				app.frames = 0
				app.state = 3
			}
		}
		else {}
	}
}

fn main() {
	mut app := &App{}
	app.ctx = gg.new_context(
		width:        800
		height:       450
		window_title: 'raylib [shapes] example - easings ball'
		user_data:    app
		frame_fn:     on_frame
		event_fn:     on_event
		update_fn:    on_update
		bg_color:     raywhite
	)
	app.ctx.run()
}
