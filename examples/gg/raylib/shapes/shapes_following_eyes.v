module main

// raylib [shapes] example - following eyes, ported to gg.
import gg
import math

const raywhite = gg.Color{245, 245, 245, 255}
const lightgray = gg.Color{200, 200, 200, 255}
const brown = gg.Color{127, 106, 79, 255}
const darkgreen = gg.Color{0, 117, 44, 255}
const black_c = gg.Color{0, 0, 0, 255}

struct App {
mut:
	ctx        &gg.Context = unsafe { nil }
	sclera_lx  f32         = 300
	sclera_ly  f32         = 225
	sclera_rx  f32         = 500
	sclera_ry  f32         = 225
	sclera_rad f32         = 80
	iris_lx    f32         = 300
	iris_ly    f32         = 225
	iris_rx    f32         = 500
	iris_ry    f32         = 225
	iris_rad   f32         = 24
}

fn on_frame(data voidptr) {
	mut app := unsafe { &App(data) }
	ctx := app.ctx
	ctx.begin()
	ctx.draw_circle_filled(app.sclera_lx, app.sclera_ly, app.sclera_rad, lightgray)
	ctx.draw_circle_filled(app.iris_lx, app.iris_ly, app.iris_rad, brown)
	ctx.draw_circle_filled(app.iris_lx, app.iris_ly, 10, black_c)
	ctx.draw_circle_filled(app.sclera_rx, app.sclera_ry, app.sclera_rad, lightgray)
	ctx.draw_circle_filled(app.iris_rx, app.iris_ry, app.iris_rad, darkgreen)
	ctx.draw_circle_filled(app.iris_rx, app.iris_ry, 10, black_c)
	ctx.end()
}

fn on_update(dt f32, data voidptr) {
	mut app := unsafe { &App(data) }
	_ = dt
	mx, my := f32(app.ctx.mouse_pos_x), f32(app.ctx.mouse_pos_y)
	app.iris_lx, app.iris_ly = mx, my
	app.iris_rx, app.iris_ry = mx, my
	limit := app.sclera_rad - app.iris_rad
	// clamp each iris to its sclera
	mut dx := app.iris_lx - app.sclera_lx
	mut dy := app.iris_ly - app.sclera_ly
	if math.sqrtf(dx * dx + dy * dy) > limit {
		a := f32(math.atan2(dy, dx))
		app.iris_lx = app.sclera_lx + limit * math.cosf(a)
		app.iris_ly = app.sclera_ly + limit * math.sinf(a)
	}
	dx = app.iris_rx - app.sclera_rx
	dy = app.iris_ry - app.sclera_ry
	if math.sqrtf(dx * dx + dy * dy) > limit {
		a := f32(math.atan2(dy, dx))
		app.iris_rx = app.sclera_rx + limit * math.cosf(a)
		app.iris_ry = app.sclera_ry + limit * math.sinf(a)
	}
}

fn main() {
	mut app := &App{}
	app.ctx = gg.new_context(
		width:        800
		height:       450
		window_title: 'raylib [shapes] example - following eyes'
		user_data:    app
		frame_fn:     on_frame
		event_fn:     fn (e &gg.Event, data voidptr) {
			_ = e
			_ = data
		}
		update_fn:    on_update
		bg_color:     raywhite
	)
	app.ctx.run()
}
