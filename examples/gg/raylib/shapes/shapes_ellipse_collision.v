module main

// raylib [shapes] example - ellipse collision, ported to gg.
import gg
import math

const raywhite = gg.Color{245, 245, 245, 255}
const darkgray = gg.Color{80, 80, 80, 255}
const red_c = gg.Color{230, 41, 55, 255}
const blue_c = gg.Color{0, 121, 241, 255}
const green_c = gg.Color{0, 158, 47, 255}
const white_c = gg.Color{255, 255, 255, 255}
const yellow_c = gg.Color{253, 249, 0, 255}
const gray_c = gg.Color{130, 130, 130, 255}

struct App {
mut:
	ctx        &gg.Context = unsafe { nil }
	ax         f32
	ay         f32
	arx        f32
	ary        f32
	bx         f32
	by         f32
	brx        f32
	bry        f32
	controlled int
	collide    bool
	mouse_in_a bool
	mouse_in_b bool
}

fn point_ellipse(px f32, py f32, cx f32, cy f32, rx f32, ry f32) bool {
	dx := (px - cx) / rx
	dy := (py - cy) / ry
	return dx * dx + dy * dy <= 1
}

fn ellipses_collide(ax f32, ay f32, arx f32, ary f32, bx f32, by f32, brx f32, bry f32) bool {
	dx, dy := bx - ax, by - ay
	dist := math.sqrtf(dx * dx + dy * dy)
	if dist == 0 {
		return true
	}
	theta := f32(math.atan2(dy, dx))
	ct, st := math.cosf(theta), math.sinf(theta)
	r1 := (arx * ary) / math.sqrtf((ary * ct) * (ary * ct) + (arx * st) * (arx * st))
	r2 := (brx * bry) / math.sqrtf((bry * ct) * (bry * ct) + (brx * st) * (brx * st))
	return dist <= r1 + r2
}

fn on_frame(data voidptr) {
	mut app := unsafe { &App(data) }
	ctx := app.ctx
	ctx.begin()
	ca := if app.collide { red_c } else { blue_c }
	cb := if app.collide { red_c } else { green_c }
	ctx.draw_ellipse_filled(app.ax, app.ay, app.arx, app.ary, ca)
	ctx.draw_ellipse_filled(app.bx, app.by, app.brx, app.bry, cb)
	ctx.draw_ellipse_empty(app.ax, app.ay, app.arx, app.ary, white_c)
	ctx.draw_ellipse_empty(app.bx, app.by, app.brx, app.bry, white_c)
	ctx.draw_circle_filled(app.ax, app.ay, 4, white_c)
	ctx.draw_circle_filled(app.bx, app.by, 4, white_c)
	if app.collide {
		ctx.draw_text(ctx.width / 2 - 120, 40, 'ELLIPSES COLLIDE', size: 28, color: red_c)
	} else {
		ctx.draw_text(ctx.width / 2 - 80, 40, 'NO COLLISION', size: 28, color: darkgray)
	}
	ctrl := if app.controlled == 0 { 'Controlling: A' } else { 'Controlling: B' }
	ctx.draw_text(20, ctx.height - 40, ctrl, size: 20, color: yellow_c)
	if app.mouse_in_a && app.controlled != 0 {
		ctx.draw_text(20, ctx.height - 70, 'Mouse inside ellipse A', size: 20, color: blue_c)
	}
	if app.mouse_in_b && app.controlled != 1 {
		ctx.draw_text(20, ctx.height - 70, 'Mouse inside ellipse B', size: 20, color: green_c)
	}
	ctx.draw_text(20, 20, 'Press [A] or [B] to switch control', size: 20, color: gray_c)
	ctx.end()
}

fn on_event(e &gg.Event, data voidptr) {
	mut app := unsafe { &App(data) }
	if e.typ == .key_down {
		match e.key_code {
			.a { app.controlled = 0 }
			.b { app.controlled = 1 }
			else {}
		}
	}
}

fn on_update(dt f32, data voidptr) {
	mut app := unsafe { &App(data) }
	_ = dt
	mx, my := f32(app.ctx.mouse_pos_x), f32(app.ctx.mouse_pos_y)
	if app.controlled == 0 {
		app.ax, app.ay = mx, my
	} else {
		app.bx, app.by = mx, my
	}
	app.collide = ellipses_collide(app.ax, app.ay, app.arx, app.ary, app.bx, app.by, app.brx, app.bry)
	app.mouse_in_a = point_ellipse(mx, my, app.ax, app.ay, app.arx, app.ary)
	app.mouse_in_b = point_ellipse(mx, my, app.bx, app.by, app.brx, app.bry)
}

fn main() {
	// Radii must be non-zero: point_ellipse() and ellipses_collide() divide by them.
	mut app := &App{
		ax:  200
		ay:  225
		arx: 120
		ary: 70
		bx:  600
		by:  225
		brx: 90
		bry: 90
	}
	app.ctx = gg.new_context(
		width:        800
		height:       450
		window_title: 'raylib [shapes] example - ellipse collision'
		user_data:    app
		frame_fn:     on_frame
		event_fn:     on_event
		update_fn:    on_update
		bg_color:     raywhite
	)
	app.ctx.run()
}
