module main

// raylib [shapes] example - drag puzzle, ported to gg.
import gg
import math

const raywhite = gg.Color{245, 245, 245, 255}
const gray_c = gg.Color{130, 130, 130, 255}
const green_c = gg.Color{0, 158, 47, 255}
const red_c = gg.Color{230, 41, 55, 255}
const violet = gg.Color{135, 61, 190, 255}
const blue_c = gg.Color{0, 121, 241, 255}
const orange_c = gg.Color{255, 161, 0, 255}

struct App {
mut:
	ctx   &gg.Context = unsafe { nil }
	rx    f32
	ry    f32
	rw    f32
	rh    f32
	arx   f32
	ary   f32
	cx    f32
	cy    f32
	cr    f32 = 50
	acx   f32
	acy   f32
	acr   f32 = 55
	v1x   f32
	v1y   f32
	v2x   f32
	v2y   f32
	v3x   f32
	v3y   f32
	a1x   f32
	a1y   f32
	a2x   f32
	a2y   f32
	a3x   f32
	a3y   f32
	off_x f32
	off_y f32
	pick  int // 0 none, 1 rect, 2 circle, 3 triangle
}

fn point_tri(px f32, py f32, ax f32, ay f32, bx f32, by f32, cx f32, cy f32) bool {
	d1 := (px - bx) * (ay - by) - (ax - bx) * (py - by)
	d2 := (px - cx) * (by - cy) - (bx - cx) * (py - cy)
	d3 := (px - ax) * (cy - ay) - (cx - ax) * (py - ay)
	return (d1 >= 0 && d2 >= 0 && d3 >= 0) || (d1 <= 0 && d2 <= 0 && d3 <= 0)
}

fn on_frame(data voidptr) {
	mut app := unsafe { &App(data) }
	ctx := app.ctx
	ctx.begin()
	c1 := if app.rx >= app.arx && app.rx + app.rw <= app.arx + 110
		&& app.ry >= app.ary && app.ry + app.rh <= app.ary + 110 {
		green_c
	} else {
		red_c
	}
	ctx.draw_rect_empty(app.arx, app.ary, 110, 110, c1)
	dx, dy := app.cx - app.acx, app.cy - app.acy
	c2 := if math.sqrtf(dx * dx + dy * dy) < app.acr - app.cr { green_c } else { red_c }
	ctx.draw_circle_empty(app.acx, app.acy, app.acr, c2)
	tplaced := point_tri(app.v1x, app.v1y, app.a1x, app.a1y, app.a2x, app.a2y, app.a3x, app.a3y)
		&& point_tri(app.v2x, app.v2y, app.a1x, app.a1y, app.a2x, app.a2y, app.a3x, app.a3y)
		&& point_tri(app.v3x, app.v3y, app.a1x, app.a1y, app.a2x, app.a2y, app.a3x, app.a3y)
	c3 := if tplaced { green_c } else { red_c }
	ctx.draw_triangle_empty(app.a1x, app.a1y, app.a2x, app.a2y, app.a3x, app.a3y, c3)

	if app.pick != 3 {
		ctx.draw_triangle_filled(app.v1x, app.v1y, app.v2x, app.v2y, app.v3x, app.v3y, violet)
	}
	if app.pick != 2 {
		ctx.draw_circle_filled(app.cx, app.cy, app.cr, blue_c)
	}
	if app.pick != 1 {
		ctx.draw_rect_filled(app.rx, app.ry, app.rw, app.rh, orange_c)
	}
	if app.pick == 3 {
		ctx.draw_triangle_filled(app.v1x, app.v1y, app.v2x, app.v2y, app.v3x, app.v3y, violet)
	}
	if app.pick == 2 {
		ctx.draw_circle_filled(app.cx, app.cy, app.cr, blue_c)
	}
	if app.pick == 1 {
		ctx.draw_rect_filled(app.rx, app.ry, app.rw, app.rh, orange_c)
	}
	ctx.draw_text(10, 10, 'Use mouse to drag and drop the objects into the right spot!',
		size:  20
		color: gray_c
	)
	ctx.end()
}

fn on_event(e &gg.Event, data voidptr) {
	mut app := unsafe { &App(data) }
	mx, my := f32(e.mouse_x), f32(e.mouse_y)
	if e.typ == .mouse_down && e.mouse_button == .left {
		if mx >= app.rx && mx <= app.rx + app.rw && my >= app.ry && my <= app.ry + app.rh {
			app.pick = 1
			app.off_x, app.off_y = app.rx - mx, app.ry - my
		} else if math.sqrtf((mx - app.cx) * (mx - app.cx) + (my - app.cy) * (my - app.cy)) <= app.cr {
			app.pick = 2
			app.off_x, app.off_y = app.cx - mx, app.cy - my
		} else if point_tri(mx, my, app.v1x, app.v1y, app.v2x, app.v2y, app.v3x, app.v3y) {
			app.pick = 3
			app.off_x, app.off_y = app.v1x - mx, app.v1y - my
		}
	}
	if e.typ == .mouse_up && e.mouse_button == .left {
		app.pick = 0
	}
	if e.typ == .mouse_move && app.pick != 0 {
		x := mx + app.off_x
		y := my + app.off_y
		match app.pick {
			1 {
				app.rx, app.ry = x, y
			}
			2 {
				app.cx, app.cy = x, y
			}
			3 {
				dx, dy := app.v2x - app.v1x, app.v2y - app.v1y
				ex, ey := app.v3x - app.v1x, app.v3y - app.v1y
				app.v1x, app.v1y = x, y
				app.v2x, app.v2y = x + dx, y + dy
				app.v3x, app.v3y = x + ex, y + ey
			}
			else {}
		}
	}
}

fn main() {
	mut app := &App{
		rx:  150
		ry:  275
		arx: 340
		ary: 115
		cx:  400
		cy:  325
		acx: 205
		acy: 170
		v1x: 600
		v1y: 282
		v2x: 550
		v2y: 369
		v3x: 650
		v3y: 369
		a1x: 600
		a1y: 115
		a2x: 540
		a2y: 222
		a3x: 660
		a3y: 222
	}
	app.ctx = gg.new_context(
		width:        800
		height:       450
		window_title: 'raylib [shapes] example - drag puzzle'
		user_data:    app
		frame_fn:     on_frame
		event_fn:     on_event
		bg_color:     raywhite
	)
	app.ctx.run()
}
