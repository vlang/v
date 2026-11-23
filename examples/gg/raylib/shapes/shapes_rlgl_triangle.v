module main

// raylib [shapes] example - rlgl triangle, ported to gg.
// PORT NOTE: gg has no per-vertex color gradient; the triangle uses three flat
// colored sub-triangles to keep the red/green/blue vertex identity visible.
import gg
import math

const raywhite = gg.Color{245, 245, 245, 255}
const darkgray = gg.Color{80, 80, 80, 255}
const black_c = gg.Color{0, 0, 0, 255}
const red_c = gg.Color{230, 41, 55, 255}
const green_c = gg.Color{0, 158, 47, 255}
const blue_c = gg.Color{0, 121, 241, 255}

struct App {
mut:
	ctx   &gg.Context = unsafe { nil }
	pts   [3]gg.Vec2
	sel   int = -1
	lines bool
}

fn on_frame(data voidptr) {
	mut app := unsafe { &App(data) }
	ctx := app.ctx
	ctx.begin()
	p := app.pts
	cx, cy := (p[0].x + p[1].x + p[2].x) / 3, (p[0].y + p[1].y + p[2].y) / 3
	if app.lines {
		ctx.draw_line(p[0].x, p[0].y, p[1].x, p[1].y, red_c)
		ctx.draw_line(p[1].x, p[1].y, p[2].x, p[2].y, green_c)
		ctx.draw_line(p[2].x, p[2].y, p[0].x, p[0].y, blue_c)
	} else {
		ctx.draw_triangle_filled(p[0].x, p[0].y, p[1].x, p[1].y, cx, cy, red_c)
		ctx.draw_triangle_filled(p[1].x, p[1].y, p[2].x, p[2].y, cx, cy, green_c)
		ctx.draw_triangle_filled(p[2].x, p[2].y, p[0].x, p[0].y, cx, cy, blue_c)
	}
	mx, my := f32(ctx.mouse_pos_x), f32(ctx.mouse_pos_y)
	for i in 0 .. 3 {
		dx, dy := mx - p[i].x, my - p[i].y
		if math.sqrtf(dx * dx + dy * dy) <= 8 {
			ctx.draw_circle_filled(p[i].x, p[i].y, 8, gg.Color{80, 80, 80, 128})
		}
		if i == app.sel {
			ctx.draw_circle_filled(p[i].x, p[i].y, 8, darkgray)
		}
		ctx.draw_circle_empty(p[i].x, p[i].y, 8, black_c)
	}
	ctx.draw_text(10, 10, 'SPACE: Toggle lines mode', size: 20, color: darkgray)
	ctx.draw_text(10, 40, 'LEFT-RIGHT: Toggle backface culling', size: 20, color: darkgray)
	ctx.draw_text(10, 70, 'MOUSE: Click and drag vertex points', size: 20, color: darkgray)
	ctx.draw_text(10, 100, 'R: Reset triangle to start positions', size: 20, color: darkgray)
	ctx.end()
}

fn on_event(e &gg.Event, data voidptr) {
	mut app := unsafe { &App(data) }
	if e.typ == .key_down {
		match e.key_code {
			.space { app.lines = !app.lines }
			.r {
				app.pts = [gg.Vec2{400, 150}, gg.Vec2{300, 300}, gg.Vec2{500, 300}]!
			}
			else {}
		}
	}
	if e.typ == .mouse_down && e.mouse_button == .left {
		for i in 0 .. 3 {
			dx, dy := e.mouse_x - app.pts[i].x, e.mouse_y - app.pts[i].y
			if math.sqrtf(dx * dx + dy * dy) <= 8 {
				app.sel = i
				break
			}
		}
	}
	if e.typ == .mouse_up && e.mouse_button == .left { app.sel = -1 }
}

fn on_update(dt f32, data voidptr) {
	mut app := unsafe { &App(data) }
	_ = dt
	if app.sel >= 0 {
		app.pts[app.sel].x = f32(app.ctx.mouse_pos_x)
		app.pts[app.sel].y = f32(app.ctx.mouse_pos_y)
	}
}

fn main() {
	mut app := &App{ pts: [gg.Vec2{400, 150}, gg.Vec2{300, 300}, gg.Vec2{500, 300}]! }
	app.ctx = gg.new_context(
		width:        800
		height:       450
		window_title: 'raylib [shapes] example - rlgl triangle'
		user_data:    app
		frame_fn:     on_frame
		event_fn:     on_event
		update_fn:    on_update
		bg_color:     raywhite
	)
	app.ctx.run()
}
