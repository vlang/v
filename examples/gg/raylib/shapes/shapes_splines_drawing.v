module main

// raylib [shapes] example - splines drawing, ported to gg.
// PORT NOTE: raygui replaced by number keys; B-spline/Catmull-Rom drawn as smooth polylines.
import gg
import math

const max_points = 32
const raywhite = gg.Color{245, 245, 245, 255}
const red_c = gg.Color{230, 41, 55, 255}
const gold = gg.Color{255, 203, 0, 255}
const green_c = gg.Color{0, 158, 47, 255}
const gray_c = gg.Color{130, 130, 130, 255}
const darkblue = gg.Color{0, 82, 172, 255}
const lightgray = gg.Color{200, 200, 200, 255}
const blue_c = gg.Color{0, 121, 241, 255}
const black_c = gg.Color{0, 0, 0, 255}

struct App {
mut:
	ctx        &gg.Context = unsafe { nil }
	pts        []gg.Vec2
	ctrl_start []gg.Vec2
	ctrl_end   []gg.Vec2
	count      int = 5
	sel        int = -1
	kind       int
	helpers    bool = true
}

fn (mut app App) init_points() {
	app.pts = [gg.Vec2{50, 400}, gg.Vec2{160, 220}, gg.Vec2{340, 380}, gg.Vec2{520, 60},
		gg.Vec2{710, 260}]!
	app.ctrl_start = []gg.Vec2{len: max_points - 1}
	app.ctrl_end = []gg.Vec2{len: max_points - 1}
	for i in 0 .. app.count - 1 {
		app.ctrl_start[i] = gg.Vec2{app.pts[i].x + 50, app.pts[i].y}
		app.ctrl_end[i] = gg.Vec2{app.pts[i + 1].x - 50, app.pts[i + 1].y}
	}
}

fn spline_points(app &App) []gg.Vec2 {
	mut out := []gg.Vec2{}
	if app.kind == 3 {
		for i in 0 .. app.count - 1 {
			out << app.pts[i]
			out << app.ctrl_start[i]
			out << app.ctrl_end[i]
		}
		out << app.pts[app.count - 1]
		return out
	}
	if app.count < 4 {
		return app.pts[..app.count]
	}
	for i in 0 .. app.count - 3 {
		p0 := app.pts[if i > 0 { i - 1 } else { 0 }]
		p1, p2, p3 := app.pts[i], app.pts[i + 1], app.pts[i + 2]
		for s in 0 .. 16 {
			t := f32(s) / 16
			if app.kind == 1 {
				// uniform cubic B-spline
				out << gg.Vec2{f32(0.5) * ((p0.x + 2 * p1.x + p2.x) * (1 - t) * (1 - t) +
					(2 * p2.x - p0.x) * t * (1 - t) + (p0.x - 2 * p1.x + p2.x) * t * t), f32(0.5) * ((p0.y + 2 * p1.y + p2.y) * (1 - t) * (1 - t) +
					(2 * p2.y - p0.y) * t * (1 - t) + (p0.y - 2 * p1.y + p2.y) * t * t)}
			} else {
				t2, t3 := t * t, t * t * t
				out << gg.Vec2{f32(0.5) * (2 * p1.x + (-p0.x + p2.x) * t + (2 * p0.x - 5 * p1.x + 4 * p2.x - p3.x) * t2 + (-p0.x + 3 * p1.x - 3 * p2.x + p3.x) * t3), f32(0.5) * (2 * p1.y + (-p0.y + p2.y) * t + (2 * p0.y - 5 * p1.y + 4 * p2.y - p3.y) * t2 + (-p0.y + 3 * p1.y - 3 * p2.y + p3.y) * t3)}
			}
		}
	}
	out << app.pts[app.count - 1]
	return out
}

fn on_frame(data voidptr) {
	mut app := unsafe { &App(data) }
	ctx := app.ctx
	ctx.begin()
	pts := spline_points(app)
	for i in 1 .. pts.len {
		ctx.draw_line(pts[i].x, pts[i].y, pts[i - 1].x, pts[i - 1].y, red_c)
	}
	if app.kind == 3 {
		for i in 0 .. app.count - 1 {
			ctx.draw_circle_filled(app.ctrl_start[i].x, app.ctrl_start[i].y, 6, gold)
			ctx.draw_circle_filled(app.ctrl_end[i].x, app.ctrl_end[i].y, 6, gold)
			ctx.draw_line(app.pts[i].x, app.pts[i].y, app.ctrl_start[i].x, app.ctrl_start[i].y, gray_c)
			ctx.draw_line(app.pts[i + 1].x, app.pts[i + 1].y, app.ctrl_end[i].x, app.ctrl_end[i].y, gray_c)
		}
	}
	if app.helpers {
		for i in 0 .. app.count {
			p := app.pts[i]
			ctx.draw_circle_empty(p.x, p.y, 8, if i == app.sel { blue_c } else { darkblue })
			ctx.draw_text(int(p.x), int(p.y) + 10, '[${int(p.x)}, ${int(p.y)}]',
				size:  10
				color: black_c
			)
		}
	}
	kind_name := ['LINEAR', 'BSPLINE', 'CATMULLROM', 'BEZIER'][app.kind]
	ctx.draw_text(12, 10, 'Spline type: ${kind_name} [1-4]', size: 16, color: black_c)
	ctx.draw_text(12, 40, 'helpers: ${app.helpers} [H]  right-click: add point',
		size:  16
		color: black_c
	)
	ctx.end()
}

fn on_event(e &gg.Event, data voidptr) {
	mut app := unsafe { &App(data) }
	mx, my := f32(e.mouse_x), f32(e.mouse_y)
	if e.typ == .mouse_down && e.mouse_button == .right && app.count < max_points {
		app.pts << gg.Vec2{mx, my}
		app.ctrl_start << gg.Vec2{mx + 50, my}
		app.ctrl_end << gg.Vec2{mx - 50, my}
		app.count++
	}
	if e.typ == .mouse_down && e.mouse_button == .left {
		for i in 0 .. app.count {
			dx, dy := mx - app.pts[i].x, my - app.pts[i].y
			if math.sqrtf(dx * dx + dy * dy) <= 8 { app.sel = i }
		}
	}
	if e.typ == .mouse_up && e.mouse_button == .left { app.sel = -1 }
	if e.typ == .mouse_move && app.sel >= 0 {
		app.pts[app.sel] = gg.Vec2{mx, my}
	}
	if e.typ == .key_down {
		match e.key_code {
			._1 { app.kind = 0 }
			._2 { app.kind = 1 }
			._3 { app.kind = 2 }
			._4 { app.kind = 3 }
			.h { app.helpers = !app.helpers }
			else {}
		}
	}
}

fn main() {
	mut app := &App{}
	app.init_points()
	app.ctx = gg.new_context(
		width:        800
		height:       450
		window_title: 'raylib [shapes] example - splines drawing'
		user_data:    app
		frame_fn:     on_frame
		event_fn:     on_event
		bg_color:     raywhite
	)
	app.ctx.run()
}
