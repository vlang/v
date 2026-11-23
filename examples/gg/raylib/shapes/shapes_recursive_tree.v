module main

// raylib [shapes] example - recursive tree, ported to gg.
// PORT NOTE: raygui sliders replaced by keyboard controls; bezier branches use gg cubic Bézier.
import gg
import math

const raywhite = gg.Color{245, 245, 245, 255}
const red_c = gg.Color{230, 41, 55, 255}
const black_c = gg.Color{0, 0, 0, 255}

struct Branch {
mut:
	x1    f32
	y1    f32
	x2    f32
	y2    f32
	angle f32
	len   f32
}

struct App {
mut:
	ctx      &gg.Context = unsafe { nil }
	angle    f32         = 40
	length   f32         = 120
	decay    f32         = 0.66
	depth    int         = 10
	thick    f32         = 1
	bezier   bool
	branches []Branch
}

fn (mut app App) rebuild() {
	app.branches = []Branch{cap: 1030}
	app.branches << Branch{275, 450, 275, 450 - app.length, 0, app.length}
	theta := f32(math.radians(app.angle))
	mut i := 0
	for i < app.branches.len && i < 1030 {
		b := app.branches[i]
		nl := b.len * app.decay
		if b.len >= 2 && nl >= 2 && app.branches.len < 1030 {
			a1 := b.angle + theta
			app.branches << Branch{b.x2, b.y2, b.x2 + nl * math.sinf(a1), b.y2 - nl * math.cosf(a1), a1, nl}
			a2 := b.angle - theta
			app.branches << Branch{b.x2, b.y2, b.x2 + nl * math.sinf(a2), b.y2 - nl * math.cosf(a2), a2, nl}
		}
		i++
	}
}

fn on_frame(data voidptr) {
	mut app := unsafe { &App(data) }
	ctx := app.ctx
	ctx.begin()
	for b in app.branches {
		if b.len >= 2 {
			if app.bezier {
				ctx.draw_cubic_bezier([
					b.x1,
					b.y1,
					b.x1,
					(b.y1 + b.y2) / 2,
					b.x2,
					(b.y1 + b.y2) / 2,
					b.x2,
					b.y2,
				], red_c)
			} else {
				ctx.draw_line(b.x1, b.y1, b.x2, b.y2, red_c)
			}
		}
	}
	ctx.draw_text(640, 30, 'Angle ${int(app.angle)} [A/Z]', size: 14, color: black_c)
	ctx.draw_text(640, 60, 'Length ${int(app.length)} [S/X]', size: 14, color: black_c)
	ctx.draw_text(640, 90, 'Decay ${app.decay:.2f} [D/C]', size: 14, color: black_c)
	ctx.draw_text(640, 120, 'Depth ${app.depth} [F/V]', size: 14, color: black_c)
	ctx.draw_text(640, 150, 'Bezier ${app.bezier} [B]', size: 14, color: black_c)
	ctx.end()
}

fn on_event(e &gg.Event, data voidptr) {
	mut app := unsafe { &App(data) }
	if e.typ != .key_down { return }
	match e.key_code {
		.a { app.angle += 5 }
		.z { app.angle -= 5 }
		.s { app.length += 10 }
		.x {
			if app.length > 12 { app.length -= 10 }
		}
		.d {
			if app.decay < 0.78 { app.decay += 0.02 }
		}
		.c {
			if app.decay > 0.1 { app.decay -= 0.02 }
		}
		.f {
			if app.depth < 10 { app.depth++ }
		}
		.v {
			if app.depth > 1 { app.depth-- }
		}
		.b { app.bezier = !app.bezier }
		else {}
	}
	app.rebuild()
}

fn main() {
	mut app := &App{}
	app.rebuild()
	app.ctx = gg.new_context(
		width:        800
		height:       450
		window_title: 'raylib [shapes] example - recursive tree'
		user_data:    app
		frame_fn:     on_frame
		event_fn:     on_event
		bg_color:     raywhite
	)
	app.ctx.run()
}
