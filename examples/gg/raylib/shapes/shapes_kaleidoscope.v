module main

// raylib [shapes] example - kaleidoscope, ported to gg.
import gg
import math

const max_lines = 8192
const raywhite = gg.Color{245, 245, 245, 255}
const black_c = gg.Color{0, 0, 0, 255}
const maroon = gg.Color{190, 33, 55, 255}

struct Line {
mut:
	x1 f32
	y1 f32
	x2 f32
	y2 f32
}

struct App {
mut:
	ctx      &gg.Context = unsafe { nil }
	lines    []Line
	symmetry int = 6
	thick    f32 = 3
	count    int
	total    int
	prev_x   f32
	prev_y   f32
	mx       f32
	my       f32
}

fn on_frame(data voidptr) {
	mut app := unsafe { &App(data) }
	ctx := app.ctx
	ctx.begin()
	ox, oy := f32(ctx.width) / 2, f32(ctx.height) / 2
	for i in 0 .. app.count {
		l := app.lines[i]
		ctx.draw_line(l.x1 + ox, l.y1 + oy, l.x2 + ox, l.y2 + oy, black_c)
	}
	// PORT NOTE: raygui buttons replaced by simple text; symmetry with +/- keys.
	ctx.draw_text(ctx.width - 55, 5, 'Reset', size: 14, color: maroon)
	ctx.draw_text(ctx.width - 55, ctx.height - 30, '<', size: 14, color: maroon)
	ctx.draw_text(ctx.width - 30, ctx.height - 30, '>', size: 14, color: maroon)
	ctx.draw_text(10, ctx.height - 30,
		'LINES: ${app.count}/${max_lines}  symmetry=${app.symmetry} (+/-)',
		size:  20
		color: maroon
	)
	ctx.end()
}

fn on_event(e &gg.Event, data voidptr) {
	mut app := unsafe { &App(data) }
	if e.typ == .key_down {
		match e.key_code {
			.equal, .kp_add {
				if app.symmetry < 24 { app.symmetry++ }
			}
			.minus, .kp_subtract {
				if app.symmetry > 1 { app.symmetry-- }
			}
			.r {
				app.count = 0
				app.total = 0
			}
			else {}
		}
	}
	if e.typ == .mouse_move {
		app.prev_x, app.prev_y = app.mx, app.my
		app.mx, app.my = e.mouse_x, e.mouse_y
	}
}

fn on_update(dt f32, data voidptr) {
	mut app := unsafe { &App(data) }
	_ = dt
	if !app.ctx.mouse_buttons.has(.left) {
		return
	}
	mx, my := f32(app.ctx.mouse_pos_x), f32(app.ctx.mouse_pos_y)
	ox, oy := f32(app.ctx.width) / 2, f32(app.ctx.height) / 2
	mut sx, mut sy := mx - ox, my - oy
	mut ex, mut ey := app.prev_x, app.prev_y
	angle := f32(math.radians(360.0 / f32(app.symmetry)))
	ca, sa := math.cosf(angle), math.sinf(angle)
	for _ in 0 .. app.symmetry {
		if app.total >= max_lines - 1 {
			break
		}
		nsx, nsy := sx * ca - sy * sa, sx * sa + sy * ca
		nex, ney := ex * ca - ey * sa, ex * sa + ey * ca
		sx, sy, ex, ey = nsx, nsy, nex, ney
		for app.lines.len <= app.total + 1 {
			app.lines << Line{}
		}
		app.lines[app.total] = Line{sx, sy, ex, ey}
		app.lines[app.total + 1] = Line{-sx, sy, -ex, ey}
		app.total += 2
	}
	app.count = app.total
	app.prev_x, app.prev_y = mx, my
}

fn main() {
	mut app := &App{}
	app.ctx = gg.new_context(
		width:        800
		height:       450
		window_title: 'raylib [shapes] example - kaleidoscope'
		user_data:    app
		frame_fn:     on_frame
		event_fn:     on_event
		update_fn:    on_update
		bg_color:     raywhite
	)
	app.ctx.run()
}
