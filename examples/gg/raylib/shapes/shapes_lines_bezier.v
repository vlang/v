module main

// raylib [shapes] example - lines bezier, ported to gg.
import gg
import math

const raywhite = gg.Color{245, 245, 245, 255}
const gray_c = gg.Color{130, 130, 130, 255}
const blue_c = gg.Color{0, 121, 241, 255}
const red_c = gg.Color{230, 41, 55, 255}

struct App {
mut:
	ctx        &gg.Context = unsafe { nil }
	sx         f32         = 30
	sy         f32         = 30
	ex         f32         = 770
	ey         f32         = 420
	move_start bool
	move_end   bool
}

fn on_frame(data voidptr) {
	mut app := unsafe { &App(data) }
	ctx := app.ctx
	ctx.begin()
	ctx.draw_text(15, 20, 'MOVE START-END POINTS WITH MOUSE', size: 20, color: gray_c)

	// PORT NOTE: raylib's DrawLineBezier is an in-out interpolation curve;
	// gg's cubic Bézier with control points at 1/3 and 2/3 gives the same S-curve.
	dx, dy := app.ex - app.sx, app.ey - app.sy
	ctx.draw_cubic_bezier([
		app.sx,
		app.sy,
		app.sx + dx / 3,
		app.sy + dy / 3,
		app.sx + dx * 2 / 3,
		app.sy + dy * 2 / 3,
		app.ex,
		app.ey,
	], blue_c)

	mx, my := f32(ctx.mouse_pos_x), f32(ctx.mouse_pos_y)
	hover_s := math.sqrtf((mx - app.sx) * (mx - app.sx) + (my - app.sy) * (my - app.sy)) <= 10
	hover_e := math.sqrtf((mx - app.ex) * (mx - app.ex) + (my - app.ey) * (my - app.ey)) <= 10
	rs := if hover_s { f32(14) } else { f32(8) }
	re := if hover_e { f32(14) } else { f32(8) }
	ctx.draw_circle_filled(app.sx, app.sy, rs, if app.move_start { red_c } else { blue_c })
	ctx.draw_circle_filled(app.ex, app.ey, re, if app.move_end { red_c } else { blue_c })
	ctx.end()
}

fn on_event(e &gg.Event, data voidptr) {
	mut app := unsafe { &App(data) }
	mx, my := f32(e.mouse_x), f32(e.mouse_y)
	if e.typ == .mouse_down && e.mouse_button == .left {
		if math.sqrtf((mx - app.sx) * (mx - app.sx) + (my - app.sy) * (my - app.sy)) <= 10 {
			app.move_start = true
		} else if math.sqrtf((mx - app.ex) * (mx - app.ex) + (my - app.ey) * (my - app.ey)) <= 10 {
			app.move_end = true
		}
	}
	if e.typ == .mouse_up && e.mouse_button == .left {
		app.move_start = false
		app.move_end = false
	}
	if e.typ == .mouse_move {
		if app.move_start {
			app.sx, app.sy = mx, my
		}
		if app.move_end {
			app.ex, app.ey = mx, my
		}
	}
}

fn main() {
	mut app := &App{}
	app.ctx = gg.new_context(
		width:        800
		height:       450
		window_title: 'raylib [shapes] example - lines bezier'
		user_data:    app
		frame_fn:     on_frame
		event_fn:     on_event
		bg_color:     raywhite
	)
	app.ctx.run()
}
