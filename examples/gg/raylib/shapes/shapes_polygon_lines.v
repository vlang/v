module main

// raylib [shapes] example - polygon lines, ported to gg.
// PORT NOTE: gg outlines are 1px; thickness is approximated by nested polygons.
import gg
import math

const raywhite = gg.Color{245, 245, 245, 255}
const lime = gg.Color{1, 222, 38, 255}
const maroon = gg.Color{190, 33, 55, 255}
const lightgray = gg.Color{200, 200, 200, 255}
const red_c = gg.Color{230, 41, 55, 255}
const blue_c = gg.Color{0, 121, 241, 255}
const black_c = gg.Color{0, 0, 0, 255}

fn poly(cx f32, cy f32, size f32, edges int, rot f32) []f32 {
	mut pts := []f32{}
	r := f32(math.radians(rot))
	for i in 0 .. edges {
		a := f32(math.tau) * f32(i) / f32(edges) + r
		pts << cx + size * math.cosf(a)
		pts << cy + size * math.sinf(a)
	}
	return pts
}

fn thick_poly(ctx &gg.Context, cx f32, cy f32, size f32, edges int, rot f32, t int, c gg.Color) {
	for i in 0 .. t {
		ctx.draw_poly_empty(poly(cx, cy, size - f32(i), edges, rot), c)
	}
}

fn on_frame(data voidptr) {
	mut app := unsafe { &App(data) }
	ctx := app.ctx
	ctx.begin()
	ctx.draw_text(10, 280, 'thick = ${app.thick}', size: 20, color: lime)
	ctx.draw_text(10, 10, 'These should look identical!', size: 20, color: maroon)
	ctx.draw_rect_filled(10, 40, 50, 50, lightgray)
	ctx.draw_circle_filled(95, 65, 25, lightgray)
	ctx.draw_text(130, 60, 'DrawRectangleLinesEx() and DrawCircleLinesEx()',
		size:  10
		color: black_c
	)
	thick_poly(ctx, 35, 125, 35.355, 4, 45, app.thick, red_c)
	thick_poly(ctx, 95, 125, 25, 36, 0, app.thick, red_c)
	ctx.draw_text(130, 120, 'DrawPolyLinesEx()', size: 10, color: black_c)

	xc := [f32(290), 430, 570, 710, 290, 430, 570, 710]
	yc := [f32(220), 220, 220, 220, 360, 360, 360, 360]
	ec := [8, 4, 5, 6, 7, 8, 9, 10]
	for i in 0 .. 8 {
		ctx.draw_convex_poly(poly(xc[i], yc[i], 60, ec[i], app.rotation), lightgray)
		thick_poly(ctx, xc[i], yc[i], 60, ec[i], app.rotation, app.thick, blue_c)
	}
	ctx.end()
}

struct App {
mut:
	ctx      &gg.Context = unsafe { nil }
	thick    int         = 2
	rotation f32
}

fn on_event(e &gg.Event, data voidptr) {
	mut app := unsafe { &App(data) }
	if e.typ == .key_down {
		match e.key_code {
			.up { app.thick++ }
			.down {
				if app.thick > 1 { app.thick-- }
			}
			else {}
		}
		if app.thick > 60 { app.thick = 60 }
		if app.thick < 1 { app.thick = 1 }
	}
}

fn on_update(dt f32, data voidptr) {
	mut app := unsafe { &App(data) }
	app.rotation += 36 * dt
}

fn main() {
	mut app := &App{}
	app.ctx = gg.new_context(
		width:        800
		height:       450
		window_title: 'raylib [shapes] example - polygon lines'
		user_data:    app
		frame_fn:     on_frame
		event_fn:     on_event
		update_fn:    on_update
		bg_color:     raywhite
	)
	app.ctx.run()
}
