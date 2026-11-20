module main

// raylib [shapes] example - outlines testbed, ported to gg.
// PORT NOTE: raygui sliders replaced by keyboard controls; world/screen thickness
// distinction is approximated by scaling the outline width with zoom.
import gg
import math

const size = f32(46)
const spacing = f32(14)
const bg_col = gg.Color{50, 50, 55, 255}
const filled = gg.Color{0, 82, 172, 255}
const outline_c = gg.Color{255, 255, 0, 130}
const orange_c = gg.Color{255, 161, 0, 255}
const panel = gg.Color{80, 80, 80, 190}

fn poly(cx f32, cy f32, radius f32, edges int) []f32 {
	mut pts := []f32{}
	for i in 0 .. edges {
		a := f32(math.tau) * f32(i) / f32(edges)
		pts << cx + radius * math.cosf(a)
		pts << cy + radius * math.sinf(a)
	}
	return pts
}

fn draw_shapes(ctx &gg.Context, ox f32, oy f32, thick int, roundness f32, sides int) {
	pad := size + spacing
	for col in 0 .. 8 {
		x := ox + 20 + f32(col) * pad
		y := oy + 30
		cx, cy := x + size / 2, y + size / 2
		match col {
			0 {
				ctx.draw_rect_filled(x, y, size, size, filled)
				for i in 0 .. thick {
					ctx.draw_rect_empty(x - f32(i), y - f32(i), size + f32(i) * 2, size + f32(i) * 2, outline_c)
				}
			}
			1 {
				ctx.draw_rounded_rect_filled(x, y, size, size, size * roundness, filled)
				for i in 0 .. thick {
					ctx.draw_rounded_rect_empty(x - f32(i), y - f32(i), size + f32(i) * 2, size + f32(i) * 2, size * roundness, outline_c)
				}
			}
			2 {
				ctx.draw_circle_filled(cx, cy, size / 2, filled)
				for i in 0 .. thick { ctx.draw_circle_empty(cx, cy, size / 2 - f32(i), outline_c) }
			}
			3 {
				ctx.draw_ellipse_filled(cx, cy, size / 2, size / 4, filled)
				for i in 0 .. thick {
					ctx.draw_ellipse_empty(cx, cy, size / 2 - f32(i), size / 4 - f32(i), outline_c)
				}
			}
			4 {
				ctx.draw_slice_filled(cx, cy, size / 2, f32(math.radians(70)), f32(math.radians(-180)), 36, filled)
				for i in 0 .. thick {
					ctx.draw_slice_empty(cx, cy, size / 2 - f32(i), f32(math.radians(70)), f32(math.radians(-180)), 36, outline_c)
				}
			}
			5 {
				ctx.draw_arc_filled(cx, cy, size / 4, size / 4, f32(math.radians(70)), f32(math.radians(-180)), 36, filled)
				for i in 0 .. thick {
					ctx.draw_arc_empty(cx, cy, size / 4, size / 4 - f32(i), f32(math.radians(70)), f32(math.radians(-180)), 36, outline_c)
				}
			}
			6 {
				ctx.draw_triangle_filled(x, y + size * 0.8, x + size * 0.6, y + size, x + size, y, filled)
				for i in 0 .. thick {
					ctx.draw_triangle_empty(x - f32(i), y + size * 0.8, x + size * 0.6, y + size + f32(i), x + size + f32(i), y - f32(i), outline_c)
				}
			}
			7 {
				ctx.draw_convex_poly(poly(cx, cy, size / 2, sides), filled)
				for i in 0 .. thick {
					ctx.draw_poly_empty(poly(cx, cy, size / 2 - f32(i), sides), outline_c)
				}
			}
			else {}
		}
	}
}

struct App {
mut:
	ctx       &gg.Context = unsafe { nil }
	pan_x     f32
	pan_y     f32
	zoom      f32 = 1
	thick     int = 2
	roundness f32 = 0.4
	sides     int = 6
}

fn on_frame(data voidptr) {
	mut app := unsafe { &App(data) }
	ctx := app.ctx
	ctx.begin()
	draw_shapes(ctx, app.pan_x, app.pan_y, app.thick, app.roundness, app.sides)
	draw_shapes(ctx, app.pan_x, app.pan_y + 110, int(f32(app.thick) / app.zoom), app.roundness, app.sides)
	ctx.draw_rect_filled(510, 0, 290, 450, panel)
	ctx.draw_text(520, 10, 'Outlines testbed', size: 16, color: orange_c)
	ctx.draw_text(520, 40, 'Thickness ${app.thick} [Q/W]', size: 14, color: orange_c)
	ctx.draw_text(520, 65, 'Roundness ${app.roundness:.2f} [E/R]', size: 14, color: orange_c)
	ctx.draw_text(520, 90, 'Polygon sides ${app.sides} [T/Y]', size: 14, color: orange_c)
	ctx.draw_text(520, 115, 'Pan: WASD / drag', size: 14, color: orange_c)
	ctx.draw_text(520, 140, 'Zoom: wheel / Z/X', size: 14, color: orange_c)
	ctx.draw_text(520, 165, 'Reset: C', size: 14, color: orange_c)
	ctx.end()
}

fn on_event(e &gg.Event, data voidptr) {
	mut app := unsafe { &App(data) }
	if e.typ == .mouse_scroll { app.zoom += e.scroll_y * 0.1 }
	if e.typ == .key_down {
		match e.key_code {
			.q {
				if app.thick > 1 { app.thick-- }
			}
			.w { app.thick++ }
			.e {
				if app.roundness > 0 { app.roundness -= 0.1 }
			}
			.r {
				if app.roundness < 1 { app.roundness += 0.1 }
			}
			.t {
				if app.sides > 3 { app.sides-- }
			}
			.y { app.sides++ }
			.z {
				if app.zoom > 0.2 { app.zoom -= 0.1 }
			}
			.x { app.zoom += 0.1 }
			.c {
				app.pan_x = 0
				app.pan_y = 0
				app.zoom = 1
			}
			else {}
		}
	}
}

fn on_update(dt f32, data voidptr) {
	mut app := unsafe { &App(data) }
	_ = dt
	k := app.ctx.pressed_keys
	if k[gg.KeyCode.a] { app.pan_x += 10 }
	if k[gg.KeyCode.d] { app.pan_x -= 10 }
	if k[gg.KeyCode.s] { app.pan_y -= 10 }
	if k[gg.KeyCode.w] { app.pan_y += 10 }
	if app.zoom < 0.1 { app.zoom = 0.1 }
}

fn main() {
	mut app := &App{}
	app.ctx = gg.new_context(
		width:        800
		height:       450
		window_title: 'raylib [shapes] example - outlines testbed'
		user_data:    app
		frame_fn:     on_frame
		event_fn:     on_event
		update_fn:    on_update
		bg_color:     bg_col
	)
	app.ctx.run()
}
