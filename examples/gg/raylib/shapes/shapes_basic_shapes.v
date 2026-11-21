module main

// raylib [shapes] example - basic shapes, ported to gg.
import gg
import math

const raywhite = gg.Color{245, 245, 245, 255}
const darkgray = gg.Color{80, 80, 80, 255}
const darkblue = gg.Color{0, 82, 172, 255}
const green_c = gg.Color{0, 158, 47, 255}
const skyblue = gg.Color{102, 191, 255, 255}
const yellow_c = gg.Color{253, 249, 0, 255}
const red_c = gg.Color{230, 41, 55, 255}
const maroon = gg.Color{190, 33, 55, 255}
const gold = gg.Color{255, 203, 0, 255}
const orange_c = gg.Color{255, 161, 0, 255}
const violet = gg.Color{135, 61, 190, 255}
const brown = gg.Color{127, 106, 79, 255}
const beige = gg.Color{211, 176, 131, 255}
const black_c = gg.Color{0, 0, 0, 255}

struct App {
mut:
	ctx      &gg.Context = unsafe { nil }
	rotation f32
}

fn on_frame(data voidptr) {
	mut app := unsafe { &App(data) }
	ctx := app.ctx
	ctx.begin()
	w := f32(ctx.width)

	ctx.draw_text(20, 20, 'some basic shapes available on raylib',
		size:  20
		color: darkgray
	)

	// Circle shapes and lines
	ctx.draw_circle_filled(w / 5, 120, 35, darkblue)
	// PORT NOTE: gg has no radial gradients; approximate the circle gradient with concentric rings.
	mut r := f32(60)
	for r > 0 {
		t := 1 - r / 60
		c := gg.Color{
			r: u8(f32(green_c.r) * (1 - t) + f32(skyblue.r) * t)
			g: u8(f32(green_c.g) * (1 - t) + f32(skyblue.g) * t)
			b: u8(f32(green_c.b) * (1 - t) + f32(skyblue.b) * t)
			a: 255
		}
		ctx.draw_circle_filled(w / 5, 220, r, c)
		r -= 2
	}
	ctx.draw_circle_empty(w / 5, 340, 80, darkblue)
	ctx.draw_ellipse_filled(w / 5, 120, 25, 20, yellow_c)
	ctx.draw_ellipse_empty(w / 5, 120, 30, 25, yellow_c)

	// Rectangle shapes and lines
	ctx.draw_rect_filled(w / 4 * 2 - 60, 100, 120, 60, red_c)
	// PORT NOTE: gg has no horizontal gradients; approximate with vertical strips.
	for i in 0 .. 36 {
		t := f32(i) / 35
		c := gg.Color{
			r: u8(f32(maroon.r) * (1 - t) + f32(gold.r) * t)
			g: u8(f32(maroon.g) * (1 - t) + f32(gold.g) * t)
			b: u8(f32(maroon.b) * (1 - t) + f32(gold.b) * t)
			a: 255
		}
		ctx.draw_rect_filled(w / 4 * 2 - 90 + f32(i) * 5, 170, 5, 130, c)
	}
	ctx.draw_rect_empty(w / 4 * 2 - 40, 320, 80, 60, orange_c)

	// Triangle shapes and lines
	ctx.draw_triangle_filled(w / 4 * 3, 80, w / 4 * 3 - 60, 150, w / 4 * 3 + 60, 150, violet)
	ctx.draw_triangle_empty(w / 4 * 3, 160, w / 4 * 3 - 20, 230, w / 4 * 3 + 20, 230, darkblue)

	// Polygon shapes and lines
	ctx.draw_polygon_filled(w / 4 * 3, 330, 80, 6, int(app.rotation), brown)
	ctx.draw_poly_empty(poly_points(w / 4 * 3, 330, 90, 6, app.rotation), brown)
	ctx.draw_poly_empty(poly_points(w / 4 * 3, 330, 85, 6, app.rotation), beige)

	ctx.draw_line(18, 42, w - 18, 42, black_c)
	ctx.end()
}

fn poly_points(cx f32, cy f32, radius f32, edges int, rotation f32) []f32 {
	mut pts := []f32{}
	rot := f32(math.radians(rotation))
	for i in 0 .. edges {
		a := f32(math.tau) * f32(i) / f32(edges) + rot
		pts << cx + radius * math.cosf(a)
		pts << cy + radius * math.sinf(a)
	}
	return pts
}

fn on_update(dt f32, data voidptr) {
	mut app := unsafe { &App(data) }
	app.rotation += 0.2 * dt * 60
}

fn main() {
	mut app := &App{}
	app.ctx = gg.new_context(
		width:        800
		height:       450
		window_title: 'raylib [shapes] example - basic shapes'
		user_data:    app
		frame_fn:     on_frame
		update_fn:    on_update
		bg_color:     raywhite
	)
	app.ctx.run()
}
