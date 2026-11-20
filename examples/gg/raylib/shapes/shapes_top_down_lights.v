module main

// raylib [shapes] example - top down lights, ported to gg.
// PORT NOTE: gg has no custom blend modes or offscreen light masks. Shadows are
// approximated by projecting box edges away from each light and drawing dark quads
// over a tiled checkerboard floor, with radial gradient light pools.
import gg
import math
import rand

const max_boxes = 20
const max_lights = 16
const black_c = gg.Color{0, 0, 0, 255}
const darkbrown = gg.Color{76, 63, 42, 255}
const darkgray = gg.Color{80, 80, 80, 255}
const green_c = gg.Color{0, 158, 47, 255}
const darkgreen = gg.Color{0, 117, 44, 255}
const yellow_c = gg.Color{253, 249, 0, 255}
const white_c = gg.Color{255, 255, 255, 255}
const purple = gg.Color{211, 22, 197, 255}
const darkblue = gg.Color{0, 82, 172, 255}

struct Light {
mut:
	active bool
	x      f32
	y      f32
	radius f32
}

struct Box {
mut:
	x f32
	y f32
	w f32
	h f32
}

struct App {
mut:
	ctx        &gg.Context = unsafe { nil }
	boxes      []Box
	lights     []Light
	next       int = 1
	show_lines bool
}

fn on_frame(data voidptr) {
	mut app := unsafe { &App(data) }
	ctx := app.ctx
	ctx.begin()
	// Checkerboard floor
	for y in 0 .. 14 {
		for x in 0 .. 25 {
			c := if (x + y) % 2 == 0 { darkbrown } else { darkgray }
			ctx.draw_rect_filled(f32(x) * 32, f32(y) * 32, 32, 32, c)
		}
	}
	// Light pools
	for li in 0 .. app.lights.len {
		l := app.lights[li]
		if !l.active { continue }
		mut r := l.radius
		for r > 0 {
			t := 1 - r / l.radius
			c := gg.Color{255, 255, 255, u8(t * 180)}
			ctx.draw_circle_filled(l.x, l.y, r, c)
			r -= 8
		}
		// Projected shadows from nearby boxes
		for b in app.boxes {
			dx, dy := b.x - l.x, b.y - l.y
			dist := math.sqrtf(dx * dx + dy * dy)
			if dist > l.radius * 2 || dist < 1 { continue }
			ux, uy := dx / dist, dy / dist
			len := l.radius * 2
			ctx.draw_triangle_filled(b.x, b.y, b.x + b.w, b.y, b.x + ux * len, b.y + uy * len,
				gg.Color{0, 0, 0, 90})
			ctx.draw_triangle_filled(b.x + b.w, b.y + b.h, b.x, b.y + b.h,
				b.x + b.w + ux * len, b.y + b.h + uy * len, gg.Color{0, 0, 0, 90})
		}
	}
	for b in app.boxes {
		ctx.draw_rect_filled(b.x, b.y, b.w, b.h, darkblue)
		if app.show_lines {
			ctx.draw_rect_empty(b.x, b.y, b.w, b.h, purple)
		}
	}
	for li in 0 .. app.lights.len {
		l := app.lights[li]
		if l.active {
			ctx.draw_circle_filled(l.x, l.y, 10, if li == 0 { yellow_c } else { white_c })
		}
	}
	hint := if app.show_lines { '(F1) Hide Shadow Volumes' } else { '(F1) Show Shadow Volumes' }
	ctx.draw_text(10, 50, hint, size: 10, color: green_c)
	ctx.draw_text(10, 10, 'Drag to move light #1', size: 10, color: darkgreen)
	ctx.draw_text(10, 30, 'Right click to add new light', size: 10, color: darkgreen)
	ctx.end()
}

fn on_event(e &gg.Event, data voidptr) {
	mut app := unsafe { &App(data) }
	if e.typ == .key_down && e.key_code == .f1 {
		app.show_lines = !app.show_lines
	}
	if e.typ == .mouse_down && e.mouse_button == .right && app.next < max_lights {
		app.lights[app.next] = Light{true, e.mouse_x, e.mouse_y, 200}
		app.next++
	}
}

fn on_update(dt f32, data voidptr) {
	mut app := unsafe { &App(data) }
	_ = dt
	if app.ctx.mouse_buttons.has(.left) {
		app.lights[0].x = f32(app.ctx.mouse_pos_x)
		app.lights[0].y = f32(app.ctx.mouse_pos_y)
	}
}

fn main() {
	mut app := &App{}
	app.boxes = [
		Box{150, 80, 40, 40},
		Box{1200, 700, 40, 40},
		Box{200, 600, 40, 40},
		Box{1000, 50, 40, 40},
		Box{500, 350, 40, 40},
	]!
	for _ in 5 .. max_boxes {
		app.boxes << Box{f32(rand.intn(800) or { 0 }), f32(rand.intn(450) or { 0 }), f32(10 + (rand.intn(90) or { 10 })), f32(10 + (rand.intn(90) or { 10 }))}
	}
	app.lights = []Light{len: max_lights}
	app.lights[0] = Light{true, 600, 400, 300}
	app.ctx = gg.new_context(
		width:        800
		height:       450
		window_title: 'raylib [shapes] example - top down lights'
		user_data:    app
		frame_fn:     on_frame
		event_fn:     on_event
		update_fn:    on_update
		bg_color:     black_c
	)
	app.ctx.run()
}
