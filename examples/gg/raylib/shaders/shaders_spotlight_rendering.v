// Port of raylib [shaders] example - spotlight rendering.
// PORT NOTE: shader spotlight.fs requires gg user-shader support (not yet available);
// approximated with additive translucent circles and dark overlay rectangles for the spotlight mask.
module main

import gg
import math
import rand

const win_width = 800
const win_height = 450
const max_spots = 3
const max_stars = 400

struct Star {
mut:
	x  f32
	y  f32
	sx f32
	sy f32
}

struct Spot {
mut:
	x       f32
	y       f32
	speed_x f32
	speed_y f32
	inner   f32
	radius  f32
	color   gg.Color
}

struct App {
mut:
	gg    &gg.Context = unsafe { nil }
	stars []Star
	spots []Spot
	frame int
}

fn reset_star(mut s &Star) {
	s.x = f32(win_width) / 2
	s.y = f32(win_height) / 2
	s.sx = f32(rand.intn(2000) or { 100 } - 1000) / 100
	s.sy = f32(rand.intn(2000) or { 100 } - 1000) / 100
}

fn main() {
	mut app := &App{}
	for _ in 0 .. max_stars {
		mut s := Star{}
		reset_star(&s)
		app.stars << s
	}
	app.spots = [
		Spot{ inner: 28, radius: 48, color: gg.yellow },
		Spot{ inner: 56, radius: 96, color: gg.cyan },
		Spot{ inner: 84, radius: 144, color: gg.magenta },
	]
	for i in 0 .. max_spots {
		app.spots[i].x = f32(rand.intn(win_width - 128) or { 100 } + 64)
		app.spots[i].y = f32(rand.intn(win_height - 128) or { 100 } + 64)
		app.spots[i].speed_x = (f32(rand.intn(440) or { 200 }) - 400) / 25
		app.spots[i].speed_y = (f32(rand.intn(440) or { 200 }) - 400) / 25
	}
	app.gg = gg.new_context(
		width:         win_width
		height:        win_height
		window_title:  'raylib [shaders] example - spotlight rendering'
		bg_color:      gg.dark_blue
		user_data:     app
		update_fn:     update
		event_fn:      event
		frame_fn:      frame
		create_window: true
	)
	app.gg.run()
}

fn update(dt f32, data voidptr) {
	_ = dt
	mut app := unsafe { &App(data) }
	app.frame++
	for i in 0 .. app.stars.len {
		mut s := &app.stars[i]
		s.x += s.sx
		s.y += s.sy
		if s.x < 0 || s.x > win_width || s.y < 0 || s.y > win_height {
			reset_star(s)
		}
	}
	for i in 1 .. max_spots {
		mut sp := &app.spots[i]
		sp.x += sp.speed_x
		sp.y += sp.speed_y
		if sp.x < 64 || sp.x > win_width - 64 {
			sp.speed_x *= -1
		}
		if sp.y < 64 || sp.y > win_height - 64 {
			sp.speed_y *= -1
		}
	}
}

fn event(e &gg.Event, data voidptr) {
	mut app := unsafe { &App(data) }
	if app.spots.len > 0 {
		app.spots[0].x = e.mouse_x
		app.spots[0].y = e.mouse_y
	}
}

fn frame(data voidptr) {
	app := unsafe { &App(data) }
	ctx := app.gg
	ctx.begin()
	for s in app.stars {
		ctx.draw_rect_filled(int(s.x), int(s.y), 2, 2, gg.white)
	}
	for i in 0 .. 16 {
		x := f32(win_width) / 2 + math.cosf((f32(app.frame) + f32(i) * 8) / 51.45) * f32(win_width) / 2.2 - 32
		y := f32(win_height) / 2 + math.sinf((f32(app.frame) + f32(i) * 8) / 17.87) * f32(win_height) / 4.2
		ctx.draw_circle_filled(x, y, 8, gg.Color{255, 255, 255, 30})
	}
	// Dark mask with additive circles as the light pools.
	ctx.draw_rect_filled(0, 0, f32(win_width) / 2, f32(win_height), gg.Color{0, 0, 0, 220})
	for sp in app.spots {
		ctx.draw_circle_filled(sp.x, sp.y, sp.radius, gg.Color{sp.color.r, sp.color.g, sp.color.b, 40})
		ctx.draw_circle_filled(sp.x, sp.y, sp.inner, gg.Color{255, 255, 255, 60})
	}
	ctx.draw_text(10, 30, 'Move the mouse!', size: 20, color: gg.green)
	ctx.draw_text(int(f32(win_width) * 0.2), win_height / 2, 'Pitch Black',
		size:  20
		color: gg.green
	)
	ctx.draw_text(int(f32(win_width) * 0.66), win_height / 2, 'Dark', size: 20, color: gg.green)
	ctx.end()
}
