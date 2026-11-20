module main

// Port of raylib models_point_rendering.c
// PORT NOTE: gg has no GL point-mode or point-size API (rlEnablePointMode).
// Points are drawn as tiny cubes; the count is capped to keep the port usable.
import gg
import math
import rand

const pr_black = gg.Color{0, 0, 0, 255}
const pr_green = gg.Color{0, 255, 0, 255}
const pr_red = gg.Color{255, 0, 0, 255}

const max_points = 10000

struct Point {
mut:
	pos   gg.Vec3
	color gg.Color
}

struct App {
mut:
	ctx      &gg.Context = unsafe { nil }
	cam      gg.Camera3D
	points   []Point
	num      int = 1000
	use_alt  bool
}

fn make_points(n int) []Point {
	mut pts := []Point{len: n}
	for i in 0 .. n {
		theta := f32(math.pi) * f32(rand.intn(10000) or { 5000 }) / 10000.0
		phi := 2 * f32(math.pi) * f32(rand.intn(10000) or { 5000 }) / 10000.0
		r := 10.0 * f32(rand.intn(10000) or { 5000 }) / 10000.0
		h := r * 36
		hue := int(h) % 360
		c := hsv(hue, 0.8, 0.9)
		pts[i] = Point{
			pos:   gg.Vec3{r * f32(math.sin(f64(theta))) * f32(math.cos(f64(phi))),
				r * f32(math.sin(f64(theta))) * f32(math.sin(f64(phi))), r * f32(math.cos(f64(theta)))}
			color: c
		}
	}
	return pts
}

fn hsv(h int, s f32, v f32) gg.Color {
	hf := f32(h) / 60.0
	c := v * s
	x := c * (1 - math.abs((hf % 2.0) - 1))
	m := v - c
	mut r := f32(0)
	mut g := f32(0)
	mut b := f32(0)
	match int(hf) {
		0 {
			r = c
			g = x
		}
		1 {
			r = x
			g = c
		}
		2 {
			g = c
			b = x
		}
		3 {
			g = x
			b = c
		}
		4 {
			r = x
			b = c
		}
		else {
			r = c
			b = x
		}
	}
	return gg.Color{u8((r + m) * 255), u8((g + m) * 255), u8((b + m) * 255), 255}
}

fn init(data voidptr) {
	mut app := unsafe { &App(data) }
	app.cam = gg.Camera3D{
		position: gg.Vec3{3, 3, 3}
		target:   gg.Vec3{}
		fovy:     45
	}
	app.points = make_points(app.num)
}

fn frame(data voidptr) {
	mut app := unsafe { &App(data) }
	app.ctx.begin()
	app.ctx.set_camera(app.cam)
	app.ctx.begin_3d(cam: app.cam) or {
		app.ctx.end()
		return
	}
	sz := if app.use_alt { 0.06 } else { 0.08 }
	for p in app.points {
		app.ctx.draw_cube(p.pos, gg.Vec3{sz, sz, sz}, p.color)
	}
	app.ctx.draw_sphere_wires(gg.Vec3{}, 1, 10, 10, gg.Color{255, 255, 0, 255})
	app.ctx.end_3d()
	app.ctx.draw_text(10, 330, 'Point Count: ${app.num} (capped at ${max_points})',
		size: 30, color: gg.white)
	app.ctx.draw_text(10, 40, 'UP - Increase points', size: 20, color: gg.white)
	app.ctx.draw_text(10, 70, 'DOWN - Decrease points', size: 20, color: gg.white)
	app.ctx.draw_text(10, 100, 'SPACE - Drawing function', size: 20, color: gg.white)
	if app.use_alt {
		app.ctx.draw_text(10, 130, 'Using: small cubes (point mode unavailable)',
			size: 20, color: pr_red)
	} else {
		app.ctx.draw_text(10, 130, 'Using: small cubes', size: 20, color: pr_green)
	}
	app.ctx.end()
}

fn on_event(e &gg.Event, data voidptr) {
	mut app := unsafe { &App(data) }
	if e.typ != .key_down {
		return
	}
	match e.key_code {
		.space { app.use_alt = !app.use_alt }
		.up {
			app.num *= 10
			if app.num > max_points { app.num = max_points }
			app.points = make_points(app.num)
		}
		.down {
			app.num /= 10
			if app.num < 1000 { app.num = 1000 }
			app.points = make_points(app.num)
		}
		else {}
	}
}

fn update(dt f32, data voidptr) {
	mut app := unsafe { &App(data) }
	app.cam.update(.orbital, app.ctx, dt)
}

fn main() {
	mut app := &App{}
	app.ctx = gg.new_context(
		width:        800
		height:       450
		window_title: 'raylib [models] example - point rendering'
		user_data:    app
		init_fn:      init
		frame_fn:     frame
		event_fn:     on_event
		update_fn:    update
		bg_color:     pr_black
		depth:        true
	)
	app.ctx.run()
}
