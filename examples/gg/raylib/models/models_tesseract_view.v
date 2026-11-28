module main

// Port of raylib models_tesseract_view.c
// The 4D-to-3D perspective projection is computed directly; edges are drawn
// between vertices that differ in exactly one of their four coordinates.
import gg
import math

const tv_raywhite = gg.Color{245, 245, 245, 255}
const tv_red = gg.Color{255, 0, 0, 255}
const tv_maroon = gg.Color{128, 0, 0, 255}

struct V4 {
mut:
	x f32
	y f32
	z f32
	w f32
}

struct App {
mut:
	ctx        &gg.Context = unsafe { nil }
	cam        gg.Camera3D
	vertices   [16]V4
	transformed [16]gg.Vec3
	wvalues    [16]f32
	time       f32
}

fn init(data voidptr) {
	mut app := unsafe { &App(data) }
	app.cam = gg.Camera3D{
		position: gg.Vec3{4, 4, 4}
		target:   gg.Vec3{}
		up:       gg.Vec3{0, 0, 1}
		fovy:     50
	}
	mut idx := 0
	for x in -1 .. 2 {
		for y in -1 .. 2 {
			for z in -1 .. 2 {
				for w in -1 .. 2 {
					app.vertices[idx] = V4{f32(x), f32(y), f32(z), f32(w)}
					idx++
				}
			}
		}
	}
}

fn frame(data voidptr) {
	mut app := unsafe { &App(data) }
	app.time += 1.0 / 60.0
	rot := f32(math.pi) / 4.0 * app.time
	for i in 0 .. 16 {
		p := app.vertices[i]
		// rotate XW
		nx := p.x * f32(math.cos(f64(rot))) - p.w * f32(math.sin(f64(rot)))
		nw := p.x * f32(math.sin(f64(rot))) + p.w * f32(math.cos(f64(rot)))
		c := 3.0 / (3.0 - nw)
		app.transformed[i] = gg.Vec3{c * nx, c * p.y, c * p.z}
		app.wvalues[i] = nw
	}
	app.ctx.begin()
	app.ctx.set_camera(app.cam)
	app.ctx.begin_3d(cam: app.cam) or {
		app.ctx.end()
		return
	}
	for i in 0 .. 16 {
		r := math.abs(f64(app.wvalues[i]) * 0.1)
		app.ctx.draw_sphere(app.transformed[i], f32(r), 8, 8, tv_red)
		for j in i + 1 .. 16 {
			diff := int(app.vertices[i].x == app.vertices[j].x) +
				int(app.vertices[i].y == app.vertices[j].y) +
				int(app.vertices[i].z == app.vertices[j].z) +
				int(app.vertices[i].w == app.vertices[j].w)
			if diff == 3 {
				app.ctx.draw_line_3d(app.transformed[i], app.transformed[j], tv_maroon)
			}
		}
	}
	app.ctx.end_3d()
	app.ctx.end()
}

fn main() {
	mut app := &App{}
	app.ctx = gg.new_context(
		width:        800
		height:       450
		window_title: 'raylib [models] example - tesseract view'
		user_data:    app
		init_fn:      init
		frame_fn:     frame
		bg_color:     tv_raywhite
		depth:        true
	)
	app.ctx.run()
}
