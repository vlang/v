// Port of raylib [shaders] example - vertex displacement.
// PORT NOTE: shader vertex_displacement.vs (Perlin-noise vertex displacement) requires gg
// user-shader support (not yet available); approximated with an animated field of triangles
// using immediate-mode ctx.begin_triangles/vertex3/end_batch.
module main

import gg
import math

const win_width = 800
const win_height = 450

struct App {
mut:
	gg   &gg.Context = unsafe { nil }
	cam  gg.Camera3D
	time f32
}

fn main() {
	mut app := &App{}
	app.cam = gg.Camera3D{ position: gg.Vec3{20, 5, -20}, target: gg.Vec3{}, fovy: 60 }
	app.cam.orbit(30, 15, 30)
	app.gg = gg.new_context(
		width:         win_width
		height:        win_height
		window_title:  'raylib [shaders] example - vertex displacement'
		bg_color:      gg.white
		depth:         true
		user_data:     app
		update_fn:     update
		frame_fn:      frame
		create_window: true
	)
	app.gg.run()
}

fn update(dt f32, data voidptr) {
	mut app := unsafe { &App(data) }
	app.time += dt
	app.cam.update(.free, app.gg, dt)
}

fn height_at(x f32, z f32, t f32) f32 {
	return math.sinf(x * 0.5 + t) * math.cosf(z * 0.5 + t * 0.7) * 1.2
}

fn frame(data voidptr) {
	app := unsafe { &App(data) }
	ctx := app.gg
	ctx.begin()
	ctx.set_camera(app.cam)
	ctx.begin_3d() or { panic(err.msg()) }
	// Displaced grid rendered as immediate-mode triangles.
	ctx.begin_triangles()
	step := f32(1.6)
	for x in -15 .. 15 {
		for z in -15 .. 15 {
			fx, fz := f32(x) * step, f32(z) * step
			p0 := gg.Vec3{fx, height_at(fx, fz, app.time), fz}
			p1 := gg.Vec3{fx + step, height_at(fx + step, fz, app.time), fz}
			p2 := gg.Vec3{fx + step, height_at(fx + step, fz + step, app.time), fz + step}
			p3 := gg.Vec3{fx, height_at(fx, fz + step, app.time), fz + step}
			c := gg.rgb(60, 130, 90)
			ctx.vertex3(p0.x, p0.y, p0.z, c)
			ctx.vertex3(p2.x, p2.y, p2.z, c)
			ctx.vertex3(p1.x, p1.y, p1.z, c)
			ctx.vertex3(p0.x, p0.y, p0.z, c)
			ctx.vertex3(p3.x, p3.y, p3.z, c)
			ctx.vertex3(p2.x, p2.y, p2.z, c)
		}
	}
	ctx.end_batch()
	ctx.end_3d()
	ctx.draw_text(10, 10, 'Vertex displacement (CPU / immediate-mode approximation)',
		size:  18
		color: gg.dark_gray
	)
	ctx.end()
}
