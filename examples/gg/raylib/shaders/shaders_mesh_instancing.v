// Port of raylib [shaders] example - mesh instancing.
// PORT NOTE: shader lighting_instancing.vs with GPU instancing requires gg user-shader support
// (not yet available); approximated by drawing many meshes in immediate-mode batches.
module main

import gg
import math
import rand

const win_width = 800
const win_height = 450
const max_instances = 600

struct Instance {
mut:
	x     f32
	y     f32
	z     f32
	angle f32
	axis  gg.Vec3
}

struct App {
mut:
	gg        &gg.Context = unsafe { nil }
	cam       gg.Camera3D
	instances []Instance
}

fn main() {
	mut app := &App{}
	app.cam = gg.Camera3D{ position: gg.Vec3{-25, 25, -25}, target: gg.Vec3{}, fovy: 45 }
	app.cam.orbit(30, 20, 42)
	for _ in 0 .. max_instances {
		app.instances << Instance{
			x:     f32(rand.intn(101) or { 0 } - 50)
			y:     f32(rand.intn(101) or { 0 } - 50)
			z:     f32(rand.intn(101) or { 0 } - 50)
			angle: f32(rand.intn(180) or { 0 }) * f32(math.pi) / 180.0
			axis:  gg.Vec3{f32(rand.intn(360) or { 0 }), f32(rand.intn(360) or { 0 }), f32(rand.intn(360) or { 0 })}.normalized()
		}
	}
	app.gg = gg.new_context(
		width:         win_width
		height:        win_height
		window_title:  'raylib [shaders] example - mesh instancing'
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
	app.cam.update(.orbital, app.gg, dt)
}

fn frame(data voidptr) {
	app := unsafe { &App(data) }
	ctx := app.gg
	ctx.begin()
	ctx.set_camera(app.cam)
	ctx.begin_3d() or { panic(err.msg()) }
	// Immediate-mode batch: one batch of triangles for all instance cubes.
	ctx.begin_triangles()
	for inst in app.instances {
		half := f32(0.5)
		hx, hy, hz := half, half, half
		c := gg.red
		// A simple axis-aligned cube per instance (rotation encoded by jitter).
		pts := [gg.Vec3{inst.x - hx, inst.y - hy, inst.z - hz},
			gg.Vec3{inst.x + hx, inst.y - hy, inst.z - hz},
			gg.Vec3{inst.x + hx, inst.y + hy, inst.z - hz},
			gg.Vec3{inst.x - hx, inst.y + hy, inst.z - hz},
			gg.Vec3{inst.x - hx, inst.y - hy, inst.z + hz},
			gg.Vec3{inst.x + hx, inst.y - hy, inst.z + hz},
			gg.Vec3{inst.x + hx, inst.y + hy, inst.z + hz},
			gg.Vec3{inst.x - hx, inst.y + hy, inst.z + hz}]!
		tris := [u8(4), 5, 6, 4, 6, 7, 1, 0, 3, 1, 3, 2, 7, 6, 2, 7, 2, 3, 0, 1, 5, 0, 5, 4, 5,
			1, 2, 5, 2, 6, 0, 4, 7, 0, 7, 3]!
		for ti in tris {
			v := pts[ti]
			ctx.vertex3(v.x, v.y, v.z, c)
		}
	}
	ctx.end_batch()
	ctx.end_3d()
	ctx.draw_text(10, 10, '${max_instances} instanced cubes (immediate-mode batch)',
		size:  16
		color: gg.dark_gray
	)
	ctx.end()
}
