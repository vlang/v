// Port of raylib [shaders] example - texture tiling.
// PORT NOTE: shader tiling.fs with a tiling uniform and repeat wrap requires gg user-shader
// support (not yet available); a grid of individually colored quads approximates the tiled cube.
module main

import gg

const win_width = 800
const win_height = 450
const tiles = 3

struct App {
mut:
	gg   &gg.Context = unsafe { nil }
	cam  gg.Camera3D
	cube gg.Mesh3D
}

fn main() {
	mut app := &App{}
	app.cam = gg.Camera3D{ position: gg.Vec3{4, 4, 4}, target: gg.Vec3{0, 0.5, 0}, fovy: 45 }
	app.cam.orbit(30, 20, 7)
	app.cube = gg.gen_mesh_cube(gg.Vec3{2, 2, 2})
	app.gg = gg.new_context(
		width:         win_width
		height:        win_height
		window_title:  'raylib [shaders] example - texture tiling'
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
	app.cam.update(.free, app.gg, dt)
}

fn frame(data voidptr) {
	app := unsafe { &App(data) }
	ctx := app.gg
	ctx.begin()
	ctx.set_camera(app.cam)
	ctx.begin_3d() or { panic(err.msg()) }
	// Tiled 3x3 pattern drawn as immediate-mode quads on the cube's top face.
	ctx.begin_triangles()
	for ty in 0 .. tiles {
		for tx in 0 .. tiles {
			c := if (tx + ty) % 2 == 0 { gg.rgb(210, 100, 80) } else { gg.rgb(80, 120, 210) }
			x0 := -1 + f32(tx) * 2 / tiles
			x1 := -1 + f32(tx + 1) * 2 / tiles
			z0 := -1 + f32(ty) * 2 / tiles
			z1 := -1 + f32(ty + 1) * 2 / tiles
			y := f32(1.01)
			ctx.vertex3(x0, y, z0, c)
			ctx.vertex3(x1, y, z1, c)
			ctx.vertex3(x1, y, z0, c)
			ctx.vertex3(x0, y, z0, c)
			ctx.vertex3(x0, y, z1, c)
			ctx.vertex3(x1, y, z1, c)
		}
	}
	ctx.end_batch()
	mat := gg.Material3D{ color: gg.rgb(180, 180, 180), ambient: 0.6 }
	ctx.draw_material_mesh(&app.cube, gg.Vec3{}, gg.Vec3{}, 1, &mat, app.cam)
	ctx.draw_grid(10, 1, gg.gray)
	ctx.end_3d()
	ctx.draw_text(10, 10, 'Use mouse to rotate the camera (tiled faces approximated)',
		size:  16
		color: gg.dark_gray
	)
	ctx.end()
}
