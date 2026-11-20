module main

import gg

const win_width = 640
const win_height = 480

struct App {
mut:
	gg    &gg.Context = unsafe { nil }
	angle f32
	mesh  gg.Mesh3D
}

fn main() {
	mut app := &App{}
	app.mesh = gg.gen_mesh_cube(gg.Vec3{1, 1, 1})
	app.gg = gg.new_context(
		width:        win_width
		height:       win_height
		window_title: 'gg 3D cube'
		bg_color:     gg.rgb(20, 24, 32)
		depth:        true
		frame_fn:     frame
		update_fn:    update
		user_data:    app
	)
	app.gg.run()
}

fn update(dt f32, data voidptr) {
	mut app := unsafe { &App(data) }
	app.angle += dt * 40.0
}

fn frame(data voidptr) {
	app := unsafe { &App(data) }
	ctx := app.gg
	ctx.begin()
	mut cam := gg.Camera3D{}
	cam.orbit(app.angle, 20.0, 7.0)
	ctx.begin_3d(cam)
	ctx.draw_grid(20, 1.0, gg.gray)
	ctx.draw_cube(gg.Vec3{-1.2, 0, 0}, gg.Vec3{2, 2, 2}, gg.red)
	ctx.draw_cube(gg.Vec3{1.2, 0, 0}, gg.Vec3{2, 2, 2}, gg.blue)
	ctx.draw_sphere(gg.Vec3{0, 2.2, 0}, 0.9, 8, 16, gg.green)
	ctx.draw_cylinder(gg.Vec3{-3, -1, 0}, gg.Vec3{-3, 1, 0}, 0.4, 12, gg.yellow)
	ctx.draw_mesh(&app.mesh, gg.Vec3{0, 3, 0}, gg.Vec3{0, 45, 0}, 1.0,
		gg.rgb(255, 0, 255))
	ctx.draw_cube_wires(gg.Vec3{0, -1.6, 0}, gg.Vec3{2, 0.4, 2}, gg.white)
	ctx.end_3d()
	ctx.end()
}
