module main

// Port of raylib models_geometric_shapes.c
// PORT NOTE: gg has no capsule primitive. Capsules are approximated with a
// cylinder plus two spheres; all other shapes map to gg's 3D draw helpers.
import gg

const gs_raywhite = gg.Color{245, 245, 245, 255}
const gs_gold = gg.Color{255, 203, 40, 255}
const gs_maroon = gg.Color{128, 0, 0, 255}
const gs_magenta = gg.Color{255, 0, 255, 255}
const gs_purple = gg.Color{138, 43, 226, 255}
const gs_skyblue = gg.Color{135, 206, 235, 255}
const gs_darkblue = gg.Color{0, 42, 141, 255}
const gs_brown = gg.Color{165, 42, 42, 255}
const gs_pink = gg.Color{255, 109, 194, 255}
const gs_lime = gg.Color{0, 255, 0, 255}

struct App {
mut:
	ctx      &gg.Context = unsafe { nil }
	cam      gg.Camera3D
	cone     gg.Mesh3D
}

fn init(data voidptr) {
	mut app := unsafe { &App(data) }
	app.cam = gg.Camera3D{
		position: gg.Vec3{0, 10, 10}
		target:   gg.Vec3{}
		fovy:     45
	}
	app.cone = gg.gen_mesh_cone(gg.Vec3{1, 0, -4}, gg.Vec3{1, 3, -4}, 1.5, 8)
}

fn frame(data voidptr) {
	mut app := unsafe { &App(data) }
	app.ctx.begin()
	app.ctx.set_camera(app.cam)
	app.ctx.begin_3d(cam: app.cam) or {
		app.ctx.end()
		return
	}
	app.ctx.draw_cube(gg.Vec3{-4, 0, 2}, gg.Vec3{2, 5, 2}, gg.red)
	app.ctx.draw_cube_wires(gg.Vec3{-4, 0, 2}, gg.Vec3{2, 5, 2}, gs_gold)
	app.ctx.draw_cube_wires(gg.Vec3{-4, 0, -2}, gg.Vec3{3, 6, 2}, gs_maroon)

	app.ctx.draw_sphere(gg.Vec3{-1, 0, -2}, 1, 16, 16, gg.green)
	app.ctx.draw_sphere_wires(gg.Vec3{1, 0, 2}, 2, 16, 16, gs_lime)

	app.ctx.draw_cylinder(gg.Vec3{4, -1.5, -2}, gg.Vec3{4, 1.5, -2}, 1.5, 4, gs_skyblue)
	app.ctx.draw_cylinder_wires(gg.Vec3{4, -1.5, -2}, gg.Vec3{4, 1.5, -2}, 1.5, 4, gs_darkblue)
	app.ctx.draw_cylinder(gg.Vec3{4.5, -2, 2}, gg.Vec3{4.5, 0, 2}, 1, 6, gs_brown)
	app.ctx.draw_cylinder_wires(gg.Vec3{4.5, -2, 2}, gg.Vec3{4.5, 0, 2}, 1, 6, gs_brown)

	app.ctx.draw_mesh(&app.cone, gg.Vec3{}, gg.Vec3{}, 1.0, gs_gold)
	app.ctx.draw_sphere_wires(gg.Vec3{1, 0, -4}, 1.5, 8, 8, gs_pink)

	app.ctx.draw_cylinder(gg.Vec3{-3, -0.25, -4}, gg.Vec3{-4, 0.25, -4}, 1.2, 8, gs_purple)
	app.ctx.draw_sphere(gg.Vec3{-3, 0.25, -4}, 1.2, 8, 8, gs_purple)
	app.ctx.draw_sphere(gg.Vec3{-4, -0.25, -4}, 1.2, 8, 8, gs_purple)

	app.ctx.draw_grid(10, 1.0, gg.gray)
	app.ctx.end_3d()
	app.ctx.end()
}

fn main() {
	mut app := &App{}
	app.ctx = gg.new_context(
		width:        800
		height:       450
		window_title: 'raylib [models] example - geometric shapes'
		user_data:    app
		init_fn:      init
		frame_fn:     frame
		bg_color:     gs_raywhite
		depth:        true
	)
	app.ctx.run()
}
