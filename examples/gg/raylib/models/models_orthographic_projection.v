module main

// Port of raylib models_orthographic_projection.c
import gg

const op_raywhite = gg.Color{245, 245, 245, 255}
const op_gold = gg.Color{255, 203, 40, 255}
const op_maroon = gg.Color{128, 0, 0, 255}
const op_darkgray = gg.Color{80, 80, 80, 255}
const op_skyblue = gg.Color{135, 206, 235, 255}
const op_darkblue = gg.Color{0, 42, 141, 255}
const op_brown = gg.Color{165, 42, 42, 255}
const op_pink = gg.Color{255, 109, 194, 255}
const op_lime = gg.Color{0, 255, 0, 255}

struct App {
mut:
	ctx &gg.Context = unsafe { nil }
	cam gg.Camera3D
}

fn init(data voidptr) {
	mut app := unsafe { &App(data) }
	app.cam = gg.Camera3D{
		position: gg.Vec3{0, 10, 10}
		target:   gg.Vec3{}
		fovy:     45
	}
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
	app.ctx.draw_cube_wires(gg.Vec3{-4, 0, 2}, gg.Vec3{2, 5, 2}, op_gold)
	app.ctx.draw_cube_wires(gg.Vec3{-4, 0, -2}, gg.Vec3{3, 6, 2}, op_maroon)
	app.ctx.draw_sphere(gg.Vec3{-1, 0, -2}, 1, 16, 16, gg.green)
	app.ctx.draw_sphere_wires(gg.Vec3{1, 0, 2}, 2, 16, 16, op_lime)
	app.ctx.draw_cylinder(gg.Vec3{4, -1.5, -2}, gg.Vec3{4, 1.5, -2}, 1.5, 4, op_skyblue)
	app.ctx.draw_cylinder_wires(gg.Vec3{4, -1.5, -2}, gg.Vec3{4, 1.5, -2}, 1.5, 4, op_darkblue)
	app.ctx.draw_cylinder(gg.Vec3{4.5, -2, 2}, gg.Vec3{4.5, 0, 2}, 1, 6, op_brown)
	app.ctx.draw_cylinder_wires(gg.Vec3{4.5, -2, 2}, gg.Vec3{4.5, 0, 2}, 1, 6, op_brown)
	app.ctx.draw_cylinder(gg.Vec3{1, -1.5, -4}, gg.Vec3{1, 1.5, -4}, 1.5, 8, op_gold)
	app.ctx.draw_cylinder_wires(gg.Vec3{1, -1.5, -4}, gg.Vec3{1, 1.5, -4}, 1.5, 8, op_pink)
	app.ctx.draw_grid(10, 1.0, gg.gray)
	app.ctx.end_3d()
	app.ctx.draw_text(10, 420, 'Press Spacebar to switch camera type', size: 20, color: op_darkgray)
	label := if app.cam.projection == .orthographic { 'ORTHOGRAPHIC' } else { 'PERSPECTIVE' }
	app.ctx.draw_text(10, 40, label, size: 20, color: op_darkgray)
	app.ctx.end()
}

fn on_event(e &gg.Event, data voidptr) {
	mut app := unsafe { &App(data) }
	if e.typ == .key_down && e.key_code == .space {
		if app.cam.projection == .perspective {
			app.cam.projection = .orthographic
			app.cam.ortho_size = 10
		} else {
			app.cam.projection = .perspective
			app.cam.fovy = 45
		}
	}
}

fn main() {
	mut app := &App{}
	app.ctx = gg.new_context(
		width:        800
		height:       450
		window_title: 'raylib [models] example - orthographic projection'
		user_data:    app
		init_fn:      init
		frame_fn:     frame
		event_fn:     on_event
		bg_color:     op_raywhite
		depth:        true
	)
	app.ctx.run()
}
