module main

// PORT NOTE: gg has no cubemap/skybox shader API (LoadTextureCubemap,
// GenTextureCubemap). The six-faced skybox is approximated with a large wire
// cube surrounding the camera and a grid; drag & drop is unavailable too.
import gg

const sky_raywhite = gg.Color{245, 245, 245, 255}
const sky_blue = gg.Color{0, 121, 241, 255}
const sky_dark = gg.Color{80, 80, 80, 255}

struct App {
mut:
	ctx &gg.Context = unsafe { nil }
	cam gg.Camera3D
}

fn init(data voidptr) {
	mut app := unsafe { &App(data) }
	app.cam = gg.Camera3D{
		position: gg.Vec3{1, 1, 1}
		target:   gg.Vec3{4, 1, 4}
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
	app.ctx.draw_cube_wires(gg.Vec3{}, gg.Vec3{40, 40, 40}, sky_blue)
	app.ctx.draw_cube_wires(gg.Vec3{}, gg.Vec3{30, 30, 30}, sky_dark)
	app.ctx.draw_grid(10, 1.0, gg.gray)
	app.ctx.end_3d()
	app.ctx.draw_text(10, 430, 'Skybox (cubemap shader unavailable in gg) - WASD + mouse to look',
		size: 10, color: sky_dark)
	app.ctx.end()
}

fn update(dt f32, data voidptr) {
	mut app := unsafe { &App(data) }
	app.cam.update(.first_person, app.ctx, dt)
}

fn main() {
	mut app := &App{}
	app.ctx = gg.new_context(
		width:        800
		height:       450
		window_title: 'raylib [models] example - skybox rendering'
		user_data:    app
		init_fn:      init
		frame_fn:     frame
		update_fn:    update
		bg_color:     sky_raywhite
		depth:        true
	)
	app.ctx.run()
}
