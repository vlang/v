module main

// PORT NOTE: gg has no IQM loader (load_model supports only .obj/.gltf/.glb).
// A placeholder cube stands in for the IQM character and the original example's
// controls are described in the HUD.
import gg

const iq_raywhite = gg.Color{245, 245, 245, 255}
const iq_gray = gg.Color{128, 128, 128, 255}
const iq_maroon = gg.Color{128, 0, 0, 255}

struct App {
mut:
	ctx &gg.Context = unsafe { nil }
	cam gg.Camera3D
}

fn init(data voidptr) {
	mut app := unsafe { &App(data) }
	app.cam = gg.Camera3D{
		position: gg.Vec3{10, 10, 10}
		target:   gg.Vec3{0, 4, 0}
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
	app.ctx.draw_cube(gg.Vec3{0, 1.5, 0}, gg.Vec3{2, 3, 2}, iq_gray)
	app.ctx.draw_cube_wires(gg.Vec3{0, 1.5, 0}, gg.Vec3{2, 3, 2}, iq_maroon)
	app.ctx.draw_grid(10, 1.0, gg.gray)
	app.ctx.end_3d()
	app.ctx.draw_text(10, 10, 'IQM loading unavailable in gg (placeholder shown)',
		size: 20, color: iq_maroon)
	app.ctx.draw_text(10, 40, 'Animation speed: 1.00 (no animation data)',
		size: 20, color: iq_maroon)
	app.ctx.draw_text(10, 416, 'Use left and right arrow keys to change current animation',
		size: 10, color: iq_gray)
	app.ctx.draw_text(10, 430, 'Use up and down arrow keys to change animation speed',
		size: 10, color: iq_gray)
	app.ctx.end()
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
		window_title: 'raylib [models] example - loading iqm'
		user_data:    app
		init_fn:      init
		frame_fn:     frame
		update_fn:    update
		bg_color:     iq_raywhite
		depth:        true
	)
	app.ctx.run()
}
