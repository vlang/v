module main

// PORT NOTE: gg has no M3D loader or skeleton/pose API. A placeholder cube is
// drawn and the original skeleton display is approximated with wire boxes.
import gg

const m3_raywhite = gg.Color{245, 245, 245, 255}
const m3_gray = gg.Color{211, 211, 211, 255}
const m3_maroon = gg.Color{128, 0, 0, 255}

struct App {
mut:
	ctx &gg.Context = unsafe { nil }
	cam gg.Camera3D
	show_skeleton bool
}

fn init(data voidptr) {
	mut app := unsafe { &App(data) }
	app.cam = gg.Camera3D{
		position: gg.Vec3{1.5, 1.5, 1.5}
		target:   gg.Vec3{0, 0.4, 0}
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
	if app.show_skeleton {
		app.ctx.draw_cube_wires(gg.Vec3{0, 0.6, 0}, gg.Vec3{0.15, 1.2, 0.15}, gg.red)
		app.ctx.draw_line_3d(gg.Vec3{0, 1.2, 0}, gg.Vec3{0.6, 0.9, 0}, gg.red)
		app.ctx.draw_line_3d(gg.Vec3{0, 1.2, 0}, gg.Vec3{-0.6, 0.9, 0}, gg.red)
	} else {
		app.ctx.draw_cube(gg.Vec3{0, 0.6, 0}, gg.Vec3{0.8, 1.2, 0.6}, m3_gray)
	}
	app.ctx.draw_grid(10, 1.0, gg.gray)
	app.ctx.end_3d()
	app.ctx.draw_text(10, 10, 'M3D loading unavailable in gg (placeholder shown)',
		size: 20, color: m3_gray)
	app.ctx.draw_text(10, 40, 'Press SPACE to draw skeleton', size: 20, color: m3_maroon)
	app.ctx.draw_text(800 - 210, 430, '(c) CesiumMan model by KhronosGroup', size: 10, color: m3_gray)
	app.ctx.end()
}

fn on_event(e &gg.Event, data voidptr) {
	mut app := unsafe { &App(data) }
	if e.typ == .key_down && e.key_code == .space {
		app.show_skeleton = !app.show_skeleton
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
		window_title: 'raylib [models] example - loading m3d'
		user_data:    app
		init_fn:      init
		frame_fn:     frame
		event_fn:     on_event
		update_fn:    update
		bg_color:     m3_raywhite
		depth:        true
	)
	app.ctx.run()
}
