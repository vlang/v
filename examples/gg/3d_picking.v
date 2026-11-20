module main

import gg

struct App {
mut:
	gg        &gg.Context = unsafe { nil }
	cam       gg.Camera3D
	picked    int = -1
	colliders []gg.Collider
}

fn main() {
	mut app := &App{}
	app.cam = gg.new_camera3d(gg.Vec3{0, 3, 8}, gg.Vec3{})
	app.cam.orbit(30, 20, 8)
	app.colliders = [
		gg.Collider{ id: 0, center: gg.Vec3{-1.5, 1, 0}, size: gg.Vec3{2, 2, 2} }
		gg.Collider{ id: 1, center: gg.Vec3{1.5, 1, 0}, size: gg.Vec3{2, 2, 2} },
	]
	app.gg = gg.new_context(
		width:        640
		height:       480
		window_title: 'gg 3D picking'
		depth:        true
		event_fn:     on_event
		frame_fn:     frame
		user_data:    app
	)
	app.gg.run()
}

fn on_event(e &gg.Event, data voidptr) {
	mut app := unsafe { &App(data) }
	if e.typ == .mouse_down {
		app.picked = app.gg.pick_at(e, app.colliders) or { -1 }
	}
}

fn frame(data voidptr) {
	app := unsafe { &App(data) }
	ctx := app.gg
	ctx.begin()
	ctx.set_camera(app.cam)
	ctx.begin_3d() or { panic(err.msg()) }
	ctx.debug_mode3d(true, true)
	for i, c in app.colliders {
		color := if i == app.picked { gg.yellow } else { gg.blue }
		ctx.draw_cube(c.center, c.size, color)
	}
	ctx.end_3d()
	ctx.draw_text_def(8, 8, 'click a cube; picked=${app.picked}')
	ctx.end()
}
