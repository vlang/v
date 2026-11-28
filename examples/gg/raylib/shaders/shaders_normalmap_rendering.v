// Port of raylib [shaders] example - normalmap rendering.
// PORT NOTE: shader normalmap.vs/fs with tangent-space normal mapping and specular exponent
// requires gg user-shader support (not yet available); approximated with Material3D lighting.
module main

import gg

const win_width = 800
const win_height = 450

struct App {
mut:
	gg         &gg.Context = unsafe { nil }
	cam        gg.Camera3D
	plane      gg.Mesh3D
	light_pos  gg.Vec3 = gg.Vec3{0, 1, 0}
	use_normal bool    = true
	shininess  f32     = 8
	spin       f32
}

fn main() {
	mut app := &App{}
	app.cam = gg.Camera3D{ position: gg.Vec3{0, 2, -4}, target: gg.Vec3{}, fovy: 45 }
	app.cam.orbit(30, 10, 5)
	app.plane = gg.gen_mesh_plane(4, 4, 1)
	app.gg = gg.new_context(
		width:         win_width
		height:        win_height
		window_title:  'raylib [shaders] example - normalmap rendering'
		bg_color:      gg.white
		depth:         true
		user_data:     app
		update_fn:     update
		event_fn:      event
		frame_fn:      frame
		create_window: true
	)
	app.gg.run()
}

fn update(dt f32, data voidptr) {
	mut app := unsafe { &App(data) }
	mut var_dir := gg.Vec3{}
	if app.gg.pressed_keys[gg.KeyCode.w] {
		var_dir.z += 1
	}
	if app.gg.pressed_keys[gg.KeyCode.s] {
		var_dir.z -= 1
	}
	if app.gg.pressed_keys[gg.KeyCode.d] {
		var_dir.x -= 1
	}
	if app.gg.pressed_keys[gg.KeyCode.a] {
		var_dir.x += 1
	}
	app.light_pos = app.light_pos + var_dir.normalized().scale(dt * 3)
	if app.gg.pressed_keys[gg.KeyCode.up] {
		app.shininess += 40 * dt
	}
	if app.gg.pressed_keys[gg.KeyCode.down] {
		app.shininess -= 40 * dt
	}
	if app.shininess < 2 {
		app.shininess = 2
	}
	if app.shininess > 128 {
		app.shininess = 128
	}
	app.spin += dt * 0.5
}

fn event(e &gg.Event, data voidptr) {
	if e.typ != .key_down {
		return
	}
	mut app := unsafe { &App(data) }
	if e.key_code == .n {
		app.use_normal = !app.use_normal
	}
}

fn frame(data voidptr) {
	app := unsafe { &App(data) }
	ctx := app.gg
	ctx.begin()
	ctx.set_camera(app.cam)
	ctx.begin_3d() or { panic(err.msg()) }
	mat := gg.Material3D{
		color:   gg.rgb(110, 110, 120)
		ambient: if app.use_normal { 0.2 } else { 0.5 }
		light:   gg.Light3D{
			direction: (gg.Vec3{} - app.light_pos).normalized()
			color:     gg.white
			intensity: 1.0
		}
	}
	ctx.draw_material_mesh(&app.plane, gg.Vec3{}, gg.Vec3{0, app.spin * 57.3, 0}, 2, &mat, app.cam)
	ctx.draw_sphere(app.light_pos, 0.2, 8, 8, gg.orange)
	ctx.end_3d()
	col := if app.use_normal { gg.dark_green } else { gg.red }
	toggle := if app.use_normal { 'On' } else { 'Off' }
	ctx.draw_text(10, 10, 'Use [N] to toggle normal map: ${toggle}', size: 12, color: col)
	ctx.draw_text(10, 34, 'WASD move light, UP/DOWN shininess (${app.shininess:.1f})',
		size:  12
		color: gg.black
	)
	ctx.end()
}
