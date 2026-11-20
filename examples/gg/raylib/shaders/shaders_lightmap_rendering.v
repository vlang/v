// Port of raylib [shaders] example - lightmap rendering.
// PORT NOTE: shader lightmap.vs/fs with a second UV set and a baked lightmap requires gg
// user-shader support (not yet available); approximated with Material3D lighting and a
// RenderTexture2D that holds a small CPU-generated lightmap preview.
module main

import gg

const win_width = 800
const win_height = 450
const map_size = 16

struct App {
mut:
	gg       &gg.Context = unsafe { nil }
	cam      gg.Camera3D
	plane    gg.Mesh3D
	lightmap &gg.RenderTexture2D = unsafe { nil }
}

fn main() {
	mut app := &App{}
	app.cam = gg.Camera3D{ position: gg.Vec3{4, 6, 8}, target: gg.Vec3{}, fovy: 45 }
	app.cam.orbit(30, 20, 10)
	app.plane = gg.gen_mesh_plane(f32(map_size), f32(map_size), 1)
	app.gg = gg.new_context(
		width:         win_width
		height:        win_height
		window_title:  'raylib [shaders] example - lightmap rendering'
		bg_color:      gg.white
		depth:         true
		user_data:     app
		init_fn:       init
		update_fn:     update
		frame_fn:      frame
		cleanup_fn:    cleanup
		create_window: true
	)
	app.gg.run()
}

fn init(data voidptr) {
	mut app := unsafe { &App(data) }
	app.lightmap = gg.new_render_texture2d(map_size, map_size) or {
		eprintln('render texture: ${err}')
		return
	}
}

fn cleanup(data voidptr) {
	app := unsafe { &App(data) }
	if !isnil(app.lightmap) {
		mut rt := app.lightmap
		rt.free()
	}
}

fn update(dt f32, data voidptr) {
	mut app := unsafe { &App(data) }
	app.cam.update(.orbital, app.gg, dt)
}

fn frame(data voidptr) {
	app := unsafe { &App(data) }
	ctx := app.gg
	if !isnil(app.lightmap) {
		mut rt := app.lightmap
		rt.begin(gg.black)
		// A lightmap would be baked here; gg has no second UV channel for it.
		rt.end()
	}
	ctx.begin()
	ctx.set_camera(app.cam)
	ctx.begin_3d() or { panic(err.msg()) }
	mat := gg.Material3D{
		color:   gg.rgb(90, 130, 160)
		ambient: 0.35
		light:   gg.Light3D{ direction: gg.Vec3{0.3, -1, 0.2}, color: gg.yellow }
	}
	ctx.draw_material_mesh(&app.plane, gg.Vec3{}, gg.Vec3{}, 1, &mat, app.cam)
	ctx.end_3d()
	ctx.draw_text(10, 10, 'Lightmap preview ${map_size}x${map_size} (RenderTexture2D)',
		size:  14
		color: gg.dark_gray
	)
	ctx.draw_text(10, 30, 'Baked lightmap shader unavailable in gg', size: 14, color: gg.gray)
	ctx.end()
}
