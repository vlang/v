module main

// Port of raylib models_rotating_cube.c
import gg

const rc_raywhite = gg.Color{245, 245, 245, 255}

fn cache_img(ctx &gg.Context, img gg.Image) int {
	mut copy := img
	copy.nr_channels = 4
	copy.init_sokol_image()
	return ctx.cache_image(copy)
}

struct App {
mut:
	ctx      &gg.Context = unsafe { nil }
	cam      gg.Camera3D
	mesh     gg.Mesh3D
	model    gg.Model3D
	texture  gg.Image
	crop     gg.Image
	rotation f32
}

fn init(data voidptr) {
	mut app := unsafe { &App(data) }
	app.cam = gg.Camera3D{
		position: gg.Vec3{0, 3, 3}
		target:   gg.Vec3{}
		fovy:     45
	}
	app.mesh = gg.gen_mesh_cube(gg.Vec3{1, 1, 1})
	app.model = gg.Model3D{mesh: &app.mesh}
	app.texture = app.ctx.create_image('resources/cubicmap_atlas.png') or { gg.Image{} }
	if app.texture.data != unsafe { nil } {
		app.crop = gg.image_crop(&app.texture, 0, app.texture.height / 2, app.texture.width / 2,
			app.texture.height / 2) or { gg.Image{} }
		_ = cache_img(app.ctx, app.crop)
		app.model.material.texture = &app.crop
	}
}

fn frame(data voidptr) {
	mut app := unsafe { &App(data) }
	app.rotation += 1
	app.model.rotation = gg.Vec3{app.rotation * 0.5, app.rotation, 0}
	app.ctx.begin()
	app.ctx.set_camera(app.cam)
	app.ctx.begin_3d(cam: app.cam) or {
		app.ctx.end()
		return
	}
	app.ctx.draw_model(&app.model, app.cam)
	app.ctx.draw_grid(10, 1.0, gg.gray)
	app.ctx.end_3d()
	app.ctx.end()
}

fn main() {
	mut app := &App{}
	app.ctx = gg.new_context(
		width:        800
		height:       450
		window_title: 'raylib [models] example - rotating cube'
		user_data:    app
		init_fn:      init
		frame_fn:     frame
		bg_color:     rc_raywhite
		depth:        true
	)
	app.ctx.run()
}
