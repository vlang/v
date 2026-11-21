module main

// Port of raylib models_textured_cube.c
// PORT NOTE: gg has no raw texcoord immediate mode. Full-texture cubes use
// draw_cube_tex(); the partial-texture cube uses an image_crop()-ed region.
import gg

const tc_raywhite = gg.Color{245, 245, 245, 255}
const tc_darkgray = gg.Color{80, 80, 80, 255}

struct App {
mut:
	ctx   &gg.Context = unsafe { nil }
	cam   gg.Camera3D
	tex   gg.Image
	crop  gg.Image
	has_crop bool
}

fn cache_img(ctx &gg.Context, img gg.Image) int {
	mut copy := img
	copy.nr_channels = 4
	copy.init_sokol_image()
	return ctx.cache_image(copy)
}

fn init(data voidptr) {
	mut app := unsafe { &App(data) }
	app.cam = gg.Camera3D{
		position: gg.Vec3{0, 10, 10}
		target:   gg.Vec3{}
		fovy:     45
	}
	app.tex = app.ctx.create_image('resources/cubicmap_atlas.png') or { gg.Image{} }
	if app.tex.data != unsafe { nil } {
		app.crop = gg.image_crop(&app.tex, 0, app.tex.height / 2, app.tex.width / 2,
			app.tex.height / 2) or { gg.Image{} }
		_ = cache_img(app.ctx, app.crop)
		app.has_crop = true
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
	app.ctx.draw_cube_tex(gg.Vec3{-2, 2, 0}, gg.Vec3{2, 4, 2}, &app.tex, gg.white)
	if app.has_crop {
		app.ctx.draw_cube_tex(gg.Vec3{2, 1, 0}, gg.Vec3{2, 2, 2}, &app.crop, gg.white)
	}
	app.ctx.draw_grid(10, 1.0, gg.gray)
	app.ctx.end_3d()
	app.ctx.end()
}

fn main() {
	mut app := &App{}
	app.ctx = gg.new_context(
		width:        800
		height:       450
		window_title: 'raylib [models] example - textured cube'
		user_data:    app
		init_fn:      init
		frame_fn:     frame
		bg_color:     tc_raywhite
		depth:        true
	)
	app.ctx.run()
}
