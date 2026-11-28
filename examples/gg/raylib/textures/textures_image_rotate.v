module main

// Port of raylib textures_image_rotate.c
// PORT NOTE: gg can only bake 90-degree rotations on CPU images
// (image_rotate_90). The 45-degree variant draws the original image with a
// render-time rotation instead of pre-rotating its pixels.
import gg

const rotate_raywhite = gg.Color{245, 245, 245, 255}
const rotate_darkgray = gg.Color{80, 80, 80, 255}

struct App {
mut:
	ctx      &gg.Context = unsafe { nil }
	original gg.Image
	rot90    gg.Image
	rotm90   gg.Image
	idx90    int = -1
	idxm90   int = -1
	current  int
}

fn cache_img(ctx &gg.Context, img gg.Image) int {
	mut copy := img
	copy.nr_channels = 4
	copy.init_sokol_image()
	return ctx.cache_image(copy)
}

fn init(data voidptr) {
	mut app := unsafe { &App(data) }
	app.original = app.ctx.create_image('resources/raylib_logo.png') or { gg.Image{} }

	app.rot90 = app.ctx.create_image('resources/raylib_logo.png') or { gg.Image{} }
	gg.image_rotate_90(mut &app.rot90)
	app.idx90 = cache_img(app.ctx, app.rot90)

	app.rotm90 = app.ctx.create_image('resources/raylib_logo.png') or { gg.Image{} }
	gg.image_rotate_90(mut &app.rotm90)
	gg.image_rotate_90(mut &app.rotm90)
	gg.image_rotate_90(mut &app.rotm90)
	app.idxm90 = cache_img(app.ctx, app.rotm90)
}

fn frame(data voidptr) {
	mut app := unsafe { &App(data) }
	app.ctx.begin()
	match app.current {
		0 {
			app.ctx.draw_image_with_config(
				img:      &app.original
				img_rect: gg.Rect{400 - f32(app.original.width) / 2, 225 - f32(app.original.height) / 2, f32(app.original.width), f32(app.original.height)}
				rotation: 45
			)
		}
		1 {
			if app.idx90 >= 0 {
				app.ctx.draw_image_by_id(400 - f32(app.rot90.width) / 2, 225 - f32(app.rot90.height) / 2,
					app.rot90.width, app.rot90.height, app.idx90)
			}
		}
		else {
			if app.idxm90 >= 0 {
				app.ctx.draw_image_by_id(400 - f32(app.rotm90.width) / 2, 225 - f32(app.rotm90.height) / 2,
					app.rotm90.width, app.rotm90.height, app.idxm90)
			}
		}
	}
	app.ctx.draw_text(250, 420, 'Press LEFT MOUSE BUTTON to rotate the image clockwise',
		size:  10
		color: rotate_darkgray
	)
	app.ctx.end()
}

fn on_event(e &gg.Event, data voidptr) {
	mut app := unsafe { &App(data) }
	if (e.typ == .mouse_down && e.mouse_button == .left)
		|| (e.typ == .key_down && e.key_code == .right) {
		app.current = (app.current + 1) % 3
	}
}

fn main() {
	mut app := &App{}
	app.ctx = gg.new_context(
		width:        800
		height:       450
		window_title: 'raylib [textures] example - image rotate'
		user_data:    app
		init_fn:      init
		frame_fn:     frame
		event_fn:     on_event
		bg_color:     rotate_raywhite
	)
	app.ctx.run()
}
