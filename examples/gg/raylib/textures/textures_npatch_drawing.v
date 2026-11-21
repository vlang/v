module main

// Port of raylib textures_npatch_drawing.c
// PORT NOTE: gg has only one 9-slice helper. Three-patch (horizontal/vertical)
// modes are approximated by 9-slicing with all four borders set to the raylib
// patch values; the single-axis stretch of raylib is not reproduced exactly.
import gg

const npatch_raywhite = gg.Color{245, 245, 245, 255}
const npatch_blue = gg.Color{0, 121, 241, 255}
const npatch_darkgray = gg.Color{80, 80, 80, 255}

struct App {
mut:
	ctx &gg.Context = unsafe { nil }
	tex gg.Image
	r1  gg.Rect
	r2  gg.Rect
	r3  gg.Rect
	r4  gg.Rect
}

fn init(data voidptr) {
	mut app := unsafe { &App(data) }
	app.tex = app.ctx.create_image('resources/ninepatch_button.png') or { gg.Image{} }
	app.r1 = gg.Rect{480, 160, 32, 32}
	app.r2 = gg.Rect{160, 160, 32, 32}
	app.r3 = gg.Rect{160, 93, 32, 32}
	app.r4 = gg.Rect{92, 160, 32, 32}
}

fn frame(data voidptr) {
	mut app := unsafe { &App(data) }
	mx := f32(app.ctx.mouse_pos_x)
	my := f32(app.ctx.mouse_pos_y)

	app.r1.width = mx - app.r1.x
	app.r1.height = my - app.r1.y
	app.r2.width = mx - app.r2.x
	app.r2.height = my - app.r2.y
	app.r3.width = mx - app.r3.x
	app.r4.height = my - app.r4.y
	mut rects := [&app.r1, &app.r2, &app.r3, &app.r4]
	for mut r in rects {
		if r.width < 1 {
			r.width = 1
		}
		if r.width > 300 {
			r.width = 300
		}
		if r.height < 1 {
			r.height = 1
		}
		if r.height > 300 {
			r.height = 300
		}
	}

	app.ctx.begin()
	app.ctx.draw_9slice(&app.tex, app.r2.x, app.r2.y, app.r2.width, app.r2.height, 16, 16, 16, 16)
	app.ctx.draw_9slice(&app.tex, app.r1.x, app.r1.y, app.r1.width, app.r1.height, 12, 40, 12, 12)
	app.ctx.draw_9slice(&app.tex, app.r3.x, app.r3.y, app.r3.width, app.r3.height, 8, 8, 8, 8)
	app.ctx.draw_9slice(&app.tex, app.r4.x, app.r4.y, app.r4.width, app.r4.height, 6, 6, 6, 6)

	app.ctx.draw_rect_empty(5, 88, 74, 266, npatch_blue)
	app.ctx.draw_image(10, 93, f32(app.tex.width), f32(app.tex.height), &app.tex)
	app.ctx.draw_text(15, 360, 'TEXTURE', size: 10, color: npatch_darkgray)
	app.ctx.draw_text(10, 20, 'Move the mouse to stretch or shrink the n-patches',
		size:  20
		color: npatch_darkgray
	)
	app.ctx.end()
}

fn main() {
	mut app := &App{}
	app.ctx = gg.new_context(
		width:        800
		height:       450
		window_title: 'raylib [textures] example - npatch drawing'
		user_data:    app
		init_fn:      init
		frame_fn:     frame
		bg_color:     npatch_raywhite
	)
	app.ctx.run()
}
