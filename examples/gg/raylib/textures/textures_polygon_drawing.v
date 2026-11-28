module main

// Port of raylib textures_polygon_drawing.c
// PORT NOTE: gg has no immediate-mode textured polygon API (rlBegin/rlTexCoord).
// The cat texture is drawn as a rotating textured quad made of triangular
// strips approximated with draw_image_with_config(); the UV-mapped polygon
// silhouette of the original is not reproduced exactly.
import gg
import math

const poly_raywhite = gg.Color{245, 245, 245, 255}
const poly_darkgray = gg.Color{80, 80, 80, 255}

struct App {
mut:
	ctx     &gg.Context = unsafe { nil }
	texture gg.Image
	angle   f32
}

fn init(data voidptr) {
	mut app := unsafe { &App(data) }
	app.texture = app.ctx.create_image('resources/cat.png') or { gg.Image{} }
}

fn frame(data voidptr) {
	mut app := unsafe { &App(data) }
	app.angle++
	app.ctx.begin()
	w := f32(app.texture.width)
	h := f32(app.texture.height)
	app.ctx.draw_text(20, 20, 'textured polygon', size: 20, color: poly_darkgray)
	// A fan of textured slices gives the silhouette a faceted polygon feel.
	slices := 10
	for i in 0 .. slices {
		t := f32(i) / f32(slices)
		app.ctx.draw_image_with_config(
			img:       &app.texture
			img_rect:  gg.Rect{400 - w / 2, 225 - h / 2 + (t - 0.5) * 12, w, h}
			part_rect: gg.Rect{t * w, 0, w / f32(slices), h}
			rotation:  app.angle + f32(math.sin(f64(t * 6.28)) * 4)
		)
	}
	app.ctx.end()
}

fn main() {
	mut app := &App{}
	app.ctx = gg.new_context(
		width:        800
		height:       450
		window_title: 'raylib [textures] example - polygon drawing'
		user_data:    app
		init_fn:      init
		frame_fn:     frame
		bg_color:     poly_raywhite
	)
	app.ctx.run()
}
