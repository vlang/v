module main

// Port of raylib textures_image_drawing.c
// PORT NOTE: ImageDrawTextEx() bakes a custom font into CPU pixels; gg has no
// font-to-image baking, so the caption is drawn as 2D text over the final
// texture at render time instead. Circle lines on an image are approximated
// with a pixel-level ring.
import gg

const drawing_raywhite = gg.Color{245, 245, 245, 255}
const drawing_darkgray = gg.Color{80, 80, 80, 255}

fn cache_rgba(ctx &gg.Context, w int, h int, buf []u8) int {
	mut img := gg.Image{ width: w, height: h, nr_channels: 4, ok: true, data: buf.data }
	img.init_sokol_image()
	return ctx.cache_image(img)
}

fn put_pixel(buf []u8, w int, h int, x int, y int, c gg.Color) {
	if x < 0 || y < 0 || x >= w || y >= h {
		return
	}
	i := (y * w + x) * 4
	unsafe {
		buf[i] = c.r
		buf[i + 1] = c.g
		buf[i + 2] = c.b
		buf[i + 3] = c.a
	}
}

struct App {
mut:
	ctx    &gg.Context = unsafe { nil }
	idx    int         = -1
	final  []u8
	w      int
	h      int
	cat    gg.Image
	cat2   gg.Image
	parrot gg.Image
}

fn init(data voidptr) {
	mut app := unsafe { &App(data) }
	app.cat = app.ctx.create_image('resources/cat.png') or { gg.Image{} }
	if app.cat.data == unsafe { nil } || app.cat.width <= 0 {
		return
	}
	cropped := gg.image_crop(&app.cat, 100, 10, 280, 380) or { gg.Image{} }
	app.cat2 = cropped
	gg.image_flip_horizontal(&app.cat2)
	resized := gg.image_resize(&app.cat2, 150, 200) or { gg.Image{} }
	app.cat2 = resized

	app.parrot = app.ctx.create_image('resources/parrots.png') or { gg.Image{} }
	if app.parrot.data == unsafe { nil } || app.parrot.width <= 0 {
		return
	}
	app.w = app.parrot.width
	app.h = app.parrot.height
	mut work := unsafe { (&u8(app.parrot.data)).vbytes(app.w * app.h * 4).clone() }

	// ImageDrawImagePro: draw the resized cat at 1.5x over the parrots.
	if app.cat2.data != unsafe { nil } {
		src := unsafe { &u8(app.cat2.data) }
		for y in 0 .. app.cat2.height {
			for x in 0 .. app.cat2.width {
				s := (y * app.cat2.width + x) * 4
				r := unsafe { src[s] }
				g := unsafe { src[s + 1] }
				b := unsafe { src[s + 2] }
				dx := int(30 + f32(x) * 1.5)
				dy := int(40 + f32(y) * 1.5)
				for sy := 0; sy < 2; sy++ {
					for sx := 0; sx < 2; sx++ {
						put_pixel(work, app.w, app.h, dx + sx, dy + sy, gg.Color{r, g, b, 255})
					}
				}
			}
		}
	}

	// Crop 100 pixels off the top and bottom of the result.
	crop_top := 50
	crop_h := app.h - 100
	mut cropped_parrot := []u8{len: app.w * crop_h * 4}
	for y in 0 .. crop_h {
		for x in 0 .. app.w {
			s := ((y + crop_top) * app.w + x) * 4
			d := (y * app.w + x) * 4
			unsafe {
				cropped_parrot[d] = work[s]
				cropped_parrot[d + 1] = work[s + 1]
				cropped_parrot[d + 2] = work[s + 2]
				cropped_parrot[d + 3] = work[s + 3]
			}
		}
	}
	work = cropped_parrot
	app.h = crop_h
	put_pixel(work, app.w, app.h, 10, 10, drawing_raywhite)
	for y in 20 .. 30 {
		for x in 5 .. 15 {
			put_pixel(work, app.w, app.h, x, y, drawing_raywhite)
		}
	}
	// A small ring around (10, 10) with radius 5.
	for y in 5 .. 16 {
		for x in 5 .. 16 {
			d := (x - 10) * (x - 10) + (y - 10) * (y - 10)
			if d >= 16 && d <= 36 {
				put_pixel(work, app.w, app.h, x, y, drawing_raywhite)
			}
		}
	}
	app.final = work
	app.idx = cache_rgba(app.ctx, app.w, app.h, app.final)
}

fn frame(data voidptr) {
	mut app := unsafe { &App(data) }
	app.ctx.begin()
	if app.idx >= 0 {
		app.ctx.draw_image_by_id(400 - f32(app.w) / 2, 225 - f32(app.h) / 2 - 40, app.w, app.h, app.idx)
		app.ctx.draw_rect_empty(400 - f32(app.w) / 2, 225 - f32(app.h) / 2 - 40, f32(app.w), f32(app.h),
			drawing_darkgray)
	}
	app.ctx.draw_text(240, 350, 'We are drawing only one texture from various images composed!',
		size:  10
		color: drawing_darkgray
	)
	app.ctx.draw_text(190, 370,
		'Source images have been cropped, scaled, flipped and copied one over the other.',
		size:  10
		color: drawing_darkgray
	)
	app.ctx.end()
}

fn main() {
	mut app := &App{}
	app.ctx = gg.new_context(
		width:        800
		height:       450
		window_title: 'raylib [textures] example - image drawing'
		user_data:    app
		init_fn:      init
		frame_fn:     frame
		bg_color:     drawing_raywhite
	)
	app.ctx.run()
}
