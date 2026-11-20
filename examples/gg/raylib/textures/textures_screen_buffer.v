module main

// Port of raylib textures_screen_buffer.c
// The flame palette is generated with an HSV helper because gg has no
// ColorFromHSV(); the simulation itself runs on a CPU RGBA buffer uploaded to
// a cached image each frame.
import gg
import math
import rand

const screen_raywhite = gg.Color{245, 245, 245, 255}

fn hsv(h_in f32, s f32, v f32) gg.Color {
	mut h := f32(math.fmod(f64(h_in), 360.0))
	if h < 0 { h += 360 }
	c := v * s
	x := c * (1 - math.abs(f32(math.fmod(f64(h / 60.0), 2.0)) - 1))
	m := v - c
	mut r := f32(0)
	mut g := f32(0)
	mut b := f32(0)
	match int(h / 60) {
		0 {
			r = c
			g = x
		}
		1 {
			r = x
			g = c
		}
		2 {
			g = c
			b = x
		}
		3 {
			g = x
			b = c
		}
		4 {
			r = x
			b = c
		}
		else {
			r = c
			b = x
		}
	}
	return gg.Color{u8((r + m) * 255), u8((g + m) * 255), u8((b + m) * 255), 255}
}

fn cache_rgba(ctx &gg.Context, w int, h int, buf []u8) int {
	mut img := gg.Image{ width: w, height: h, nr_channels: 4, ok: true, data: buf.data }
	img.init_sokol_image()
	return ctx.cache_image(img)
}

struct App {
mut:
	ctx     &gg.Context = unsafe { nil }
	idx     int         = -1
	buf     []u8
	indices []u8
	roots   []u8
	palette [256]gg.Color
	img_w   int = 400
	img_h   int = 225
}

fn init(data voidptr) {
	mut app := unsafe { &App(data) }
	app.buf = []u8{len: app.img_w * app.img_h * 4}
	app.indices = []u8{len: app.img_w * app.img_h}
	app.roots = []u8{len: app.img_w}
	for i in 0 .. 256 {
		t := f32(i) / 255.0
		app.palette[i] = hsv(250 + 150 * t * t, t, t)
	}
	app.idx = cache_rgba(app.ctx, app.img_w, app.img_h, app.buf)
}

fn frame(data voidptr) {
	mut app := unsafe { &App(data) }
	for x in 2 .. app.roots.len {
		flame := int(app.roots[x]) + (rand.intn(3) or { 1 })
		app.roots[x] = if flame > 255 { 255 } else { u8(flame) }
	}
	for x in 0 .. app.roots.len {
		app.indices[x + (app.img_h - 1) * app.img_w] = app.roots[x]
	}
	for x in 0 .. app.img_w {
		if app.indices[x] != 0 { app.indices[x] = 0 }
	}
	mut y := 1
	for y < app.img_h {
		for x in 0 .. app.img_w {
			i := x + y * app.img_w
			mut ci := int(app.indices[i])
			if ci != 0 {
				app.indices[i] = 0
				step := (rand.intn(3) or { 1 }) - 1
				nx := x + step
				if nx > 0 && nx < app.img_w {
					above := i - app.img_w + step
					if above >= 0 && above < app.indices.len {
						decay := rand.intn(4) or { 1 }
						if decay < ci { ci -= decay } else { ci = 0 }
						app.indices[above] = u8(ci)
					}
				}
			}
		}
		y++
	}
	for yy in 1 .. app.img_h {
		for x in 0 .. app.img_w {
			c := app.palette[app.indices[x + yy * app.img_w]]
			i := (x + yy * app.img_w) * 4
			unsafe {
				app.buf[i] = c.r
				app.buf[i + 1] = c.g
				app.buf[i + 2] = c.b
				app.buf[i + 3] = 255
			}
		}
	}
	app.ctx.update_pixel_data(app.idx, app.buf.data)

	app.ctx.begin()
	if app.idx >= 0 {
		app.ctx.draw_image_by_id(0, 0, app.img_w * 2, app.img_h * 2, app.idx)
	}
	app.ctx.end()
}

fn main() {
	mut app := &App{}
	app.ctx = gg.new_context(
		width:        800
		height:       450
		window_title: 'raylib [textures] example - screen buffer'
		user_data:    app
		init_fn:      init
		frame_fn:     frame
		bg_color:     screen_raywhite
	)
	app.ctx.run()
}
