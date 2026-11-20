module main

// Port of raylib textures_image_channel.c
// PORT NOTE: gg has no ImageFromChannel()/ImageAlphaMask(). The four channel
// images are extracted from the source RGBA buffer directly here.
import gg

const channel_raywhite = gg.Color{245, 245, 245, 255}

fn cache_rgba(ctx &gg.Context, w int, h int, buf []u8) int {
	mut img := gg.Image{ width: w, height: h, nr_channels: 4, ok: true, data: buf.data }
	img.init_sokol_image()
	return ctx.cache_image(img)
}

fn build_channel(orig []u8, w int, h int, ch int) []u8 {
	mut out := []u8{len: w * h * 4}
	for i in 0 .. w * h {
		v := orig[i * 4 + ch]
		s := i * 4
		if ch == 3 {
			unsafe {
				out[s] = v
				out[s + 1] = v
				out[s + 2] = v
				out[s + 3] = v
			}
		} else {
			a := orig[s + 3]
			unsafe {
				out[s] = v
				out[s + 1] = v
				out[s + 2] = v
				out[s + 3] = a
			}
		}
	}
	return out
}

fn build_checker(w int, h int) []u8 {
	mut out := []u8{len: w * h * 4}
	orange := gg.Color{255, 160, 0, 255}
	yellow := gg.Color{255, 255, 0, 255}
	for y in 0 .. h {
		for x in 0 .. w {
			c := if ((x / (w / 20)) + (y / (h / 20))) % 2 == 0 { orange } else { yellow }
			i := (y * w + x) * 4
			unsafe {
				out[i] = c.r
				out[i + 1] = c.g
				out[i + 2] = c.b
				out[i + 3] = 255
			}
		}
	}
	return out
}

struct App {
mut:
	ctx      &gg.Context = unsafe { nil }
	idx      [6]int
	store    [6][]u8
	full_idx int = -1
	w        int
	h        int
}

fn init(data voidptr) {
	mut app := unsafe { &App(data) }
	src := app.ctx.create_image('resources/fudesumi.png') or { gg.Image{} }
	if src.data == unsafe { nil } || src.width <= 0 {
		return
	}
	app.w = src.width
	app.h = src.height
	orig := unsafe { (&u8(src.data)).vbytes(app.w * app.h * 4).clone() }
	app.store[4] = orig
	app.full_idx = cache_rgba(app.ctx, app.w, app.h, app.store[4])

	for ch in 0 .. 4 {
		app.store[ch] = build_channel(orig, app.w, app.h, ch)
		app.idx[ch] = cache_rgba(app.ctx, app.w, app.h, app.store[ch])
	}
	app.store[5] = build_checker(800, 450)
	app.idx[5] = cache_rgba(app.ctx, 800, 450, app.store[5])
}

fn frame(data voidptr) {
	mut app := unsafe { &App(data) }
	app.ctx.begin()
	if app.idx[5] >= 0 {
		app.ctx.draw_image_by_id(0, 0, 800, 450, app.idx[5])
	}
	// full image
	if app.full_idx >= 0 {
		app.ctx.draw_image_by_id(50, 10, f32(app.w) * 0.8, f32(app.h) * 0.8, app.full_idx)
	}
	// channels, tinted to match raylib's colour overlays
	tints := [gg.Color{255, 0, 0, 255}, gg.Color{0, 255, 0, 255}, gg.Color{0, 0, 255, 255},
		gg.Color{255, 255, 255, 255}]
	positions := [gg.Rect{410, 10, 0, 0}, gg.Rect{600, 10, 0, 0}, gg.Rect{410, 230, 0, 0},
		gg.Rect{600, 230, 0, 0}]
	for i in 0 .. 4 {
		p := positions[i]
		p.width = f32(app.w) * 0.8 / 2
		p.height = f32(app.h) * 0.8 / 2
		if app.idx[i] >= 0 {
			app.ctx.draw_image_with_config(img_id: app.idx[i], img_rect: p, color: tints[i])
		}
	}
	app.ctx.end()
}

fn main() {
	mut app := &App{}
	app.ctx = gg.new_context(
		width:        800
		height:       450
		window_title: 'raylib [textures] example - image channel'
		user_data:    app
		init_fn:      init
		frame_fn:     frame
		bg_color:     channel_raywhite
	)
	app.ctx.run()
}
