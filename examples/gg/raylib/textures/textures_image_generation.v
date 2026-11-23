module main

// Port of raylib textures_image_generation.c
// PORT NOTE: gg has no GenImage* helpers; each procedural texture is generated
// directly into an RGBA buffer. Perlin noise is approximated with smooth value
// noise (the original uses a Perlin implementation).
import gg
import math
import rand

const gen_raywhite = gg.Color{245, 245, 245, 255}
const gen_gray = gg.Color{128, 128, 128, 255}
const gen_skyblue = gg.Color{135, 206, 235, 255}

fn lerp_color(a gg.Color, b gg.Color, t f32) gg.Color {
	return gg.Color{
		r: u8(f32(a.r) + (f32(b.r) - f32(a.r)) * t)
		g: u8(f32(a.g) + (f32(b.g) - f32(a.g)) * t)
		b: u8(f32(a.b) + (f32(b.b) - f32(a.b)) * t)
		a: 255
	}
}

fn cache_rgba(ctx &gg.Context, w int, h int, buf []u8) int {
	mut img := gg.Image{ width: w, height: h, nr_channels: 4, ok: true, data: buf.data }
	img.init_sokol_image()
	return ctx.cache_image(img)
}

struct App {
mut:
	ctx     &gg.Context = unsafe { nil }
	images  [9][]u8
	indices [9]int
	current int
	img_w   int = 800
	img_h   int = 450
}

fn init(data voidptr) {
	mut app := unsafe { &App(data) }
	w := app.img_w
	h := app.img_h
	red := gg.Color{230, 41, 55, 255}
	blue := gg.Color{0, 121, 241, 255}
	white := gg.Color{255, 255, 255, 255}
	black := gg.Color{0, 0, 0, 255}

	// 0 vertical, 1 horizontal, 2 diagonal gradient
	for mode in 0 .. 3 {
		mut buf := []u8{len: w * h * 4}
		for y in 0 .. h {
			for x in 0 .. w {
				t := match mode {
					0 { f32(y) / f32(h) }
					1 { f32(x) / f32(w) }
					else { (f32(x) + f32(y)) / f32(w + h) }
				}
				c := lerp_color(red, blue, t)
				i := (y * w + x) * 4
				unsafe {
					buf[i] = c.r
					buf[i + 1] = c.g
					buf[i + 2] = c.b
					buf[i + 3] = 255
				}
			}
		}
		app.images[mode] = buf
		app.indices[mode] = cache_rgba(app.ctx, w, h, buf)
	}

	// 3 radial, 4 square gradient
	for mode in 3 .. 5 {
		mut buf := []u8{len: w * h * 4}
		mut dy := f32(h) / 2
		mut dx := f32(w) / 2
		if mode == 4 {
			dx = f32(w)
			dy = f32(h)
		}
		for y in 0 .. h {
			for x in 0 .. w {
				ddx := f32(x) - f32(w) / 2
				ddy := f32(y) - f32(h) / 2
				mut dist := if mode == 3 {
					f32(math.sqrt(f64(ddx * ddx + ddy * ddy))) / f32(math.sqrt(f64(dx * dx + dy * dy)) / 2)
				} else {
					f32(math.max(f64(math.abs(ddx) / f64(dx)), f64(math.abs(ddy) / f64(dy)))) * 2
				}
				if dist > 1 {
					dist = 1
				}
				c := lerp_color(white, black, dist)
				i := (y * w + x) * 4
				unsafe {
					buf[i] = c.r
					buf[i + 1] = c.g
					buf[i + 2] = c.b
					buf[i + 3] = 255
				}
			}
		}
		app.images[mode] = buf
		app.indices[mode] = cache_rgba(app.ctx, w, h, buf)
	}

	// 5 checked
	{
		mut buf := []u8{len: w * h * 4}
		for y in 0 .. h {
			for x in 0 .. w {
				c := if ((x / 32) + (y / 32)) % 2 == 0 { red } else { blue }
				i := (y * w + x) * 4
				unsafe {
					buf[i] = c.r
					buf[i + 1] = c.g
					buf[i + 2] = c.b
					buf[i + 3] = 255
				}
			}
		}
		app.images[5] = buf
		app.indices[5] = cache_rgba(app.ctx, w, h, buf)
	}

	// 6 white noise
	{
		mut buf := []u8{len: w * h * 4}
		for i in 0 .. w * h {
			v := u8(rand.intn(256) or { 128 })
			unsafe {
				buf[i * 4] = v
				buf[i * 4 + 1] = v
				buf[i * 4 + 2] = v
				buf[i * 4 + 3] = 255
			}
		}
		app.images[6] = buf
		app.indices[6] = cache_rgba(app.ctx, w, h, buf)
	}

	// 7 value noise (Perlin approximation)
	{
		mut buf := []u8{len: w * h * 4}
		for y in 0 .. h {
			for x in 0 .. w {
				v := f32(0.5 + 0.5 * math.sin(f64(f32(x) / 24.0 + math.sin(f64(f32(y) / 31.0)) * 2.0)) *
					math.cos(f64(f32(y) / 19.0)))
				c := lerp_color(black, white, v)
				i := (y * w + x) * 4
				unsafe {
					buf[i] = c.r
					buf[i + 1] = c.g
					buf[i + 2] = c.b
					buf[i + 3] = 255
				}
			}
		}
		app.images[7] = buf
		app.indices[7] = cache_rgba(app.ctx, w, h, buf)
	}

	// 8 cellular
	{
		mut buf := []u8{len: w * h * 4}
		for y in 0 .. h {
			for x in 0 .. w {
				cx := x / 32
				cy := y / 32
				n := (cx * 73 + cy * 151) % 256
				c := gg.Color{u8(n), u8(255 - n), u8((cx + cy) * 17), 255}
				i := (y * w + x) * 4
				unsafe {
					buf[i] = c.r
					buf[i + 1] = c.g
					buf[i + 2] = c.b
					buf[i + 3] = 255
				}
			}
		}
		app.images[8] = buf
		app.indices[8] = cache_rgba(app.ctx, w, h, buf)
	}
}

fn frame(data voidptr) {
	mut app := unsafe { &App(data) }
	app.ctx.begin()
	if app.current >= 0 && app.current < 9 && app.indices[app.current] >= 0 {
		app.ctx.draw_image_by_id(0, 0, app.img_w, app.img_h, app.indices[app.current])
	}
	app.ctx.draw_rect_filled(30, 400, 325, 30, gg.Color{135, 206, 235, 128})
	app.ctx.draw_rect_empty(30, 400, 325, 30, gg.Color{255, 255, 255, 128})
	app.ctx.draw_text(40, 410, 'MOUSE LEFT BUTTON to CYCLE PROCEDURAL TEXTURES',
		size:  10
		color: gen_gray
	)

	names := ['VERTICAL GRADIENT', 'HORIZONTAL GRADIENT', 'DIAGONAL GRADIENT', 'RADIAL GRADIENT',
		'SQUARE GRADIENT', 'CHECKED', 'WHITE NOISE', 'PERLIN NOISE', 'CELLULAR']
	c := if app.current == 6 || app.current == 7 {
		gg.red
	} else if app.current == 3 || app.current == 4 {
		gg.Color{211, 211, 211, 255}
	} else {
		gen_raywhite
	}
	app.ctx.draw_text(540, 10, names[app.current], size: 20, color: c)
	app.ctx.end()
}

fn on_event(e &gg.Event, data voidptr) {
	mut app := unsafe { &App(data) }
	if (e.typ == .mouse_down && e.mouse_button == .left)
		|| (e.typ == .key_down && e.key_code == .right) {
		app.current = (app.current + 1) % 9
	}
}

fn main() {
	mut app := &App{}
	app.ctx = gg.new_context(
		width:        800
		height:       450
		window_title: 'raylib [textures] example - image generation'
		user_data:    app
		init_fn:      init
		frame_fn:     frame
		event_fn:     on_event
		bg_color:     gen_raywhite
	)
	app.ctx.run()
}
