module main

// Port of raylib textures_image_processing.c
// PORT NOTE: gg has no ImageColor* CPU helpers. The colour operations are
// implemented directly on the RGBA buffer. Gaussian blur uses a separable box
// blur approximation instead of raylib's true gaussian kernel.
import gg

const proc_raywhite = gg.Color{245, 245, 245, 255}
const proc_darkgray = gg.Color{80, 80, 80, 255}
const proc_skyblue = gg.Color{135, 206, 235, 255}
const proc_darkblue = gg.Color{0, 121, 241, 255}
const proc_gray = gg.Color{128, 128, 128, 255}
const proc_blue = gg.Color{0, 121, 241, 255}

const process_text = ['NO PROCESSING', 'COLOR GRAYSCALE', 'COLOR TINT', 'COLOR INVERT', 'COLOR CONTRAST',
	'COLOR BRIGHTNESS', 'GAUSSIAN BLUR', 'FLIP VERTICAL', 'FLIP HORIZONTAL']

fn cache_rgba(ctx &gg.Context, w int, h int, buf []u8) int {
	mut img := gg.Image{ width: w, height: h, nr_channels: 4, ok: true, data: buf.data }
	img.init_sokol_image()
	return ctx.cache_image(img)
}

fn apply_process(work []u8, orig []u8, w int, h int, mode int) {
	if mode == 0 {
		unsafe { for i in 0 .. work.len { work[i] = orig[i] }
		 }
		return
	}
	if mode == 7 || mode == 8 {
		unsafe { for i in 0 .. work.len { work[i] = orig[i] }
		 }
		for y in 0 .. h {
			for x in 0 .. w {
				sx := if mode == 7 { x } else { w - 1 - x }
				sy := if mode == 7 { h - 1 - y } else { y }
				si := (sy * w + sx) * 4
				di := (y * w + x) * 4
				unsafe {
					work[di] = orig[si]
					work[di + 1] = orig[si + 1]
					work[di + 2] = orig[si + 2]
					work[di + 3] = orig[si + 3]
				}
			}
		}
		return
	}
	if mode == 6 {
		// Separable box blur, repeated to approximate raylib's gaussian blur.
		mut src := orig.clone()
		for pass in 0 .. 3 {
			for y in 0 .. h {
				for x in 0 .. w {
					mut r := 0
					mut g := 0
					mut b := 0
					mut n := 0
					for k := -2; k <= 2; k++ {
						xx := x + k
						if xx < 0 || xx >= w {
							continue
						}
						i := (y * w + xx) * 4
						unsafe {
							r += int(src[i])
							g += int(src[i + 1])
							b += int(src[i + 2])
						}
						n++
					}
					i := (y * w + x) * 4
					unsafe {
						work[i] = u8(r / n)
						work[i + 1] = u8(g / n)
						work[i + 2] = u8(b / n)
						work[i + 3] = 255
					}
				}
			}
			if pass < 2 {
				unsafe { for i in 0 .. work.len { src[i] = work[i] }
				 }
			}
		}
		return
	}
	for i in 0 .. w * h {
		p := i * 4
		mut r := int(orig[p])
		mut g := int(orig[p + 1])
		mut b := int(orig[p + 2])
		match mode {
			1 {
				lum := int(0.299 * f32(r) + 0.587 * f32(g) + 0.114 * f32(b))
				r = lum
				g = lum
				b = lum
			}
			2 {
				g = int(f32(g) * 0.8)
				b = int(f32(b) * 0.2)
			}
			3 {
				r = 255 - r
				g = 255 - g
				b = 255 - b
			}
			4 {
				f := (259.0 * (f32(r) - 40 + 255.0)) / (255.0 * (259.0 - (f32(r) - 40)))
				r = int(f32(r) + (f - 1.0) * f32(r))
				g = int(f32(g) + (f - 1.0) * f32(g))
				b = int(f32(b) + (f - 1.0) * f32(b))
			}
			5 {
				r += 80
				g += 80
				b += 80
			}
			else {}
		}
		unsafe {
			work[p] = u8(r)
			work[p + 1] = u8(g)
			work[p + 2] = u8(b)
			work[p + 3] = 255
		}
	}
}

struct App {
mut:
	ctx     &gg.Context = unsafe { nil }
	idx     int         = -1
	orig    []u8
	work    []u8
	w       int
	h       int
	current int
	hover   int = -1
	reload  bool
}

fn rebuild(mut app &App) {
	apply_process(app.work, app.orig, app.w, app.h, app.current)
	app.ctx.update_pixel_data(app.idx, app.work.data)
	app.reload = false
}

fn init(data voidptr) {
	mut app := unsafe { &App(data) }
	img := app.ctx.create_image('resources/parrots.png') or { gg.Image{} }
	app.w = img.width
	app.h = img.height
	if img.data != unsafe { nil } && app.w > 0 {
		app.orig = unsafe { (&u8(img.data)).vbytes(app.w * app.h * 4).clone() }
		app.work = []u8{len: app.orig.len}
		app.idx = cache_rgba(app.ctx, app.w, app.h, app.work)
		rebuild(app)
	}
}

fn frame(data voidptr) {
	mut app := unsafe { &App(data) }
	if app.reload {
		rebuild(app)
	}
	app.ctx.begin()
	app.ctx.draw_text(40, 30, 'IMAGE PROCESSING:', size: 10, color: proc_darkgray)
	for i in 0 .. 9 {
		y := 50 + 32 * i
		active := i == app.current || i == app.hover
		c := if active { proc_skyblue } else { gg.Color{211, 211, 211, 255} }
		outline := if active { proc_blue } else { proc_gray }
		app.ctx.draw_rect_filled(40, f32(y), 150, 30, c)
		app.ctx.draw_rect_empty(40, f32(y), 150, 30, outline)
		app.ctx.draw_text(40 + 75 - app.ctx.text_width(process_text[i]) / 2, y + 11,
			process_text[i],
			size:  10
			color: if active { proc_darkblue } else { proc_darkgray }
		)
	}
	if app.idx >= 0 {
		app.ctx.draw_image_by_id(f32(800 - app.w - 60), f32(450 / 2 - app.h / 2), app.w, app.h, app.idx)
	}
	app.ctx.end()
}

fn on_event(e &gg.Event, data voidptr) {
	mut app := unsafe { &App(data) }
	if e.typ == .key_down {
		if e.key_code == .down {
			app.current = (app.current + 1) % 9
			app.reload = true
		} else if e.key_code == .up {
			app.current = (app.current + 8) % 9
			app.reload = true
		}
	}
	if e.typ == .mouse_button_up && e.mouse_button == .left && app.hover >= 0 {
		app.current = app.hover
		app.reload = true
	}
}

fn update(dt f32, data voidptr) {
	mut app := unsafe { &App(data) }
	_ = dt
	mx := f32(app.ctx.mouse_pos_x)
	my := f32(app.ctx.mouse_pos_y)
	app.hover = -1
	for i in 0 .. 9 {
		y := f32(50 + 32 * i)
		if mx >= 40 && mx <= 190 && my >= y && my <= y + 30 {
			app.hover = i
			break
		}
	}
}

fn main() {
	mut app := &App{}
	app.ctx = gg.new_context(
		width:        800
		height:       450
		window_title: 'raylib [textures] example - image processing'
		user_data:    app
		init_fn:      init
		frame_fn:     frame
		event_fn:     on_event
		update_fn:    update
		bg_color:     proc_raywhite
	)
	app.ctx.run()
}
