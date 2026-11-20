module main

// Port of raylib textures_image_kernel.c
// PORT NOTE: gg has no ImageKernelConvolution(); the three convolution kernels
// are applied directly to the RGBA buffers here.
import gg

const kernel_raywhite = gg.Color{245, 245, 245, 255}

fn cache_rgba(ctx &gg.Context, w int, h int, buf []u8) int {
	mut img := gg.Image{ width: w, height: h, nr_channels: 4, ok: true, data: buf.data }
	img.init_sokol_image()
	return ctx.cache_image(img)
}

fn convolve(src []u8, out []u8, w int, h int, kernel []f32) {
	for y in 0 .. h {
		for x in 0 .. w {
			mut r := f32(0)
			mut g := f32(0)
			mut b := f32(0)
			for ky in 0 .. 3 {
				for kx in 0 .. 3 {
					sx := x + kx - 1
					sy := y + ky - 1
					if sx < 0 || sy < 0 || sx >= w || sy >= h {
						continue
					}
					weight := kernel[ky * 3 + kx]
					i := (sy * w + sx) * 4
					unsafe {
						r += weight * f32(src[i])
						g += weight * f32(src[i + 1])
						b += weight * f32(src[i + 2])
					}
				}
			}
			i := (y * w + x) * 4
			unsafe {
				out[i] = u8(r)
				out[i + 1] = u8(g)
				out[i + 2] = u8(b)
				out[i + 3] = 255
			}
		}
	}
}

fn normalize(kernel []f32) {
	mut sum := f32(0)
	for v in kernel {
		sum += v
	}
	if sum != 0 {
		for i in 0 .. kernel.len {
			kernel[i] /= sum
		}
	}
}

struct App {
mut:
	ctx   &gg.Context = unsafe { nil }
	idx   [4]int
	store [4][]u8
}

fn init(data voidptr) {
	mut app := unsafe { &App(data) }
	cat := app.ctx.create_image('resources/cat.png') or { gg.Image{} }
	if cat.data == unsafe { nil } || cat.width <= 0 {
		return
	}
	w := cat.width
	h := cat.height
	orig := unsafe { (&u8(cat.data)).vbytes(w * h * 4).clone() }

	gaussian := [f32(1), 2, 1, 2, 4, 2, 1, 2, 1]
	sobel := [f32(1), 0, -1, 2, 0, -2, 1, 0, -1]
	sharpen := [f32(0), -1, 0, -1, 5, -1, 0, -1, 0]
	normalize(mut gaussian)
	normalize(mut sobel)
	normalize(mut sharpen)

	// sharp, sobel, gaussian (6 passes), original
	app.store[0] = []u8{len: w * h * 4}
	convolve(orig, app.store[0], w, h, sharpen)
	app.store[1] = []u8{len: w * h * 4}
	convolve(orig, app.store[1], w, h, sobel)
	app.store[2] = []u8{len: w * h * 4}
	mut tmp := orig.clone()
	mut next := []u8{len: w * h * 4}
	for _ in 0 .. 6 {
		convolve(tmp, next, w, h, gaussian)
		unsafe { for i in 0 .. tmp.len { tmp[i] = next[i] }
		 }
	}
	unsafe { for i in 0 .. app.store[2].len { app.store[2][i] = tmp[i] }
	 }
	app.store[3] = orig.clone()

	for i in 0 .. 4 {
		crop_w := if w < 200 { w } else { 200 }
		crop_h := if h < 450 { h } else { 450 }
		mut cropped := []u8{len: crop_w * crop_h * 4}
		for y in 0 .. crop_h {
			for x in 0 .. crop_w {
				s := (y * w + x) * 4
				d := (y * crop_w + x) * 4
				unsafe {
					cropped[d] = app.store[i][s]
					cropped[d + 1] = app.store[i][s + 1]
					cropped[d + 2] = app.store[i][s + 2]
					cropped[d + 3] = app.store[i][s + 3]
				}
			}
		}
		app.idx[i] = cache_rgba(app.ctx, crop_w, crop_h, cropped)
	}
}

fn frame(data voidptr) {
	mut app := unsafe { &App(data) }
	app.ctx.begin()
	for i in 0 .. 4 {
		if app.idx[i] >= 0 {
			app.ctx.draw_image_by_id(f32(i * 200), 0, 200, 450, app.idx[i])
		}
	}
	app.ctx.end()
}

fn main() {
	mut app := &App{}
	app.ctx = gg.new_context(
		width:        800
		height:       450
		window_title: 'raylib [textures] example - image kernel'
		user_data:    app
		init_fn:      init
		frame_fn:     frame
		bg_color:     kernel_raywhite
	)
	app.ctx.run()
}
