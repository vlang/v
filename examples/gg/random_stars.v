import gg
import math
import time
import rand

const pwidth = 800

const pheight = 600

struct AppState {
mut:
	gg          &gg.Context = unsafe { nil }
	istream_idx int
	pixels      [pheight][pwidth]gg.Color
}

@[direct_array_access]
fn (mut state AppState) update() {
	for {
		unsafe { vmemset(&state.pixels, 0, pwidth * pheight * sizeof[gg.Color]()) }
		state.draw_sky() or {}
		time.sleep(30_000 * time.millisecond)
	}
}

fn (mut state AppState) draw_sky() ! {
	for _ in 0 .. 2000 {
		mut star_size := 1
		for rand.i32_in_range(0, 100)! < 5 {
			star_size += 10
		}
		for rand.i32_in_range(0, 100000)! < 5 {
			star_size += 85
		}
		sx := if star_size < 10 {
			rand.i32_in_range(-40, pwidth + 40)!
		} else {
			rand.i32_in_range(40, pwidth - 40)!
		}
		sy := if star_size < 10 {
			rand.i32_in_range(-40, pheight + 40)!
		} else {
			rand.i32_in_range(40, pheight - 40)!
		}
		state.draw_star(sx, sy, gg.Color{
			r: u8(rand.i32_in_range(50, 255)!)
			g: u8(rand.i32_in_range(50, 255)!)
			b: u8(rand.i32_in_range(50, 255)!)
		}, star_size)
	}
}

@[direct_array_access]
fn (mut state AppState) draw_star(x int, y int, c gg.Color, radius int) {
	if radius == 0 {
		return
	}
	minx := math.max(0, x - radius)
	miny := math.max(0, y - radius)
	maxx := math.min(pwidth, x + radius)
	maxy := math.min(pheight, y + radius)
	for cx in minx .. maxx {
		for cy in miny .. maxy {
			dx := math.abs[f32](cx - x) / f32(radius)
			dy := math.abs[f32](cy - y) / f32(radius)
			gradient := math.max[f32](0, math.min[f32](1, 1 - (math.sqrtf(dx) + math.sqrtf(dy))))
			if gradient < 0.01 {
				continue
			}
			mut pixel := unsafe { &state.pixels[cy][cx] }
			color := gg.Color{
				r: u8(math.min(255, int(pixel.r) + int(f32(c.r) * gradient)))
				g: u8(math.min(255, int(pixel.g) + int(f32(c.g) * gradient)))
				b: u8(math.min(255, int(pixel.b) + int(f32(c.b) * gradient)))
				a: 255
			}
			unsafe {
				*pixel = color
			}
		}
	}
}

fn (mut state AppState) draw() {
	mut istream_image := state.gg.get_cached_image_by_idx(state.istream_idx)
	istream_image.update_pixel_data(unsafe { &u8(&state.pixels) })
	size := gg.window_size()
	sx := -50 + 50 * math.sinf(f32(state.gg.frame) / 100)
	sy := -50 + 50 * math.cosf(f32(state.gg.frame) / 100)
	scale := 200 + 50 * math.sinf(f32(state.gg.frame) / 300)
	wx := size.width + scale
	wy := size.height + scale
	state.gg.draw_image(sx, sy, wx, wy, istream_image)
}

fn graphics_init(mut state AppState) {
	state.istream_idx = state.gg.new_streaming_image(pwidth, pheight, 4, pixel_format: .rgba8)
}

fn graphics_frame(mut state AppState) {
	state.gg.begin()
	state.draw()
	state.gg.end()
}

fn main() {
	mut state := &AppState{}
	state.gg = gg.new_context(
		width:        pwidth
		height:       pheight
		window_title: 'Random stars'
		init_fn:      graphics_init
		frame_fn:     graphics_frame
		user_data:    state
	)
	spawn state.update()
	state.gg.run()
}
