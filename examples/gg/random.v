import gg
import time

const pwidth = 800

const pheight = 600

const pbytes = 4

struct AppState {
mut:
	gg          &gg.Context = unsafe { nil }
	istream_idx int
	pixels      [pheight][pwidth]u32
}

[direct_array_access]
fn (mut state AppState) update() {
	mut rcolor := u64(state.gg.frame)
	for {
		for y in 0 .. pheight {
			for x in 0 .. pwidth {
				rcolor = rcolor * 1664525 + 1013904223
				state.pixels[y][x] = u32(rcolor & 0x0000_0000_FFFF_FFFF) | 0x1010AFFF
			}
		}
		time.sleep(33 * time.millisecond)
	}
}

fn (mut state AppState) draw() {
	mut istream_image := state.gg.get_cached_image_by_idx(state.istream_idx)
	istream_image.update_pixel_data(&state.pixels)
	size := gg.window_size()
	state.gg.draw_image(0, 0, size.width, size.height, istream_image)
}

// gg callbacks:

fn graphics_init(mut state AppState) {
	state.istream_idx = state.gg.new_streaming_image(pwidth, pheight, pbytes, pixel_format: .rgba8)
}

fn graphics_frame(mut state AppState) {
	state.gg.begin()
	state.draw()
	state.gg.end()
}

fn main() {
	mut state := &AppState{}
	state.gg = gg.new_context(
		width: 800
		height: 600
		create_window: true
		window_title: 'Random Static'
		init_fn: graphics_init
		frame_fn: graphics_frame
		user_data: state
	)
	go state.update()
	state.gg.run()
}
