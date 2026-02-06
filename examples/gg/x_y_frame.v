import gg
import time

const pwidth = 640
const pheight = 480

struct AppState {
mut:
	gg          &gg.Context = unsafe { nil }
	istream_idx int
	pixels      [pheight][pwidth]u32
}

@[direct_array_access]
fn (mut state AppState) update() {
	mut rcolor := u32(0)
	for {
		for y in 0 .. pheight {
			for x in 0 .. pwidth {
				rcolor = x * y * u32(state.gg.frame)
				state.pixels[y][x] = u32(rcolor) | 0xFF_00_00_00 // set the alpha channel to 255
			}
		}
		time.sleep(16 * time.millisecond)
	}
}

fn graphics_init(mut state AppState) {
	state.istream_idx = state.gg.new_streaming_image(pwidth, pheight, 4, pixel_format: .rgba8)
}

fn graphics_frame(mut state AppState) {
	state.gg.begin()
	mut istream_image := state.gg.get_cached_image_by_idx(state.istream_idx)
	istream_image.update_pixel_data(unsafe { &u8(&state.pixels) })
	size := gg.window_size()
	state.gg.draw_image(0, 0, size.width, size.height, istream_image)
	state.gg.end()
}

mut state := &AppState{}
state.gg = gg.new_context(
	width:        pwidth
	height:       pheight
	window_title: 'x*y*frame'
	user_data:    state
	init_fn:      graphics_init
	frame_fn:     graphics_frame
)
spawn state.update()
state.gg.run()
