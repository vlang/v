import gg
import rand

// The flags here override the default limits for Sokol
#flag -D_SGL_DEFAULT_MAX_VERTICES=4194304
#flag -D_SGL_DEFAULT_MAX_COMMANDS=65536

// Without the flags, `max_circles` > 5040, will just show a blue screen without
// *any circles* drawn.
// **Note** however, that increasing `_SGL_DEFAULT_MAX_VERTICES`, also increases
// the default RAM usage of your app. In this case, instead of using ~40MB on
// Ubuntu 20.04, the app instead uses ~140MB.
const max_circles = 10_000

fn main() {
	gg.start(
		window_title: 'Hello'
		bg_color:     gg.Color{50, 50, 150, 255}
		width:        800
		height:       600
		frame_fn:     fn (ctx &gg.Context) {
			wsize := gg.window_size()
			ctx.begin()
			for _ in 0 .. max_circles {
				rx := rand.int_in_range(0, wsize.width) or { 0 }
				ry := rand.int_in_range(0, wsize.height) or { 0 }
				cr := rand.u8()
				cg := rand.u8()
				cb := rand.u8()
				ctx.draw_circle_filled(rx, ry, 10, gg.Color{cr, cg, cb, 255})
			}
			ctx.show_fps()
			ctx.end()
		}
	)
}
