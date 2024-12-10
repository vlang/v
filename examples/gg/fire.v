// Use `v -d show_fps run examples/gg/fire.v` to show a fire effect with an FPS counter.
import gg
import gx
import rand

const win_width = 800
const win_height = 600
const width = 100
const height = 140
const scale = 4
const palette = [
	gx.rgb(0x07, 0x07, 0x07),
	gx.rgb(0x1f, 0x07, 0x07),
	gx.rgb(0x2f, 0x0f, 0x07),
	gx.rgb(0x47, 0x0f, 0x07),
	gx.rgb(0x57, 0x17, 0x07),
	gx.rgb(0x67, 0x1f, 0x07),
	gx.rgb(0x77, 0x1f, 0x07),
	gx.rgb(0x8f, 0x27, 0x07),
	gx.rgb(0x9f, 0x2f, 0x07),
	gx.rgb(0xaf, 0x3f, 0x07),
	gx.rgb(0xbf, 0x47, 0x07),
	gx.rgb(0xc7, 0x47, 0x07),
	gx.rgb(0xdf, 0x4f, 0x07),
	gx.rgb(0xdf, 0x57, 0x07),
	gx.rgb(0xdf, 0x57, 0x07),
	gx.rgb(0xd7, 0x5f, 0x07),
	gx.rgb(0xd7, 0x5f, 0x07),
	gx.rgb(0xd7, 0x67, 0x0f),
	gx.rgb(0xcf, 0x6f, 0x0f),
	gx.rgb(0xcf, 0x77, 0x0f),
	gx.rgb(0xcf, 0x7f, 0x0f),
	gx.rgb(0xcf, 0x87, 0x17),
	gx.rgb(0xc7, 0x87, 0x17),
	gx.rgb(0xc7, 0x8f, 0x17),
	gx.rgb(0xc7, 0x97, 0x1f),
	gx.rgb(0xbf, 0x9f, 0x1f),
	gx.rgb(0xbf, 0x9f, 0x1f),
	gx.rgb(0xbf, 0xa7, 0x27),
	gx.rgb(0xbf, 0xa7, 0x27),
	gx.rgb(0xbf, 0xaf, 0x2f),
	gx.rgb(0xb7, 0xaf, 0x2f),
	gx.rgb(0xb7, 0xb7, 0x2f),
	gx.rgb(0xb7, 0xb7, 0x37),
	gx.rgb(0xcf, 0xcf, 0x6f),
	gx.rgb(0xdf, 0xdf, 0x9f),
	gx.rgb(0xef, 0xef, 0xc7),
	gx.rgb(0xff, 0xff, 0xff),
]!

struct App {
mut:
	gg    &gg.Context = unsafe { nil }
	buf   [][]int
	dying bool
	tiles int
}

fn main() {
	mut app := &App{
		buf: [][]int{len: height - 1, init: []int{len: width}}
	}
	app.buf << []int{len: width, init: 36} // white fire base

	app.gg = gg.new_context(
		event_fn:     event
		frame_fn:     frame
		init_fn:      retile
		window_title: 'Fire Animation'
		user_data:    app
		bg_color:     palette[0]
		width:        win_width
		height:       win_height
	)
	app.gg.run()
}

fn retile(mut app App) {
	size := app.gg.window_size()
	app.tiles = size.width / (width * scale) + 1
}

fn (mut app App) draw() {
	size := app.gg.window_size()
	for t in 0 .. app.tiles {
		app.gg.begin()
		for y, row in app.buf {
			for x, i in row {
				app.gg.draw_square_filled(t * width * scale + x * scale,
					(size.height - height * scale) + y * scale, scale, palette[i])
			}
		}
		app.gg.end(
			how: if t == 0 { .clear } else { .passthru }
		)
		// one pass per tile to avoid sgl vertex limit
	}
}

@[direct_array_access]
fn (mut app App) tick() {
	for x in 0 .. width {
		for y in 1 .. height {
			if app.buf[y][x] > 0 {
				r := rand.intn(4) or { panic('failed to generate random number') }
				xn := ((x - r + 1) % width + width) % width
				app.buf[y - 1][xn] = app.buf[y][x] - r % 2
			} else {
				app.buf[y - 1][x] = 0
			}
		}
	}
	if app.dying {
		for x in 0 .. width {
			y := height - 1
			app.buf[y][x] -= rand.intn(4) or { panic('failed to generate random number') }
			if app.buf[y][x] < 0 {
				app.buf[y][x] = 0
			}
		}
	}
}

fn event(event &gg.Event, mut app App) {
	match event.typ {
		.key_up {
			match event.key_code {
				.space {
					if app.dying {
						app.dying = false
						app.buf[height - 1] = []int{len: width, init: 36}
					} else {
						app.dying = true
					}
				}
				else {}
			}
		}
		.resized {
			retile(mut app)
		}
		else {}
	}
}

fn frame(mut app App) {
	app.draw()
	app.tick()
}
