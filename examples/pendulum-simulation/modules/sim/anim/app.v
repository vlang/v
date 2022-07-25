module anim

import gg
import gx
import sim
import sim.args as simargs

const bg_color = gx.white

struct Pixel {
	x     f32
	y     f32
	color gx.Color
}

struct App {
pub:
	args         simargs.ParallelArgs
	request_chan chan &sim.SimRequest
	result_chan  chan &sim.SimResult
pub mut:
	gg     &gg.Context = unsafe { 0 }
	iidx   int
	pixels []u32
}

pub fn new_app(args simargs.ParallelArgs) &App {
	total_pixels := args.grid.height * args.grid.width

	mut app := &App{
		args: args
		pixels: []u32{len: total_pixels}
		request_chan: chan &sim.SimRequest{cap: args.grid.width}
	}
	app.gg = gg.new_context(
		width: args.grid.width
		height: args.grid.height
		create_window: true
		window_title: 'V Pendulum Simulation'
		user_data: app
		bg_color: anim.bg_color
		frame_fn: frame
		init_fn: init
	)
	return app
}

fn init(mut app App) {
	app.iidx = app.gg.new_streaming_image(app.args.grid.width, app.args.grid.height, 4,
		pixel_format: .rgba8)
	go pixels_worker(mut app)
}

fn frame(mut app App) {
	app.gg.begin()
	app.draw()
	app.gg.end()
}

fn (mut app App) draw() {
	mut istream_image := app.gg.get_cached_image_by_idx(app.iidx)
	istream_image.update_pixel_data(&app.pixels[0])
	app.gg.draw_image(0, 0, app.args.grid.width, app.args.grid.height, istream_image)
}
