module main

import gg
import gx

struct App {
mut:
	gg     &gg.Context = unsafe { nil }
	pixels []f32
}

fn main() {
	mut pixels := []f32{}
	density := 4
	for x in 30 .. 60 {
		if x % density == 0 {
			continue
		}
		for y in 30 .. 60 {
			if y % density == 0 {
				continue
			}
			pixels << f32(x + density)
			pixels << f32(y + density)
		}
	}
	mut app := &App{
		gg: 0
		pixels: pixels
	}
	app.gg = gg.new_context(
		bg_color: gx.rgb(174, 198, 255)
		width: 100
		height: 100
		window_title: 'Set Pixels'
		frame_fn: frame
		user_data: app
	)
	app.gg.run()
}

fn frame(mut app App) {
	app.gg.begin()

	// Draw a blue pixel near each corner. (Find your magnifying glass)
	app.gg.draw_pixel(2, 2, gx.blue)
	app.gg.draw_pixel(app.gg.width - 2, 2, gx.blue)
	app.gg.draw_pixel(app.gg.width - 2, app.gg.height - 2, gx.blue)
	app.gg.draw_pixel(2, app.gg.height - 2, gx.blue)

	// Draw pixels in a grid-like pattern.
	app.gg.draw_pixels(app.pixels, gx.red)
	app.gg.end()
}
