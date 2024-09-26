module main

import gg
import gx
import automaton

const screen_width = 800
const screen_height = 600
const filled_color = gx.blue

@[live]
fn print_automaton(app &App) {
	square_size := 18
	for y := 1; y < app.a.field.maxy; y++ {
		for x := 1; x < app.a.field.maxx; x++ {
			cell := app.a.field.get(x, y)
			if cell == 1 {
				app.gg.draw_rect_filled(f32(square_size * x), f32(square_size * y), f32(square_size),
					f32(square_size), filled_color)
			}
		}
	}
}

struct App {
mut:
	gg &gg.Context = unsafe { nil }
	a  automaton.Automaton
}

fn frame(mut app App) {
	app.gg.begin()
	app.a.update()
	print_automaton(app)
	app.gg.end()
}

fn main() {
	mut app := App{
		a: automaton.gun()
	}
	app.gg = gg.new_context(
		bg_color:      gx.white
		frame_fn:      frame
		user_data:     &app
		width:         screen_width
		height:        screen_height
		create_window: true
		resizable:     false
		window_title:  'v life (with gg, gx)'
	)
	app.gg.run()
}
