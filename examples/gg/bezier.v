module main

import gg
import gx

const points = [f32(200.0), 200.0, 200.0, 100.0, 400.0, 100.0, 400.0, 300.0]

struct App {
mut:
	gg    &gg.Context = unsafe { nil }
	steps int         = 30
}

fn main() {
	mut app := &App{}
	app.gg = gg.new_context(
		bg_color:     gx.rgb(174, 198, 255)
		width:        600
		height:       400
		window_title: 'Cubic Bézier curve'
		frame_fn:     frame
		user_data:    app
	)
	app.gg.run()
}

fn (mut app App) change(delta int) {
	app.steps += delta
	println('app.steps: ${app.steps}')
}

fn frame(mut app App) {
	app.gg.begin()
	app.gg.draw_cubic_bezier_in_steps(points, u32(app.steps), gx.blue)
	app.gg.draw_cubic_bezier_recursive(points, gx.rgba(255, 50, 50, 150))
	app.gg.end()
	if app.gg.pressed_keys[int(gg.KeyCode.down)] {
		app.change(-1)
	}
	if app.gg.pressed_keys[int(gg.KeyCode.up)] {
		app.change(1)
	}
}
