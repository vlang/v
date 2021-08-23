module main

import gg
import gx

const rate = f32(1) / 60 * 10

struct App {
mut:
	gg   &gg.Context
	anim &Anim
}

struct Anim {
mut:
	time    f32
	reverse bool
}

fn (mut anim Anim) advance() {
	if anim.reverse {
		anim.time -= 1 * rate
	} else {
		anim.time += 1 * rate
	}
	// Use some arbitrary value that fits 60 fps
	if anim.time > 80 * rate || anim.time < -80 * rate {
		anim.reverse = !anim.reverse
	}
}

fn main() {
	mut app := &App{
		gg: 0
		anim: &Anim{}
	}
	app.gg = gg.new_context(
		bg_color: gx.rgb(174, 198, 255)
		width: 600
		height: 400
		window_title: 'Animated cubic Bézier curve'
		frame_fn: frame
		user_data: app
	)
	app.gg.run()
}

fn frame(mut app App) {
	time := app.anim.time

	ctrl_p1_x := f32(200.0) + (40 * time)
	ctrl_p2_x := f32(400.0) + (-40 * time)

	p1_and_p2 := [f32(200.0), 200.0 + (10 * time), 400.0, 200.0 + (10 * time)]

	app.gg.begin()
	app.gg.draw_cubic_bezier(p1_and_p2, [ctrl_p1_x, 100.0, ctrl_p2_x, 100.0], gx.blue)
	app.gg.end()
	app.anim.advance()
}
