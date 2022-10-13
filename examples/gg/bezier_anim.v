module main

import gg
import gx

const rate = f32(1) / 60 * 10

struct App {
mut:
	gg   &gg.Context = unsafe { nil }
	anim &Anim       = unsafe { nil }
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

	p1_x := f32(200.0)
	p1_y := f32(200.0) + (10 * time)

	p2_x := f32(400.0)
	p2_y := f32(200.0) + (10 * time)

	ctrl_p1_x := f32(200.0) + (40 * time)
	ctrl_p1_y := f32(100.0)
	ctrl_p2_x := f32(400.0) + (-40 * time)
	ctrl_p2_y := f32(100.0)

	points := [p1_x, p1_y, ctrl_p1_x, ctrl_p1_y, ctrl_p2_x, ctrl_p2_y, p2_x, p2_y]

	app.gg.begin()
	app.gg.draw_cubic_bezier(points, gx.blue)
	app.gg.end()
	app.anim.advance()
}
