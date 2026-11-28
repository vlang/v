module main

// A small animation starter: easing curves, numeric and color tweens, a cue
// timeline and a damped spring. Run with: v run examples/gg/anim_base.v
import gg
import gg.anim
import math
import time

struct App {
mut:
	ctx         &gg.Context = unsafe { nil }
	x_tween     anim.Tween[f32]
	color_tween anim.Tween[gg.Color]
	spring      anim.Spring
	tl          anim.Timeline
	last        time.Time
	message     string
}

fn main() {
	mut app := &App{
		x_tween:     anim.new_tween(f32(50), f32(490), 2.0, anim.lerp_f32)
		color_tween: anim.new_tween(gg.rgb(255, 94, 77), gg.rgb(54, 166, 178), 2.0, anim.lerp_color)
		spring:      anim.new_spring(120, 300)
	}
	app.x_tween.easing = .ease_in_out_cubic
	app.x_tween.looping = true
	app.color_tween.easing = .ease_in_out_cubic
	app.color_tween.looping = true
	app.tl.add(1.0, 'one second')
	app.tl.add(2.0, 'two seconds')
	app.last = time.now()

	app.ctx = gg.new_context(
		width:        560
		height:       320
		window_title: 'gg animation base'
		bg_color:     gg.rgb(24, 26, 34)
		frame_fn:     frame
		user_data:    app
	)
	app.ctx.run()
}

fn frame(mut app App) {
	now := time.now()
	mut dt := f32((now - app.last).seconds())
	app.last = now
	if dt > 0.1 {
		dt = 0.1
	}

	app.x_tween.update(dt)
	app.color_tween.update(dt)
	app.spring.target = 260 + f32(math.sin(f64(app.tl.time))) * 120
	app.spring.update(dt)
	for cue in app.tl.update(dt) {
		app.message = cue
	}

	app.ctx.begin()
	// Background band colored by the same tween as the moving ball.
	band := anim.palette_gradient(anim.palette('ocean'), app.x_tween.progress())
	app.ctx.draw_rect_filled(0, 250, 560, 70, band)

	spring_y := app.spring.value
	app.ctx.draw_line(30, spring_y, 530, spring_y, gg.rgb(80, 90, 110))
	app.ctx.draw_circle_filled(app.x_tween.value(), 120, 26, app.color_tween.value())
	if app.message != '' {
		app.ctx.draw_text_def(20, 20, 'cue: ' + app.message)
	}

	// Show the full easing family as a row of small dots.
	mut x := f32(30)
	for curve in [
		anim.Easing.linear,
		anim.Easing.ease_in_out_cubic,
		anim.Easing.ease_out_back,
		anim.Easing.ease_in_out_bounce,
	] {
		y := 210 - anim.ease(app.x_tween.progress(), curve) * 120
		app.ctx.draw_circle_filled(x, y, 6, gg.white)
		x += 40
	}
	app.ctx.end()
}
