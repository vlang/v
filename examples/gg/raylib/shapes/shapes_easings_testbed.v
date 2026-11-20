module main

// raylib [shapes] example - easings testbed, ported to gg.
import gg
import math.easing

const easing_names = [
	'EaseLinearNone',
	'EaseLinearIn',
	'EaseLinearOut',
	'EaseLinearInOut',
	'EaseSineIn',
	'EaseSineOut',
	'EaseSineInOut',
	'EaseCircIn',
	'EaseCircOut',
	'EaseCircInOut',
	'EaseCubicIn',
	'EaseCubicOut',
	'EaseCubicInOut',
	'EaseQuadIn',
	'EaseQuadOut',
	'EaseQuadInOut',
	'EaseExpoIn',
	'EaseExpoOut',
	'EaseExpoInOut',
	'EaseBackIn',
	'EaseBackOut',
	'EaseBackInOut',
	'EaseBounceOut',
	'EaseBounceIn',
	'EaseBounceInOut',
	'EaseElasticIn',
	'EaseElasticOut',
	'EaseElasticInOut',
	'None',
]!
const raywhite = gg.Color{245, 245, 245, 255}
const lightgray = gg.Color{200, 200, 200, 255}
const maroon = gg.Color{190, 33, 55, 255}

fn apply_ease(idx int, t f64) f64 {
	if idx == 28 {
		return 0
	}
	return match idx {
		0 { easing.linear(t) }
		1 { easing.linear(t) }
		2 { easing.linear(t) }
		3 { easing.linear(t) }
		4 { easing.in_sine(t) }
		5 { easing.out_sine(t) }
		6 { easing.in_out_sine(t) }
		7 { easing.in_circ(t) }
		8 { easing.out_circ(t) }
		9 { easing.in_out_circ(t) }
		10 { easing.in_cubic(t) }
		11 { easing.out_cubic(t) }
		12 { easing.in_out_cubic(t) }
		13 { easing.in_quad(t) }
		14 { easing.out_quad(t) }
		15 { easing.in_out_quad(t) }
		16 { easing.in_expo(t) }
		17 { easing.out_expo(t) }
		18 { easing.in_out_expo(t) }
		19 { easing.in_back(t) }
		20 { easing.out_back(t) }
		21 { easing.in_out_back(t) }
		22 { easing.out_bounce(t) }
		23 { easing.in_bounce(t) }
		24 { easing.in_out_bounce(t) }
		25 { easing.in_elastic(t) }
		26 { easing.out_elastic(t) }
		27 { easing.in_out_elastic(t) }
		else { 0.0 }
	}
}

struct App {
mut:
	ctx     &gg.Context = unsafe { nil }
	bx      f32         = 100
	by      f32         = 100
	t       f32
	d       f32  = 300
	paused  bool = true
	bounded bool = true
	ease_x  int  = 28
	ease_y  int  = 28
}

fn on_frame(data voidptr) {
	mut app := unsafe { &App(data) }
	ctx := app.ctx
	ctx.begin()
	ctx.draw_text(20, 20, 'Easing x: ${easing_names[app.ease_x]}', size: 20, color: lightgray)
	ctx.draw_text(20, 40, 'Easing y: ${easing_names[app.ease_y]}', size: 20, color: lightgray)
	mode := if app.bounded { 'b' } else { 'u' }
	ctx.draw_text(20, 60, 't (${mode}) = ${app.t:.2f} d = ${app.d:.2f}',
		size:  20
		color: lightgray
	)
	ctx.draw_text(20, ctx.height - 40, 'Use ENTER to play or pause movement, use SPACE to restart',
		size:  20
		color: lightgray
	)
	ctx.draw_text(20, ctx.height - 60, 'Use Q and W or A and S keys to change duration',
		size:  20
		color: lightgray
	)
	ctx.draw_text(20, ctx.height - 80, 'Use LEFT or RIGHT keys to choose easing for the x axis',
		size:  20
		color: lightgray
	)
	ctx.draw_text(20, ctx.height - 100, 'Use UP or DOWN keys to choose easing for the y axis',
		size:  20
		color: lightgray
	)
	ctx.draw_circle_filled(app.bx, app.by, 16, maroon)
	ctx.end()
}

fn on_event(e &gg.Event, data voidptr) {
	mut app := unsafe { &App(data) }
	if e.typ != .key_down {
		return
	}
	match e.key_code {
		.t {
			app.bounded = !app.bounded
			app.reset()
		}
		.right {
			app.ease_x++
			if app.ease_x > 28 { app.ease_x = 0 }
			app.reset()
		}
		.left {
			if app.ease_x == 0 { app.ease_x = 28 } else { app.ease_x-- }
			app.reset()
		}
		.down {
			app.ease_y++
			if app.ease_y > 28 { app.ease_y = 0 }
			app.reset()
		}
		.up {
			if app.ease_y == 0 { app.ease_y = 28 } else { app.ease_y-- }
			app.reset()
		}
		.w {
			app.d += 20
			app.reset()
		}
		.q {
			app.d -= 20
			app.reset()
		}
		.enter { app.paused = !app.paused }
		.space { app.reset() }
		else {}
	}
}

fn (mut app App) reset() {
	app.t = 0
	app.bx = 100
	app.by = 100
	app.paused = true
}

fn on_update(dt f32, data voidptr) {
	mut app := unsafe { &App(data) }
	_ = dt
	if app.ctx.pressed_keys[gg.KeyCode.s] && app.d < 9998 {
		app.d += 2
		app.reset()
	}
	if app.ctx.pressed_keys[gg.KeyCode.a] && app.d > 2 {
		app.d -= 2
		app.reset()
	}
	if !app.paused && ((app.bounded && app.t < app.d) || !app.bounded) {
		nt := f64(app.t) / f64(app.d)
		app.bx = 100 + f32(apply_ease(app.ease_x, nt)) * 530
		app.by = 100 + f32(apply_ease(app.ease_y, nt)) * 230
		app.t++
	}
}

fn main() {
	mut app := &App{}
	app.ctx = gg.new_context(
		width:        800
		height:       450
		window_title: 'raylib [shapes] example - easings testbed'
		user_data:    app
		frame_fn:     on_frame
		event_fn:     on_event
		update_fn:    on_update
		bg_color:     raywhite
	)
	app.ctx.run()
}
