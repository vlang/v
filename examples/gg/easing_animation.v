module main

import gg
import gx
import math.easing

const all = {
	'in_functions':     {
		'linear':     voidptr(easing.linear)
		'in_sine':    easing.in_sine
		'in_quad':    easing.in_quad
		'in_cubic':   easing.in_cubic
		'in_quart':   easing.in_quart
		'in_quint':   easing.in_quint
		'in_expo':    easing.in_expo
		'in_circ':    easing.in_circ
		'in_back':    easing.in_back
		'in_elastic': easing.in_elastic
		'in_bounce':  easing.in_bounce
	}
	'out_functions':    {
		'linear':      voidptr(easing.linear)
		'out_sine':    easing.out_sine
		'out_quad':    easing.out_quad
		'out_cubic':   easing.out_cubic
		'out_quart':   easing.out_quart
		'out_quint':   easing.out_quint
		'out_expo':    easing.out_expo
		'out_circ':    easing.out_circ
		'out_back':    easing.out_back
		'out_elastic': easing.out_elastic
		'out_bounce':  easing.out_bounce
	}
	'in_out_functions': {
		'linear':         voidptr(easing.linear)
		'in_out_sine':    easing.in_out_sine
		'in_out_quad':    easing.in_out_quad
		'in_out_cubic':   easing.in_out_cubic
		'in_out_quart':   easing.in_out_quart
		'in_out_quint':   easing.in_out_quint
		'in_out_expo':    easing.in_out_expo
		'in_out_circ':    easing.in_out_circ
		'in_out_back':    easing.in_out_back
		'in_out_elastic': easing.in_out_elastic
		'in_out_bounce':  easing.in_out_bounce
	}
}
const all_keys = all.keys()

@[heap]
struct App {
mut:
	gg   &gg.Context = unsafe { nil }
	x    f64
	t    f64
	h    f64
	kind string = all_keys.first()
}

fn (mut app App) draw_circle(label string, f easing.EasingFN) {
	offset := 30
	app.gg.draw_text_def(int(app.x) - 30, 5, label)
	app.gg.draw_line(f32(app.x), offset, f32(app.x), f32(app.h + offset), gx.gray)
	app.gg.draw_circle_filled(f32(app.x), f32(offset + f(app.t) * app.h), 10, gx.rgb(0,
		0, 255))
	app.x += 120
}

fn (mut app App) frame() {
	size := gg.window_size()
	period := u64(240)
	app.t = f32_min(1.0, f32(app.gg.frame % period) / f32(period - 30))
	app.x = 80
	app.h = size.height - 100
	app.gg.begin()
	app.gg.draw_line(0, f32(app.h + 30), size.width, f32(app.h + 30), gx.gray)
	current_map := unsafe { all[app.kind].clone() }
	for k, e in current_map {
		app.draw_circle(k, e)
	}
	app.gg.draw_text_def(50, int(app.h + 50), 'Note: use left and right arrows to change functions. Frame: ${app.gg.frame:010} | t: ${app.t:6.3f} | kind: ${app.kind}.')
	app.gg.end()
}

fn (mut app App) change_functions(direction int) {
	idx := (all_keys.len + all_keys.index(app.kind) + direction) % all_keys.len
	app.kind = all_keys[idx]
}

fn (mut app App) on_event(ev &gg.Event, x voidptr) {
	if ev.typ != .key_down {
		return
	}
	if ev.key_code == .left {
		app.change_functions(-1)
	} else if ev.key_code == .right {
		app.change_functions(1)
	}
}

mut app := &App{}
app.gg = gg.new_context(
	bg_color:     gx.rgb(174, 198, 255)
	width:        1350
	height:       800
	window_title: 'Easing functions'
	frame_fn:     app.frame
	event_fn:     app.on_event
	user_data:    app
)
app.gg.run()
