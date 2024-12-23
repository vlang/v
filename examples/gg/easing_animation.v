module main

import gg
import gx
import math.easing

struct Easing {
	f     easing.EasingFN = easing.linear
	label string
	color gx.Color
}

const all = {
	'in_functions':     {
		'linear':     Easing{
			f:     easing.linear
			color: gx.rgb(250, 250, 250)
		}
		'in_sine':    Easing{
			f:     easing.in_sine
			color: gx.rgb(20, 200, 50)
		}
		'in_quad':    Easing{
			f:     easing.in_quad
			color: gx.rgb(200, 0, 250)
		}
		'in_cubic':   Easing{
			f:     easing.in_cubic
			color: gx.rgb(200, 250, 50)
		}
		'in_quart':   Easing{
			f:     easing.in_quart
			color: gx.rgb(250, 20, 20)
		}
		'in_quint':   Easing{
			f:     easing.in_quint
			color: gx.rgb(100, 50, 100)
		}
		'in_expo':    Easing{
			f:     easing.in_expo
			color: gx.rgba(50, 55, 255, 100)
		}
		'in_circ':    Easing{
			f:     easing.in_circ
			color: gx.rgb(155, 255, 200)
		}
		'in_back':    Easing{
			f:     easing.in_back
			color: gx.rgb(0, 50, 200)
		}
		'in_elastic': Easing{
			f:     easing.in_elastic
			color: gx.rgb(255, 50, 255)
		}
		'in_bounce':  Easing{
			f:     easing.in_bounce
			color: gx.rgb(50, 0, 150)
		}
	}
	'out_functions':    {
		'linear':      Easing{
			f:     easing.linear
			color: gx.rgb(250, 250, 250)
		}
		'out_sine':    Easing{
			f:     easing.out_sine
			color: gx.rgb(100, 0, 250)
		}
		'out_quad':    Easing{
			f:     easing.out_quad
			color: gx.rgb(100, 0, 250)
		}
		'out_cubic':   Easing{
			f:     easing.out_cubic
			color: gx.rgb(100, 100, 250)
		}
		'out_quart':   Easing{
			f:     easing.out_quart
			color: gx.rgb(100, 100, 100)
		}
		'out_quint':   Easing{
			f:     easing.out_quint
			color: gx.rgb(100, 50, 100)
		}
		'out_expo':    Easing{
			f:     easing.out_expo
			color: gx.rgb(50, 50, 100)
		}
		'out_circ':    Easing{
			f:     easing.out_circ
			color: gx.rgb(50, 50, 50)
		}
		'out_back':    Easing{
			f:     easing.out_back
			color: gx.rgb(0, 50, 200)
		}
		'out_elastic': Easing{
			f:     easing.out_elastic
			color: gx.rgb(50, 50, 50)
		}
		'out_bounce':  Easing{
			f:     easing.out_bounce
			color: gx.rgb(50, 0, 150)
		}
	}
	'in_out_functions': {
		'linear':         Easing{
			f:     easing.linear
			color: gx.rgb(250, 250, 250)
		}
		'in_out_sine':    Easing{
			f:     easing.in_out_sine
			color: gx.rgb(100, 0, 250)
		}
		'in_out_quad':    Easing{
			f:     easing.in_out_quad
			color: gx.rgb(100, 0, 250)
		}
		'in_out_cubic':   Easing{
			f:     easing.in_out_cubic
			color: gx.rgb(100, 100, 250)
		}
		'in_out_quart':   Easing{
			f:     easing.in_out_quart
			color: gx.rgb(100, 100, 100)
		}
		'in_out_quint':   Easing{
			f:     easing.in_out_quint
			color: gx.rgb(100, 50, 100)
		}
		'in_out_expo':    Easing{
			f:     easing.in_out_expo
			color: gx.rgb(50, 50, 100)
		}
		'in_out_circ':    Easing{
			f:     easing.in_out_circ
			color: gx.rgb(50, 50, 50)
		}
		'in_out_back':    Easing{
			f:     easing.in_out_back
			color: gx.rgb(0, 50, 200)
		}
		'in_out_elastic': Easing{
			f:     easing.in_out_elastic
			color: gx.rgb(50, 50, 50)
		}
		'in_out_bounce':  Easing{
			f:     easing.in_out_bounce
			color: gx.rgb(50, 0, 150)
		}
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

fn (mut app App) draw_circle(label string, e Easing) {
	offset := 30
	app.gg.draw_text_def(int(app.x) - 30, 5, label)
	app.gg.draw_line(f32(app.x), offset, f32(app.x), f32(app.h + offset), gx.gray)
	app.gg.draw_circle_filled(f32(app.x), f32(offset + e.f(app.t) * app.h), 10, e.color)
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
	for k, e in all[app.kind] {
		app.draw_circle(k, e)
	}
	app.gg.draw_text_def(50, int(app.h + 50), 'Note: use left and right arrows to change functions. Frame: ${app.gg.frame:010} | t: ${app.t:6.3f} | kind: ${app.kind}.')
	app.gg.end()
	if app.gg.pressed_keys[int(gg.KeyCode.down)] {
	}
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
