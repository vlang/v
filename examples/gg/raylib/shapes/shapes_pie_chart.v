module main

// raylib [shapes] example - pie chart, ported to gg.
// PORT NOTE: the raygui editing panel is replaced with keyboard controls;
// pie/donut geometry and hover highlighting are preserved.
import gg
import math

const max_slices = 10
const raywhite = gg.Color{245, 245, 245, 255}
const gray_c = gg.Color{130, 130, 130, 255}
const lightgray = gg.Color{200, 200, 200, 255}
const white_c = gg.Color{255, 255, 255, 255}

fn hsv(h f32) gg.Color {
	s := f32(0.75)
	v := f32(0.9)
	c := v * s
	x := c * (1 - math.absf((h / 60) % 2 - 1))
	m := v - c
	r, g, b := f32(0), f32(0), f32(0)
	if h < 60 {
		r, g = c, x
	} else if h < 120 {
		r, g = x, c
	} else if h < 180 {
		g, b = c, x
	} else if h < 240 {
		g, b = x, c
	} else if h < 300 {
		r, b = x, c
	} else {
		r, b = c, x
	}
	return gg.Color{u8((r + m) * 255), u8((g + m) * 255), u8((b + m) * 255), 255}
}

fn deg_angle(deg f32) f32 {
	return f32(math.radians(90 - deg))
}

struct App {
mut:
	ctx         &gg.Context = unsafe { nil }
	slices      int         = 7
	values      []f32       = [f32(300), 100, 450, 350, 600, 380, 750, 100, 100, 100]
	inner       f32         = 25
	show_values bool        = true
	show_pct    bool
	donut       bool
	hover       int = -1
}

fn on_frame(data voidptr) {
	mut app := unsafe { &App(data) }
	ctx := app.ctx
	ctx.begin()
	cx, cy := f32(265), f32(225)
	r := f32(205)
	mut total := f32(0)
	for i in 0 .. app.slices { total += app.values[i] }
	start := f32(0)
	for i in 0 .. app.slices {
		sweep := if total > 0 { app.values[i] / total * 360 } else { f32(0) }
		mid := start + sweep / 2
		col := hsv(f32(i) / f32(app.slices) * 360)
		rad := r + if i == app.hover { f32(20) } else { f32(0) }
		if app.donut {
			ctx.draw_arc_filled(cx, cy, app.inner, rad - app.inner, deg_angle(start),
				deg_angle(start + sweep), 120, col)
		} else {
			ctx.draw_slice_filled(cx, cy, rad, deg_angle(start), deg_angle(start + sweep), 120, col)
		}
		if app.values[i] > 0 {
			label := if app.show_values && app.show_pct {
				'${app.values[i]:.1f} (${app.values[i] / total * 100:.0f}%)'
			} else if app.show_values {
				'${app.values[i]:.1f}'
			} else if app.show_pct {
				'${app.values[i] / total * 100:.0f}%'
			} else {
				''
			}
			label_r := if app.donut { (app.inner + r) / 2 } else { r * 0.7 }
			mr := f32(math.radians(mid))
			ctx.draw_text(int(cx + math.cosf(mr) * label_r - 20),
				int(cy + math.sinf(mr) * label_r), label,
				size:  20
				color: white_c
			)
		}
		start += sweep
	}

	ctx.draw_rect_filled(525, 5, 270, 440, gg.Color{200, 200, 200, 128})
	ctx.draw_rect_empty(525, 5, 270, 440, gray_c)
	ctx.draw_text(545, 15, 'Slices: ${app.slices} [+/-]', size: 16, color: black)
	ctx.draw_text(545, 40, 'Values: ${app.show_values} [V]', size: 16, color: black)
	ctx.draw_text(545, 65, 'Percent: ${app.show_pct} [P]', size: 16, color: black)
	ctx.draw_text(545, 90, 'Donut: ${app.donut} [D]', size: 16, color: black)
	ctx.draw_text(545, 115, 'Inner radius: ${int(app.inner)} [[/]]', size: 16, color: black)
	ctx.end()
}

fn on_event(e &gg.Event, data voidptr) {
	mut app := unsafe { &App(data) }
	if e.typ == .key_down {
		match e.key_code {
			.equal, .kp_add {
				if app.slices < max_slices { app.slices++ }
			}
			.minus, .kp_subtract {
				if app.slices > 1 { app.slices-- }
			}
			.v { app.show_values = !app.show_values }
			.p { app.show_pct = !app.show_pct }
			.d { app.donut = !app.donut }
			.left_bracket {
				if app.inner > 5 { app.inner -= 5 }
			}
			.right_bracket {
				if app.inner < 195 { app.inner += 5 }
			}
			else {}
		}
	}
}

fn on_update(dt f32, data voidptr) {
	mut app := unsafe { &App(data) }
	_ = dt
	mx, my := f32(app.ctx.mouse_pos_x), f32(app.ctx.mouse_pos_y)
	app.hover = -1
	dx, dy := mx - 265, my - 225
	dist := math.sqrtf(dx * dx + dy * dy)
	if dist <= 225 && (!app.donut || dist >= app.inner) {
		angle := f32(math.degrees(math.atan2(dy, dx)))
		if angle < 0 { angle += 360 }
		mut cur := f32(0)
		mut total := f32(0)
		for i in 0 .. app.slices { total += app.values[i] }
		for i in 0 .. app.slices {
			sweep := if total > 0 { app.values[i] / total * 360 } else { f32(0) }
			if angle >= cur && angle < cur + sweep {
				app.hover = i
				break
			}
			cur += sweep
		}
	}
}

fn main() {
	mut app := &App{}
	app.ctx = gg.new_context(
		width:        800
		height:       450
		window_title: 'raylib [shapes] example - pie chart'
		user_data:    app
		frame_fn:     on_frame
		event_fn:     on_event
		update_fn:    on_update
		bg_color:     raywhite
	)
	app.ctx.run()
}
