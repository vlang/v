// Port of raylib [shaders] example - ascii rendering.
// PORT NOTE: shader ascii.fs requires gg user-shader support (not yet available); a CPU pass
// converts a procedural scene into luminance characters drawn as text on a dark background.
module main

import gg

const win_width = 800
const win_height = 450
const font_size = 12
const ramp = ' .:-=+*#%@'

struct App {
mut:
	gg       &gg.Context = unsafe { nil }
	font     f32         = 9
	circle_x f32         = 40
	speed    f32         = 1
}

fn main() {
	mut app := &App{}
	app.gg = gg.new_context(
		width:         win_width
		height:        win_height
		window_title:  'raylib [shaders] example - ascii rendering'
		bg_color:      gg.black
		user_data:     app
		update_fn:     update
		event_fn:      event
		frame_fn:      frame
		create_window: true
	)
	app.gg.run()
}

fn update(dt f32, data voidptr) {
	mut app := unsafe { &App(data) }
	app.circle_x += app.speed * dt * 60
	if app.circle_x > 200 || app.circle_x < 40 {
		app.speed *= -1
	}
}

fn event(e &gg.Event, data voidptr) {
	if e.typ != .key_down {
		return
	}
	mut app := unsafe { &App(data) }
	if e.key_code == .left && app.font > 9 {
		app.font--
	} else if e.key_code == .right && app.font < 15 {
		app.font++
	}
}

fn frame(data voidptr) {
	app := unsafe { &App(data) }
	ctx := app.gg
	ctx.begin()
	stride := int(app.font) + 2
	mut y := 40
	for y < win_height {
		mut x := 0
		for x < win_width {
			// Procedural luminance: bright sprite-like blob plus a moving circle.
			fx, fy := f32(x), f32(y)
			mut blob := f32(0)
			if x > 480 {
				blob = 1.0 - (fx - 640) * (fx - 640) / f32(90000) -
					(fy - 220) * (fy - 220) / f32(40000)
			}
			circle := 1.0 - ((fx - app.circle_x) * (fx - app.circle_x) +
				(fy - f32(win_height) / 2) * (fy - f32(win_height) / 2)) / f32(9000)
			mut lum := if blob > circle { blob } else { circle }
			if lum < 0 {
				lum = 0
			}
			if lum > 1 {
				lum = 1
			}
			idx := int(lum * f32(ramp.len - 1))
			if idx >= 0 && idx < ramp.len {
				ch := ramp[idx]
				ctx.draw_text(x, y, '${ch:c}', size: int(app.font), color: gg.light_gray)
			}
			x += stride
		}
		y += stride
	}
	ctx.draw_rect_filled(0, 0, f32(win_width), 30, gg.black)
	ctx.draw_text(120, 8, 'Ascii effect - FontSize:${app.font:.0f} - [Left] -1 [Right] +1',
		size:  14
		color: gg.light_gray
	)
	ctx.end()
}
