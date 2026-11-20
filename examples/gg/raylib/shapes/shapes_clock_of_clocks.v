module main

// raylib [shapes] example - clock of clocks, ported to gg.
import gg
import math
import time

const raywhite = gg.Color{245, 245, 245, 255}
const black_c = gg.Color{0, 0, 0, 255}
const darkgray = gg.Color{80, 80, 80, 255}
const darkblue = gg.Color{0, 82, 172, 255}
const bg_color = gg.Color{0, 20, 43, 255}
const hands_col = gg.Color{207, 255, 254, 255}

// Each digit is 24 cells (6 rows x 4 cols). Tokens give the (big hand, small hand)
// direction: T=top-left, H=horizontal, R=top-right, V=vertical,
// L=bottom-left, B=bottom-right, Z=hidden.
const digit_angles = [
	'TL HH HH TR VV TL TR VV VV VV VV VV VV VV VV VV VV BL BR VV BL HH HH BR',
	'TL HH TR ZZ BL TR VV ZZ ZZ VV VV ZZ ZZ VV VV ZZ TL BR BL TR BL HH HH BR',
	'TL HH HH TR BL HH TR VV TL HH BR VV VV TL HH BR VV BL HH TR BL HH HH BR',
	'TL HH HH TR BL HH TR VV TL HH BR VV BL HH TR VV TL HH BR VV BL HH HH BR',
	'TL TR TL TR VV VV VV VV VV BL BR VV BL HH TR VV ZZ ZZ VV VV ZZ ZZ BL BR',
	'TL HH HH TR VV TL HH BR VV BL HH TR BL HH TR VV TL HH BR VV BL HH HH BR',
	'TL HH HH TR VV TL HH BR VV BL HH TR VV TL TR VV VV BL BR VV BL HH HH BR',
	'TL HH HH TR BL HH TR VV ZZ ZZ VV VV ZZ ZZ VV VV ZZ ZZ VV VV ZZ ZZ BL BR',
	'TL HH HH TR VV TL TR VV VV BL BR VV VV TL TR VV VV BL BR VV BL HH HH BR',
	'TL HH HH TR VV TL TR VV VV BL BR VV BL HH TR VV TL HH BR VV BL HH HH BR',
]!

fn angle_for(tok string) (f32, f32) {
	return match tok {
		'TL' { f32(0), f32(90) }
		'HH' { f32(0), f32(180) }
		'TR' { f32(90), f32(180) }
		'VV' { f32(90), f32(270) }
		'BL' { f32(0), f32(270) }
		'BR' { f32(180), f32(270) }
		else { f32(135), f32(135) }
	}
}

fn lerp(a f32, b f32, t f32) f32 {
	return a + (b - a) * t
}

struct App {
mut:
	ctx          &gg.Context = unsafe { nil }
	hour_mode    int         = 24
	prev_seconds int         = -1
	current_x    [6][24]f32
	current_y    [6][24]f32
	src_x        [6][24]f32
	src_y        [6][24]f32
	dst_x        [6][24]f32
	dst_y        [6][24]f32
	move_timer   f32 = 1.0
}

fn draw_hand(ctx &gg.Context, cx f32, cy f32, len f32, angle f32, c gg.Color) {
	rad := f32(math.radians(angle))
	dx, dy := math.cosf(rad), math.sinf(rad)
	// PORT NOTE: gg lines are 1px; raylib's thick rotated rect is approximated
	// with a narrow quad around the hand direction.
	px, py := -dy * 1.2, dx * 1.2
	ctx.draw_convex_poly([
		cx + px,
		cy + py,
		cx + dx * len + px,
		cy + dy * len + py,
		cx + dx * len - px,
		cy + dy * len - py,
		cx - px,
		cy - py,
	], c)
}

fn on_frame(data voidptr) {
	mut app := unsafe { &App(data) }
	ctx := app.ctx
	ctx.begin()
	ctx.draw_text(10, 30, '${app.hour_mode}-h mode, space to change',
		size:  20
		color: raywhite
	)

	face_size := f32(24)
	spacing := f32(8)
	mut x_offset := f32(4)
	for digit in 0 .. 6 {
		for row in 0 .. 6 {
			for col in 0 .. 4 {
				cell := row * 4 + col
				cx := x_offset + f32(col) * (face_size + spacing) + face_size * 0.5
				cy := 100 + f32(row) * (face_size + spacing) + face_size * 0.5
				ctx.draw_circle_empty(cx, cy, face_size * 0.5 - 2, darkgray)
				draw_hand(ctx, cx, cy, face_size * 0.5 + 4, app.current_x[digit][cell], hands_col)
				draw_hand(ctx, cx, cy, face_size * 0.5 + 2, app.current_y[digit][cell], hands_col)
			}
		}
		x_offset += (face_size + spacing) * 4
		if digit % 2 == 1 {
			ctx.draw_circle_empty(x_offset + 4, 160, 8, hands_col)
			ctx.draw_circle_empty(x_offset + 4, 225, 8, hands_col)
			x_offset += 16
		}
	}
	ctx.end()
}

fn on_event(e &gg.Event, data voidptr) {
	mut app := unsafe { &App(data) }
	if e.typ == .key_down && e.key_code == .space {
		app.hour_mode = 36 - app.hour_mode
	}
}

fn on_update(dt f32, data voidptr) {
	mut app := unsafe { &App(data) }
	t := time.now()
	if t.second != app.prev_seconds {
		app.prev_seconds = t.second
		hh := if app.hour_mode == 12 { t.hour % 12 } else { t.hour }
		digits := '${hh:02}${t.minute:02}${t.second:02}'
		for digit in 0 .. 6 {
			ch := int(digits[digit] - `0`)
			toks := digit_angles[ch].fields()
			for cell in 0 .. 24 {
				tx, ty := angle_for(toks[cell])
				app.src_x[digit][cell] = app.current_x[digit][cell]
				app.src_y[digit][cell] = app.current_y[digit][cell]
				app.dst_x[digit][cell] = tx
				app.dst_y[digit][cell] = ty
				if app.src_x[digit][cell] > app.dst_x[digit][cell] {
					app.src_x[digit][cell] -= 360
				}
				if app.src_y[digit][cell] > app.dst_y[digit][cell] {
					app.src_y[digit][cell] -= 360
				}
			}
		}
		app.move_timer = -dt
	}
	if app.move_timer < 0.5 {
		app.move_timer = math.min(app.move_timer + dt, 0.5)
		mut tt := app.move_timer / 0.5
		tt = tt * tt * (3 - 2 * tt)
		for digit in 0 .. 6 {
			for cell in 0 .. 24 {
				app.current_x[digit][cell] = lerp(app.src_x[digit][cell], app.dst_x[digit][cell], tt)
				app.current_y[digit][cell] = lerp(app.src_y[digit][cell], app.dst_y[digit][cell], tt)
			}
		}
	}
}

fn main() {
	mut app := &App{}
	app.ctx = gg.new_context(
		width:        800
		height:       450
		window_title: 'raylib [shapes] example - clock of clocks'
		user_data:    app
		frame_fn:     on_frame
		event_fn:     on_event
		update_fn:    on_update
		bg_color:     bg_color
	)
	app.ctx.run()
}
