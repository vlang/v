module main

// raylib [shapes] example - dashed line, ported to gg.
import gg
import math

const raywhite = gg.Color{245, 245, 245, 255}
const black_c = gg.Color{0, 0, 0, 255}
const darkgray = gg.Color{80, 80, 80, 255}
const skyblue = gg.Color{102, 191, 255, 255}
const skyblue_t = gg.Color{102, 191, 255, 128}
const blue_c = gg.Color{0, 121, 241, 255}
const line_colors = [
	gg.Color{230, 41, 55, 255},
	gg.Color{255, 161, 0, 255},
	gg.Color{255, 203, 0, 255},
	gg.Color{0, 158, 47, 255},
	gg.Color{0, 121, 241, 255},
	gg.Color{135, 61, 190, 255},
	gg.Color{255, 189, 196, 255},
	gg.Color{0, 0, 0, 255},
]!

struct App {
mut:
	ctx         &gg.Context = unsafe { nil }
	end_x       f32         = 780
	end_y       f32         = 400
	dash_len    f32         = 25
	blank_len   f32         = 15
	color_index int
}

fn draw_dashed(ctx &gg.Context, x1 f32, y1 f32, x2 f32, y2 f32, dash f32, blank f32, c gg.Color) {
	dx, dy := x2 - x1, y2 - y1
	length := math.sqrtf(dx * dx + dy * dy)
	if length == 0 {
		return
	}
	ux, uy := dx / length, dy / length
	mut pos := f32(0)
	for pos < length {
		end := math.min(pos + dash, length)
		ctx.draw_line(x1 + ux * pos, y1 + uy * pos, x1 + ux * end, y1 + uy * end, c)
		pos += dash + blank
	}
}

fn on_frame(data voidptr) {
	mut app := unsafe { &App(data) }
	ctx := app.ctx
	ctx.begin()
	draw_dashed(ctx, 20, 50, app.end_x, app.end_y, app.dash_len, app.blank_len,
		line_colors[app.color_index])

	ctx.draw_rect_filled(5, 5, 265, 95, skyblue_t)
	ctx.draw_rect_empty(5, 5, 265, 95, blue_c)
	ctx.draw_text(15, 15, 'CONTROLS:', size: 10, color: black_c)
	ctx.draw_text(15, 35, 'UP/DOWN: Change Dash Length', size: 10, color: black_c)
	ctx.draw_text(15, 55, 'LEFT/RIGHT: Change Space Length', size: 10, color: black_c)
	ctx.draw_text(15, 75, 'C: Cycle Color', size: 10, color: black_c)
	ctx.draw_text(15, 115, 'Dash: ${int(app.dash_len)} | Space: ${int(app.blank_len)}',
		size:  10
		color: darkgray
	)
	ctx.end()
}

fn on_event(e &gg.Event, data voidptr) {
	mut app := unsafe { &App(data) }
	if e.typ == .key_down && e.key_code == .c {
		app.color_index = (app.color_index + 1) % line_colors.len
	}
}

fn on_update(dt f32, data voidptr) {
	mut app := unsafe { &App(data) }
	_ = dt
	app.end_x = f32(app.ctx.mouse_pos_x)
	app.end_y = f32(app.ctx.mouse_pos_y)
	k := app.ctx.pressed_keys
	if k[gg.KeyCode.up] {
		app.dash_len++
	}
	if k[gg.KeyCode.down] && app.dash_len > 1 {
		app.dash_len--
	}
	if k[gg.KeyCode.right] {
		app.blank_len++
	}
	if k[gg.KeyCode.left] && app.blank_len > 1 {
		app.blank_len--
	}
}

fn main() {
	mut app := &App{}
	app.ctx = gg.new_context(
		width:        800
		height:       450
		window_title: 'raylib [shapes] example - dashed line'
		user_data:    app
		frame_fn:     on_frame
		event_fn:     on_event
		update_fn:    on_update
		bg_color:     raywhite
	)
	app.ctx.run()
}
