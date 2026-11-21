module main

// raylib [shapes] example - vector angle, ported to gg.
import gg
import math

const raywhite = gg.Color{245, 245, 245, 255}
const black_c = gg.Color{0, 0, 0, 255}
const darkgray = gg.Color{80, 80, 80, 255}
const red_c = gg.Color{230, 41, 55, 255}
const lightgray = gg.Color{200, 200, 200, 255}
const green_t = gg.Color{0, 158, 47, 153}
const lime = gg.Color{1, 222, 38, 255}

fn vec_angle(ax f32, ay f32, bx f32, by f32) f32 {
	dot := ax * bx + ay * by
	la := math.sqrtf(ax * ax + ay * ay)
	lb := math.sqrtf(bx * bx + by * by)
	if la == 0 || lb == 0 { return 0 }
	return f32(math.degrees(math.acos(math.max(f32(-1), math.min(f32(1), dot / (la * lb))))))
}

fn on_frame(data voidptr) {
	mut app := unsafe { &App(data) }
	ctx := app.ctx
	ctx.begin()
	v0x, v0y := f32(400), f32(225)
	v2x, v2y := f32(ctx.mouse_pos_x), f32(ctx.mouse_pos_y)
	if app.mode == 0 {
		ctx.draw_text(10, 10, 'MODE 0: Angle between V1 and V2', size: 20, color: black_c)
		ctx.draw_text(10, 30, 'Right Click to Move V2', size: 20, color: darkgray)
		ctx.draw_line(v0x, v0y, app.v1x, app.v1y, black_c)
		ctx.draw_line(v0x, v0y, v2x, v2y, red_c)
		start := -f32(math.degrees(math.atan2(app.v1y - v0y, app.v1x - v0x)))
		ctx.draw_slice_filled(v0x, v0y, 40, f32(math.radians(90 - start)),
			f32(math.radians(90 - start - app.angle)), 32, green_t)
		ctx.draw_text(int(app.v1x), int(app.v1y) - 10, 'v1', size: 10, color: darkgray)
	} else {
		ctx.draw_text(10, 10, 'MODE 1: Angle formed by line V1 to V2', size: 20, color: black_c)
		ctx.draw_line(0, v0y, f32(ctx.width), v0y, lightgray)
		ctx.draw_line(v0x, v0y, v2x, v2y, red_c)
		ctx.draw_slice_filled(v0x, v0y, 40, f32(math.radians(90)),
			f32(math.radians(90 + app.angle)), 32, green_t)
		ctx.draw_text(int(v0x) + 40, int(v0y), 'v1', size: 10, color: darkgray)
	}
	ctx.draw_text(int(v0x), int(v0y), 'v0', size: 10, color: darkgray)
	ctx.draw_text(int(v2x) - 10, int(v2y) - 10, 'v2', size: 10, color: darkgray)
	ctx.draw_text(460, 10, 'Press SPACE to change MODE', size: 20, color: darkgray)
	ctx.draw_text(10, 70, 'ANGLE: ${app.angle:.2f}', size: 20, color: lime)
	ctx.end()
}

struct App {
mut:
	ctx   &gg.Context = unsafe { nil }
	v1x   f32         = 500
	v1y   f32         = 305
	angle f32
	mode  int
}

fn on_event(e &gg.Event, data voidptr) {
	mut app := unsafe { &App(data) }
	if e.typ == .key_down && e.key_code == .space {
		app.mode = if app.mode == 0 { 1 } else { 0 }
	}
}

fn on_update(dt f32, data voidptr) {
	mut app := unsafe { &App(data) }
	_ = dt
	v0x, v0y := f32(400), f32(225)
	v2x, v2y := f32(app.ctx.mouse_pos_x), f32(app.ctx.mouse_pos_y)
	if app.mode == 0 {
		if app.ctx.mouse_buttons.has(.right) {
			app.v1x, app.v1y = v2x, v2y
		}
		app.angle = vec_angle(app.v1x - v0x, app.v1y - v0y, v2x - v0x, v2y - v0y)
	} else {
		app.angle = f32(math.degrees(math.atan2(v2y - v0y, v2x - v0x)))
	}
}

fn main() {
	mut app := &App{}
	app.ctx = gg.new_context(
		width:        800
		height:       450
		window_title: 'raylib [shapes] example - vector angle'
		user_data:    app
		frame_fn:     on_frame
		event_fn:     on_event
		update_fn:    on_update
		bg_color:     raywhite
	)
	app.ctx.run()
}
