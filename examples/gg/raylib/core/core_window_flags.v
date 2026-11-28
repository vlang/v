module main

// raylib [core] example - window flags
// PORT NOTE: gg/sokol exposes only a subset of raylib's runtime window
// flags (fullscreen, resizable at creation, cursor visibility, always-on-top
// at creation). The remaining flags are shown as unavailable in the HUD.

import gg
import sokol.sapp

const screen_width = 800
const screen_height = 450

const ray_white = gg.Color{245, 245, 245, 255}
const maroon = gg.Color{128, 0, 0, 255}
const dark_blue = gg.Color{0, 82, 172, 255}
const light_gray = gg.Color{200, 200, 200, 255}
const lime = gg.Color{124, 252, 0, 255}
const green = gg.Color{0, 158, 47, 255}
const gray = gg.Color{130, 130, 130, 255}
const white = gg.Color{255, 255, 255, 255}

struct App {
mut:
	gg         &gg.Context = unsafe { nil }
	ball       gg.Vec2     = gg.Vec2{400, 225}
	ball_speed gg.Vec2     = gg.Vec2{5, 4}
	cursor_on  bool        = true
}

fn main() {
	mut app := &App{}
	app.gg = gg.new_context(
		width:        screen_width
		height:       screen_height
		window_title: 'raylib [core] example - window flags'
		bg_color:     ray_white
		resizable:    true
		frame_fn:     frame
		update_fn:    update
		event_fn:     on_event
		user_data:    app
	)
	app.gg.fps.show = true
	app.gg.fps.x = 10
	app.gg.fps.y = 10
	app.gg.run()
}

fn on_event(e &gg.Event, data voidptr) {
	mut app := unsafe { &App(data) }
	if e.typ != .key_down || e.key_repeat {
		return
	}
	match e.key_code {
		.f { sapp.toggle_fullscreen() }
		.h {
			app.cursor_on = !app.cursor_on
			sapp.show_mouse(app.cursor_on)
		}
		.b { sapp.toggle_fullscreen() }
		else {}
	}
}

fn update(dt f32, data voidptr) {
	mut app := unsafe { &App(data) }
	app.ball = app.ball + app.ball_speed.scale(dt * 60)
	if app.ball.x >= screen_width - 20 || app.ball.x <= 20 {
		app.ball_speed.x *= -1
	}
	if app.ball.y >= screen_height - 20 || app.ball.y <= 20 {
		app.ball_speed.y *= -1
	}
}

fn flag_text(ctx &gg.Context, x int, y int, label string, on bool) {
	ctx.draw_text(x, y, '${label}: ${if on { 'on' } else { 'off' }}',
		size:  10
		color: if on { lime } else { maroon }
	)
}

fn frame(data voidptr) {
	app := unsafe { &App(data) }
	ctx := app.gg
	ctx.begin()
	ctx.draw_circle_filled(app.ball.x, app.ball.y, 20, maroon)
	ctx.draw_rect_empty(0, 0, f32(ctx.width), f32(ctx.height), white)
	ctx.draw_circle_filled(f32(ctx.mouse_pos_x), f32(ctx.mouse_pos_y), 10, dark_blue)
	ctx.draw_text(10, 40, 'Screen Size: [${ctx.width}, ${ctx.height}]', size: 10, color: green)
	ctx.draw_text(10, 60, 'Runtime window flags (gg subset):', size: 10, color: gray)
	flag_text(ctx, 10, 80, '[F] FULLSCREEN', sapp.is_fullscreen())
	flag_text(ctx, 10, 100, 'RESIZABLE (set at creation)', true)
	flag_text(ctx, 10, 120, '[H] CURSOR VISIBLE', app.cursor_on)
	ctx.draw_text(10, 150, 'Other raylib flags are not exposed by gg/sapp.',
		size:  10
		color: light_gray
	)
	ctx.end()
}
