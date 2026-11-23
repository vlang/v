module main

// raylib [core] example - delta time
// Compares frame-rate-independent motion (using gg's dt) with a fixed
// per-frame step. Scroll to change the displayed FPS target.
// PORT NOTE: the actual swap interval cannot be changed from gg at runtime.

import gg

const screen_width = 800
const screen_height = 450

const ray_white = gg.Color{245, 245, 245, 255}
const dark_gray = gg.Color{80, 80, 80, 255}
const red = gg.Color{255, 0, 0, 255}
const blue = gg.Color{0, 0, 255, 255}

struct App {
mut:
	gg         &gg.Context = unsafe { nil }
	delta_x    f32
	frame_x    f32
	target_fps int = 60
}

fn main() {
	mut app := &App{}
	app.gg = gg.new_context(
		width:        screen_width
		height:       screen_height
		window_title: 'raylib [core] example - delta time'
		bg_color:     ray_white
		frame_fn:     frame
		update_fn:    update
		event_fn:     on_event
		user_data:    app
	)
	app.gg.run()
}

fn on_event(e &gg.Event, data voidptr) {
	mut app := unsafe { &App(data) }
	if e.typ == .key_down && e.key_code == .r && !e.key_repeat {
		app.delta_x = 0
		app.frame_x = 0
	}
}

fn update(dt f32, data voidptr) {
	mut app := unsafe { &App(data) }
	ctx := app.gg
	if ctx.scroll_y != 0 {
		app.target_fps += int(ctx.scroll_y)
		if app.target_fps < 0 {
			app.target_fps = 0
		}
	}
	app.delta_x += dt * 6 * 10
	app.frame_x += 0.1 * 10
	if app.delta_x > screen_width {
		app.delta_x = 0
	}
	if app.frame_x > screen_width {
		app.frame_x = 0
	}
}

fn frame(data voidptr) {
	app := unsafe { &App(data) }
	ctx := app.gg
	ctx.begin()
	ctx.draw_circle_filled(app.delta_x, f32(screen_height) / 3, 32, red)
	ctx.draw_circle_filled(app.frame_x, f32(screen_height) * 2 / 3, 32, blue)
	ctx.draw_text(10, 10, 'FPS: ${app.target_fps} (target: ${app.target_fps})',
		size:  20
		color: dark_gray
	)
	fps := if app.target_fps > 0 { f32(app.target_fps) } else { f32(60) }
	ctx.draw_text(10, 30, 'Frame time: ${f32(1000) / fps:.2f} ms',
		size:  20
		color: dark_gray
	)
	ctx.draw_text(10, 50, 'Use the scroll wheel to change the fps limit, r to reset',
		size:  20
		color: dark_gray
	)
	ctx.draw_text(10, 90, 'FUNC: x += GetFrameTime()*speed', size: 20, color: red)
	ctx.draw_text(10, 240, 'FUNC: x += speed', size: 20, color: blue)
	ctx.end()
}
