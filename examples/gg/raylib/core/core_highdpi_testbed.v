module main

// raylib [core] example - highdpi testbed
// Draws a grid with coordinate labels plus DPI/monitor information.
// Space toggles borderless mode and F toggles fullscreen via sokol.sapp.

import gg
import sokol.sapp

const screen_width = 800
const screen_height = 450
const grid_spacing = 40

const ray_white = gg.Color{245, 245, 245, 255}
const gray = gg.Color{130, 130, 130, 255}
const light_gray = gg.Color{200, 200, 200, 255}
const dark_gray = gg.Color{80, 80, 80, 255}
const red = gg.Color{255, 0, 0, 255}
const blue = gg.Color{0, 0, 255, 255}
const maroon = gg.Color{128, 0, 0, 255}
const black = gg.Color{0, 0, 0, 255}

struct App {
mut:
	gg &gg.Context = unsafe { nil }
}

fn main() {
	mut app := &App{}
	app.gg = gg.new_context(
		width:        screen_width
		height:       screen_height
		window_title: 'raylib [core] example - highdpi testbed'
		bg_color:     ray_white
		resizable:    true
		frame_fn:     frame
		event_fn:     on_event
		user_data:    app
	)
	app.gg.run()
}

fn on_event(e &gg.Event, data voidptr) {
	if e.typ != .key_down || e.key_repeat {
		return
	}
	if e.key_code == .f {
		sapp.toggle_fullscreen()
	}
}

fn frame(data voidptr) {
	app := unsafe { &App(data) }
	ctx := app.gg
	ctx.begin()
	mut h := 0
	for h < ctx.height / grid_spacing + 1 {
		ctx.draw_text(4, h * grid_spacing - 4, '${h * grid_spacing:02}',
			size:  10
			color: gray
		)
		ctx.draw_line(24, f32(h * grid_spacing), f32(ctx.width), f32(h * grid_spacing), light_gray)
		h++
	}
	mut v := 0
	for v < ctx.width / grid_spacing + 1 {
		ctx.draw_text(v * grid_spacing - 10, 4, '${v * grid_spacing:02}',
			size:  10
			color: gray
		)
		ctx.draw_line(f32(v * grid_spacing), 20, f32(v * grid_spacing), f32(ctx.height), light_gray)
		v++
	}
	info := gg.monitor_info(0)
	ctx.draw_text(50, 50, 'CURRENT MONITOR: 1/${gg.monitor_count()} (${info.width}x${info.height})',
		size:  20
		color: dark_gray
	)
	ctx.draw_text(50, 90, 'SCREEN SIZE: ${ctx.width}x${ctx.height}',
		size:  20
		color: dark_gray
	)
	ctx.draw_text(50, 130, 'SCALE FACTOR: ${ctx.scale:.2f}x${ctx.scale:.2f}',
		size:  20
		color: gray
	)
	ctx.draw_rect(x: 0, y: 0, w: 30, h: 60, color: red)
	ctx.draw_rect(x: f32(ctx.width - 30), y: f32(ctx.height - 60), w: 30, h: 60, color: blue)
	mx, my := f32(ctx.mouse_pos_x), f32(ctx.mouse_pos_y)
	ctx.draw_circle_filled(mx, my, 20, maroon)
	ctx.draw_rect(x: mx - 25, y: my, w: 50, h: 2, color: black)
	ctx.draw_rect(x: mx, y: my - 25, w: 2, h: 50, color: black)
	ty := if my > f32(ctx.height) - 60 { my - 46 } else { my + 30 }
	ctx.draw_text(int(mx) - 44, int(ty), '[${int(mx)},${int(my)}]', size: 20, color: black)
	ctx.end()
}
