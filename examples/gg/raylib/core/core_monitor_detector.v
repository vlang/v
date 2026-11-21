module main

// raylib [core] example - monitor detector
// gg exposes monitor_count/monitor_info; sokol reports the active display
// as the primary monitor, so this draws the available display information.

import gg

const screen_width = 800
const screen_height = 450

const ray_white = gg.Color{245, 245, 245, 255}
const dark_gray = gg.Color{80, 80, 80, 255}
const blue = gg.Color{0, 0, 255, 255}
const red = gg.Color{255, 0, 0, 255}
const green = gg.Color{0, 158, 47, 128}

struct App {
mut:
	gg &gg.Context = unsafe { nil }
}

fn main() {
	mut app := &App{}
	app.gg = gg.new_context(
		width:        screen_width
		height:       screen_height
		window_title: 'raylib [core] example - monitor detector'
		bg_color:     ray_white
		frame_fn:     frame
		user_data:    app
	)
	app.gg.run()
}

fn frame(data voidptr) {
	app := unsafe { &App(data) }
	ctx := app.gg
	ctx.begin()
	ctx.draw_text(20, 20, 'Monitor information (gg / sokol)', size: 20, color: dark_gray)
	ctx.draw_rect_empty(20, 60, screen_width - 40, screen_height - 100, dark_gray)
	count := gg.monitor_count()
	for i in 0 .. count {
		info := gg.monitor_info(i)
		y := 80 + i * 70
		ctx.draw_text(40, y, '[${i}] ${info.name}', size: 20, color: blue)
		ctx.draw_text(40, y + 26, 'Resolution: ${info.width}x${info.height}',
			size:  16
			color: dark_gray
		)
		ctx.draw_rect_empty(40, y + 50, 120, 14, if i == 0 { red } else { dark_gray })
		if i == 0 {
			ctx.draw_rect(x: 40, y: f32(y + 50), w: 120, h: 14, color: green)
		}
	}
	ctx.draw_text(20, screen_height - 30,
		'PORT NOTE: sokol reports one active display; full enumeration requires the multiwindow build.',
		size:  10
		color: dark_gray
	)
	ctx.end()
}
