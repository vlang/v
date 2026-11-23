// Port of raylib [shaders] example - texture outline.
// PORT NOTE: shader outline.fs with outlineSize/outlineColor requires gg user-shader support
// (not yet available); the outline is drawn as a colored frame behind a procedural sprite.
module main

import gg

const win_width = 800
const win_height = 450

struct App {
mut:
	gg           &gg.Context = unsafe { nil }
	outline_size f32         = 2
}

fn main() {
	mut app := &App{}
	app.gg = gg.new_context(
		width:         win_width
		height:        win_height
		window_title:  'raylib [shaders] example - texture outline'
		bg_color:      gg.white
		user_data:     app
		event_fn:      event
		frame_fn:      frame
		create_window: true
	)
	app.gg.run()
}

fn event(e &gg.Event, data voidptr) {
	mut app := unsafe { &App(data) }
	if e.typ == .mouse_scroll {
		app.outline_size += e.scroll_y
		if app.outline_size < 1 {
			app.outline_size = 1
		}
	}
}

fn frame(data voidptr) {
	app := unsafe { &App(data) }
	ctx := app.gg
	ctx.begin()
	x := f32(win_width) / 2 - 90
	y := f32(-30)
	// Outline approximated as concentric colored frames.
	for i in 0 .. int(app.outline_size) {
		ctx.draw_rect_empty(x - i, y - i, 180 + f32(i) * 2, 360 + f32(i) * 2, gg.red)
	}
	ctx.draw_rect_filled(x, y, 180, 360, gg.Color{60, 70, 90, 255})
	ctx.draw_circle_filled(x + 90, y + 120, 50, gg.rgb(220, 190, 160))
	ctx.draw_rect_filled(x + 50, y + 180, 80, 180, gg.rgb(40, 50, 70))
	ctx.draw_text(10, 10, 'Shader-based\ntexture\noutline', size: 18, color: gg.gray)
	ctx.draw_text(10, 72, 'Scroll mouse wheel to change outline size', size: 18, color: gg.gray)
	ctx.draw_text(10, 120, 'Outline size: ${app.outline_size:.0f} px', size: 18, color: gg.dark_red)
	ctx.end()
}
