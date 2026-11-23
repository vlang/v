module main

// raylib [text] example - font filters, ported to gg.
// PORT NOTE: gg has no per-font texture filters or mipmap generation. The N key
// toggles a nearest-vs-linear flag shown in the HUD; the filter number keys are
// a visual selection only and do not change the atlas filter.
import gg

const raywhite = gg.Color{245, 245, 245, 255}
const lightgray = gg.Color{200, 200, 200, 255}
const gray_c = gg.Color{130, 130, 130, 255}
const darkgray = gg.Color{80, 80, 80, 255}
const black_c = gg.Color{0, 0, 0, 255}

struct App {
mut:
	ctx       &gg.Context = unsafe { nil }
	font_size f32         = 96
	tx        f32         = 40
	ty        f32
	filter    int
	nearest   bool
}

fn on_frame(data voidptr) {
	mut app := unsafe { &App(data) }
	ctx := app.ctx
	ctx.begin()
	ctx.draw_text(20, 20, 'Use mouse wheel to change font size', size: 10, color: gray_c)
	ctx.draw_text(20, 40, 'Use KEY_RIGHT and KEY_LEFT to move text', size: 10, color: gray_c)
	ctx.draw_text(20, 60, 'Use [N] to toggle nearest/linear filter', size: 10, color: gray_c)
	ctx.draw_text(20, 80, 'Drop a new TTF font for dynamic loading', size: 10, color: darkgray)
	ctx.draw_text(int(app.tx), int(app.ty), 'Loaded Font',
		size:  int(app.font_size)
		color: black_c
	)
	ctx.draw_rect_filled(0, f32(ctx.height - 80), f32(ctx.width), 80, lightgray)
	ctx.draw_text(20, ctx.height - 50, 'Font size: ${app.font_size:.2f}', size: 10, color: darkgray)
	ctx.draw_text(20, ctx.height - 30, 'Text size: [${ctx.text_width('Loaded Font')}, 96]',
		size:  10
		color: darkgray
	)
	names := ['POINT', 'BILINEAR', 'TRILINEAR']
	ctx.draw_text(250, 400, 'CURRENT TEXTURE FILTER:', size: 20, color: gray_c)
	ctx.draw_text(570, 400, names[app.filter], size: 20, color: black_c)
	ctx.draw_text(20, 110, 'nearest: ${app.nearest}', size: 10, color: gray_c)
	ctx.end()
}

fn on_event(e &gg.Event, data voidptr) {
	mut app := unsafe { &App(data) }
	if e.typ == .mouse_scroll { app.font_size += e.scroll_y * 4 }
	if e.typ == .key_down {
		match e.key_code {
			._1 { app.filter = 0 }
			._2 { app.filter = 1 }
			._3 { app.filter = 2 }
			.n { app.nearest = !app.nearest }
			else {}
		}
	}
	if app.font_size < 6 { app.font_size = 6 }
}

fn on_update(dt f32, data voidptr) {
	mut app := unsafe { &App(data) }
	_ = dt
	if app.ctx.pressed_keys[gg.KeyCode.left] { app.tx -= 10 }
	if app.ctx.pressed_keys[gg.KeyCode.right] { app.tx += 10 }
}

fn main() {
	mut app := &App{}
	app.ctx = gg.new_context(
		width:        800
		height:       450
		window_title: 'raylib [text] example - font filters'
		user_data:    app
		frame_fn:     on_frame
		event_fn:     on_event
		update_fn:    on_update
		bg_color:     raywhite
		font_path:    'resources/KAISG.ttf'
	)
	app.ctx.run()
}
