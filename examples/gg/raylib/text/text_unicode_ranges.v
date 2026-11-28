module main

// raylib [text] example - unicode ranges, ported to gg.
// PORT NOTE: gg cannot add codepoint ranges to an existing font atlas. The
// language strings are rendered with the system/TTF font; the range selector is
// kept as a visual control.
import gg

const (
	raywhite = gg.Color{245, 245, 245, 255}
	maroon   = gg.Color{190, 33, 55, 255}
	darkgray = gg.Color{80, 80, 80, 255}
	blue_c   = gg.Color{0, 121, 241, 255}
	lime     = gg.Color{1, 222, 38, 255}
	gray_c   = gg.Color{130, 130, 130, 255}
	black_c  = gg.Color{0, 0, 0, 255}
	red_c    = gg.Color{230, 41, 55, 255}

	lines = [
		'> English: Hello World!',
		'> Español: Hola mundo!',
		'> Ελληνικά: Γειά σου κόσμε!',
		'> Русский: Привет мир!',
		'> 中文: 你好世界!',
		'> 日本語: こんにちは世界!',
	]!
	range_names = ['ASCII', 'EUROPEAN', 'GREEK', 'CYRILLIC', 'CJK']!
)

struct App {
mut:
	ctx     &gg.Context = unsafe { nil }
	range   int
}

fn on_frame(data voidptr) {
	mut app := unsafe { &App(data) }
	ctx := app.ctx
	ctx.begin()
	ctx.draw_text(20, 20, 'ADD CODEPOINTS: [1][2][3][4]', size: 20, color: maroon)
	for i, line in lines {
		ctx.draw_text(50, 70 + 50 * i, line, size: 32, color: darkgray)
	}
	ctx.draw_rect_filled(400, 16, 380, 380, black_c)
	ctx.draw_text(410, 20, 'FONT ATLAS', size: 20, color: white_color())
	ctx.draw_rect_empty(400, 16, 380, 380, red_c)
	ctx.draw_text(20, 380, 'ATLAS SIZE: 512x512 px', size: 20, color: blue_c)
	ctx.draw_text(20, 410, 'RANGE: ${range_names[app.range]}', size: 20, color: lime)
	ctx.draw_text(ctx.width - 300, ctx.height - 20,
		'Font: Noto Sans TC. License: SIL OFL 1.1', size: 10, color: gray_c)
	ctx.end()
}

fn white_color() gg.Color { return gg.Color{255, 255, 255, 255} }

fn on_event(e &gg.Event, data voidptr) {
	mut app := unsafe { &App(data) }
	if e.typ == .key_down {
		match e.key_code {
			._0 { app.range = 0 }
			._1 { app.range = 1 }
			._2 { app.range = 2 }
			._3 { app.range = 3 }
			._4 { app.range = 4 }
			else {}
		}
	}
}

fn main() {
	mut app := &App{}
	app.ctx = gg.new_context(
		width:        800
		height:       450
		window_title: 'raylib [text] example - unicode ranges'
		user_data:    app
		frame_fn:     on_frame
		event_fn:     on_event
		bg_color:     raywhite
		font_path:    'resources/NotoSansTC-Regular.ttf'
	)
	app.ctx.run()
}
