module main

// raylib [text] example - codepoints loading, ported to gg.
// PORT NOTE: gg builds its glyph atlas automatically from the system/TTF font and
// cannot restrict generation to an explicit codepoint list. Duplicate removal is
// still demonstrated; glyphs are provided by the configured Japanese TTF.
import gg

const (
	text = 'いろはにほへと　ちりぬるを\nわかよたれそ　つねならむ\nうゐのおくやま　けふこえて\nあさきゆめみし　ゑひもせす'
	raywhite = gg.Color{245, 245, 245, 255}
	black_c  = gg.Color{0, 0, 0, 255}
	green_c  = gg.Color{0, 158, 47, 255}
	gray_c   = gg.Color{130, 130, 130, 255}
)

struct App {
mut:
	ctx        &gg.Context = unsafe { nil }
	show_atlas bool
	count      int
	unique     int
}

fn on_frame(data voidptr) {
	mut app := unsafe { &App(data) }
	ctx := app.ctx
	ctx.begin()
	ctx.draw_rect_filled(0, 0, f32(ctx.width), 70, black_c)
	ctx.draw_text(10, 10, 'Total codepoints contained in provided text: ${app.count}',
		size:  20
		color: green_c
	)
	ctx.draw_text(10, 40, 'Total codepoints required for font atlas (duplicates excluded): ${app.unique}',
		size:  20
		color: green_c
	)
	if app.show_atlas {
		ctx.draw_text(160, 100, 'FONT ATLAS VIEW (gg-generated)',
			size:  24
			color: black_c
		)
	} else {
		ctx.draw_text(160, 110, text, size: 48, color: black_c)
	}
	ctx.draw_text(10, ctx.height - 30, 'Press SPACE to toggle font atlas view!',
		size:  20
		color: gray_c
	)
	ctx.end()
}

fn on_event(e &gg.Event, data voidptr) {
	mut app := unsafe { &App(data) }
	if e.typ == .key_down && e.key_code == .space {
		app.show_atlas = !app.show_atlas
	}
}

fn main() {
	mut seen := map[rune]bool{}
	mut total := 0
	for ch in text {
		seen[ch] = true
		total++
	}
	mut app := &App{ count: total, unique: seen.len }
	app.ctx = gg.new_context(
		width:        800
		height:       450
		window_title: 'raylib [text] example - codepoints loading'
		user_data:    app
		frame_fn:     on_frame
		event_fn:     on_event
		bg_color:     raywhite
		font_path:    'resources/DotGothic16-Regular.ttf'
	)
	app.ctx.run()
}
