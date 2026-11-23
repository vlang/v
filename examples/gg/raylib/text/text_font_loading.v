module main

// raylib [text] example - font loading, ported to gg.
// PORT NOTE: gg loads TTF fonts via new_context(font_path:) and BMFonts via
// gg.load_bmfont(...). Asset files from raylib's resources/ are expected next to
// this example; if missing, gg falls back to the system font.
import gg

const (
	raywhite  = gg.Color{245, 245, 245, 255}
	lightgray = gg.Color{200, 200, 200, 255}
	gray_c    = gg.Color{130, 130, 130, 255}
	maroon    = gg.Color{190, 33, 55, 255}
	lime      = gg.Color{1, 222, 38, 255}
	msg = "!\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHI\nJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklm\nnopqrstuvwxyz{|}~¿ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒ\nÓÔÕÖ×ØÙÚÛÜÝÞßàáâãäåæçèéêëìíîïðñòóôõö\n÷øùúûüýþÿ"
)

struct App {
mut:
	ctx    &gg.Context = unsafe { nil }
	bmfont gg.SpriteFont
	atlas  gg.Image
	has_bm bool
}

fn on_init(data voidptr) {
	mut app := unsafe { &App(data) }
	if !app.has_bm { return }
	app.atlas = app.ctx.create_image(app.bmfont.texture_path, gg.ImageConfig{}) or { return }
	app.bmfont.image = &app.atlas
}

fn on_frame(data voidptr) {
	mut app := unsafe { &App(data) }
	ctx := app.ctx
	ctx.begin()
	use_ttf := ctx.pressed_keys[gg.KeyCode.space]
	ctx.draw_text(20, 20, 'Hold SPACE to use TTF generated font',
		size:  20
		color: lightgray
	)
	if !use_ttf && app.has_bm && app.bmfont.image != unsafe { nil } {
		gg.draw_sprite_text(ctx, &app.bmfont, msg, 20, 100, 1, maroon)
		ctx.draw_text(20, ctx.height - 30, 'Using BMFont (Angelcode) imported',
			size:  20
			color: gray_c
		)
	} else {
		ctx.draw_text(20, 100, msg, size: 20, color: lime)
		ctx.draw_text(20, ctx.height - 30, 'Using TTF font generated',
			size:  20
			color: gray_c
		)
	}
	ctx.end()
}

fn main() {
	mut app := &App{}
	app.bmfont = gg.load_bmfont('resources/pixantiqua.fnt') or { gg.SpriteFont{} }
	app.has_bm = app.bmfont.glyphs.len > 0
	app.ctx = gg.new_context(
		width:        800
		height:       450
		window_title: 'raylib [text] example - font loading'
		user_data:    app
		init_fn:      on_init
		frame_fn:     on_frame
		bg_color:     raywhite
		font_path:    'resources/pixantiqua.ttf'
	)
	app.ctx.run()
}
