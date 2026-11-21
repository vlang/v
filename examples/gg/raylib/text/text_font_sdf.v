module main

// raylib [text] example - font sdf, ported to gg.
// PORT NOTE: gg has no signed-distance-field font generation or custom SDF shader.
// The example demonstrates the same scaling interaction with gg's TTF text; the
// SDF-specific atlas view and shader toggle are represented as text labels only.
import gg

const (
	raywhite = gg.Color{245, 245, 245, 255}
	black_c  = gg.Color{0, 0, 0, 255}
	red_c    = gg.Color{230, 41, 55, 255}
	gray_c   = gg.Color{130, 130, 130, 255}
	darkgray = gg.Color{80, 80, 80, 255}
	maroon   = gg.Color{190, 33, 55, 255}
	msg = 'Signed Distance Fields'
)

struct App {
mut:
	ctx      &gg.Context = unsafe { nil }
	font_size f32 = 16
	sdf       bool
}

fn on_frame(data voidptr) {
	mut app := unsafe { &App(data) }
	ctx := app.ctx
	ctx.begin()
	size := int(app.font_size)
	w := ctx.text_width(msg)
	ctx.draw_text(ctx.width / 2 - w / 2, ctx.height / 2 - 50 + 80, msg,
		size:  size
		color: black_c
	)
	if app.sdf {
		ctx.draw_text(320, 20, 'SDF!', size: 80, color: red_c)
	} else {
		ctx.draw_text(315, 40, 'default font', size: 30, color: gray_c)
	}
	ctx.draw_text(ctx.width - 240, 20, 'FONT SIZE: 16.0', size: 20, color: darkgray)
	ctx.draw_text(ctx.width - 240, 50, 'RENDER SIZE: ${app.font_size:.2f}',
		size:  20
		color: darkgray
	)
	ctx.draw_text(ctx.width - 240, 90, 'Use MOUSE WHEEL to SCALE TEXT!',
		size:  10
		color: darkgray
	)
	ctx.draw_text(340, ctx.height - 30, 'HOLD SPACE to USE SDF FONT VERSION!',
		size:  20
		color: maroon
	)
	ctx.end()
}

fn on_event(e &gg.Event, data voidptr) {
	mut app := unsafe { &App(data) }
	if e.typ == .mouse_scroll {
		app.font_size += e.scroll_y * 8
		if app.font_size < 6 { app.font_size = 6 }
	}
	match e.typ {
		.key_down { app.sdf = true }
		.key_up { app.sdf = false }
		else {}
	}
}

fn main() {
	mut app := &App{}
	app.ctx = gg.new_context(
		width:        800
		height:       450
		window_title: 'raylib [text] example - font sdf'
		user_data:    app
		frame_fn:     on_frame
		event_fn:     on_event
		bg_color:     raywhite
		font_path:    'resources/anonymous_pro_bold.ttf'
	)
	app.ctx.run()
}
