module main

// raylib [shapes] example - outlines thickness, ported to gg.
// PORT NOTE: gg outlines are 1px; thickness is approximated by nested outlines.
import gg
import math

const raywhite = gg.Color{245, 245, 245, 255}
const lightgray = gg.Color{200, 200, 200, 255}
const black_c = gg.Color{0, 0, 0, 255}
const blue_c = gg.Color{0, 121, 241, 255}

struct App {
mut:
	ctx   &gg.Context = unsafe { nil }
	thick f32         = 5
}

fn thick_rect(ctx &gg.Context, x f32, y f32, w f32, h f32, t f32, c gg.Color) {
	n := int(math.abs(t))
	for i in 0 .. n + 1 {
		o := f32(i)
		ctx.draw_rect_empty(x - o, y - o, w + o * 2, h + o * 2, c)
	}
}

fn thick_circle(ctx &gg.Context, x f32, y f32, r f32, t f32, c gg.Color) {
	n := int(math.abs(t))
	for i in 0 .. n + 1 {
		ctx.draw_circle_empty(x, y, r - f32(i), c)
	}
}

fn on_frame(data voidptr) {
	mut app := unsafe { &App(data) }
	ctx := app.ctx
	ctx.begin()
	ctx.draw_text(290, 20, 'Thickness ${app.thick:.2f} [+/-]', size: 16, color: black_c)

	ctx.draw_rect_filled(35, 180, 220, 220, lightgray)
	thick_rect(ctx, 35, 180, 220, 220, app.thick, blue_c)
	ctx.draw_text(35, 160, 'DrawRectangleLinesEx()', size: 10, color: black_c)

	ctx.draw_rounded_rect_filled(290, 180, 220, 220, 44, lightgray)
	thick_rect(ctx, 290, 180, 220, 220, app.thick, blue_c)
	ctx.draw_text(290, 160, 'DrawRectangleRoundedLinesEx()', size: 10, color: black_c)

	ctx.draw_circle_filled(655, 290, 110, lightgray)
	thick_circle(ctx, 655, 290, 110, app.thick, blue_c)
	ctx.draw_text(545, 160, 'DrawCircleLinesEx()', size: 10, color: black_c)
	ctx.end()
}

fn on_event(e &gg.Event, data voidptr) {
	mut app := unsafe { &App(data) }
	if e.typ != .key_down {
		return
	}
	match e.key_code {
		.equal, .kp_add { app.thick += 1 }
		.minus, .kp_subtract {
			if app.thick > 0 { app.thick -= 1 }
		}
		else {}
	}
}

fn main() {
	mut app := &App{}
	app.ctx = gg.new_context(
		width:        800
		height:       450
		window_title: 'raylib [shapes] example - outlines thickness'
		user_data:    app
		frame_fn:     on_frame
		event_fn:     on_event
		bg_color:     raywhite
	)
	app.ctx.run()
}
