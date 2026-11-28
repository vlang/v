module main

// raylib [shapes] example - logo raylib anim, ported to gg.
import gg

const raywhite = gg.Color{245, 245, 245, 255}
const black_c = gg.Color{0, 0, 0, 255}
const gray_c = gg.Color{130, 130, 130, 255}

struct App {
mut:
	ctx      &gg.Context = unsafe { nil }
	frames   int
	letters  int
	top_w    int = 16
	left_h   int = 16
	bottom_w int = 16
	right_h  int = 16
	state    int
	alpha    f32 = 1
}

fn on_frame(data voidptr) {
	mut app := unsafe { &App(data) }
	ctx := app.ctx
	ctx.begin()
	lx, ly := ctx.width / 2 - 128, ctx.height / 2 - 128
	mut c := black_c
	c.a = u8(255 * app.alpha)
	match app.state {
		0 {
			if (app.frames / 15) % 2 == 1 {
				ctx.draw_rect_filled(lx, ly, 16, 16, c)
			}
		}
		1 {
			ctx.draw_rect_filled(lx, ly, app.top_w, 16, c)
			ctx.draw_rect_filled(lx, ly, 16, app.left_h, c)
		}
		2, 3 {
			ctx.draw_rect_filled(lx, ly, app.top_w, 16, c)
			ctx.draw_rect_filled(lx, ly + 16, 16, app.left_h - 32, c)
			ctx.draw_rect_filled(lx + 240, ly + 16, 16, app.right_h - 32, c)
			ctx.draw_rect_filled(lx, ly + 240, app.bottom_w, 16, c)
			if app.state == 3 {
				mut bg := raywhite
				bg.a = u8(255 * app.alpha)
				ctx.draw_rect_filled(ctx.width / 2 - 112, ctx.height / 2 - 112, 224, 224, bg)
				sub := 'raylib'
				if app.letters < sub.len {
					ctx.draw_text(ctx.width / 2 - 44, ctx.height / 2 + 48, sub[..app.letters],
						size:  50
						color: c
					)
				} else {
					ctx.draw_text(ctx.width / 2 - 44, ctx.height / 2 + 48, sub,
						size:  50
						color: c
					)
				}
			}
		}
		4 {
			ctx.draw_text(340, 200, '[R] REPLAY', size: 20, color: gray_c)
		}
		else {}
	}
	ctx.end()
}

fn on_event(e &gg.Event, data voidptr) {
	mut app := unsafe { &App(data) }
	if e.typ == .key_down && e.key_code == .r && app.state == 4 {
		app.frames = 0
		app.letters = 0
		app.top_w = 16
		app.left_h = 16
		app.bottom_w = 16
		app.right_h = 16
		app.alpha = 1
		app.state = 0
	}
}

fn on_update(dt f32, data voidptr) {
	mut app := unsafe { &App(data) }
	_ = dt
	match app.state {
		0 {
			app.frames++
			if app.frames == 120 {
				app.state = 1
				app.frames = 0
			}
		}
		1 {
			app.top_w += 4
			app.left_h += 4
			if app.top_w == 256 { app.state = 2 }
		}
		2 {
			app.bottom_w += 4
			app.right_h += 4
			if app.bottom_w == 256 { app.state = 3 }
		}
		3 {
			app.frames++
			if app.frames >= 12 {
				app.letters++
				app.frames = 0
			}
			if app.letters >= 10 {
				app.alpha -= 0.02
				if app.alpha <= 0 {
					app.alpha = 0
					app.state = 4
				}
			}
		}
		else {}
	}
}

fn main() {
	mut app := &App{}
	app.ctx = gg.new_context(
		width:        800
		height:       450
		window_title: 'raylib [shapes] example - logo raylib anim'
		user_data:    app
		frame_fn:     on_frame
		event_fn:     on_event
		update_fn:    on_update
		bg_color:     raywhite
	)
	app.ctx.run()
}
