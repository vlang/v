module main

// raylib [text] example - rectangle bounds, ported to gg.
// PORT NOTE: gg has no glyph-level text-box layout. Word wrapping is done by
// splitting on words and measuring with ctx.text_width.
import gg

const (
	text = "Text cannot escape\tthis container\t...word wrap also works when active so here's a long text for testing.\n\nLorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Nec ullamcorper sit amet risus nullam eget felis eget."
	raywhite = gg.Color{245, 245, 245, 255}
	gray_c   = gg.Color{130, 130, 130, 255}
	black_c  = gg.Color{0, 0, 0, 255}
	maroon   = gg.Color{190, 33, 55, 255}
	maroon_t = gg.Color{190, 33, 55, 102}
	red_c    = gg.Color{230, 41, 55, 255}
)

struct App {
mut:
	ctx      &gg.Context = unsafe { nil }
	x, y     f32 = 25, 25
	w, h     f32
	word_wrap bool = true
	resizing bool
	last_x, last_y f32
	col      gg.Color = maroon
}

fn draw_boxed(ctx &gg.Context, x f32, y f32, w f32, h f32, wrap bool) {
	line_h := f32(26)
	mut cy := y
	for para in text.split('\n') {
		if wrap {
			mut line := ''
			for word in para.split(' ') {
				candidate := if line == '' { word } else { line + ' ' + word }
				if f32(ctx.text_width(candidate)) > w && line != '' {
					ctx.draw_text(int(x), int(cy), line, size: 20, color: gray_c)
					cy += line_h
					line = word
				} else {
					line = candidate
				}
			}
			ctx.draw_text(int(x), int(cy), line, size: 20, color: gray_c)
			cy += line_h
		} else {
			ctx.draw_text(int(x), int(cy), para, size: 20, color: gray_c)
			cy += line_h
		}
		if cy > y + h { break }
	}
}

fn on_frame(data voidptr) {
	mut app := unsafe { &App(data) }
	ctx := app.ctx
	ctx.begin()
	// 1px border approximation of DrawRectangleLinesEx
	ctx.draw_rect_empty(app.x, app.y, app.w, app.h, app.col)
	ctx.draw_rect_empty(app.x + 1, app.y + 1, app.w - 2, app.h - 2, app.col)
	ctx.draw_rect_empty(app.x + 2, app.y + 2, app.w - 4, app.h - 4, app.col)
	draw_boxed(ctx, app.x + 4, app.y + 4, app.w - 8, app.h - 8, app.word_wrap)
	rx, ry := app.x + app.w - 17, app.y + app.h - 17
	ctx.draw_rect_filled(rx, ry, 14, 14, app.col)
	bottom := ctx.height - 54
	ctx.draw_rect_filled(0, bottom, f32(ctx.width), 54, gray_c)
	ctx.draw_rect_filled(382, bottom + 20, 12, 12, maroon)
	ctx.draw_text(313, bottom - 61, 'Word Wrap: ', size: 20, color: black_c)
	if app.word_wrap {
		ctx.draw_text(447, bottom - 61, 'ON', size: 20, color: red_c)
	} else {
		ctx.draw_text(447, bottom - 61, 'OFF', size: 20, color: black_c)
	}
	ctx.draw_text(218, bottom - 32, 'Press [SPACE] to toggle word wrap', size: 20, color: gray_c)
	ctx.draw_text(155, bottom + 16, 'Click hold & drag the    to resize the container', size: 20, color: raywhite)
	ctx.end()
}

fn on_event(e &gg.Event, data voidptr) {
	mut app := unsafe { &App(data) }
	if e.typ == .key_down && e.key_code == .space {
		app.word_wrap = !app.word_wrap
	}
	mx, my := f32(e.mouse_x), f32(e.mouse_y)
	if e.typ == .mouse_down && e.mouse_button == .left && mx >= app.x + app.w - 17 &&
		mx <= app.x + app.w && my >= app.y + app.h - 17 && my <= app.y + app.h {
		app.resizing = true
	}
	if e.typ == .mouse_up && e.mouse_button == .left { app.resizing = false }
}

fn on_update(dt f32, data voidptr) {
	mut app := unsafe { &App(data) }
	_ = dt
	mx, my := f32(app.ctx.mouse_pos_x), f32(app.ctx.mouse_pos_y)
	if mx >= app.x && mx <= app.x + app.w && my >= app.y && my <= app.y + app.h {
		app.col = maroon_t
	} else if !app.resizing { app.col = maroon }
	if app.resizing {
		app.w += mx - app.last_x
		app.h += my - app.last_y
		if app.w < 60 { app.w = 60 }
		if app.w > f32(app.ctx.width) - 50 { app.w = f32(app.ctx.width) - 50 }
		if app.h < 60 { app.h = 60 }
		if app.h > f32(app.ctx.height) - 160 { app.h = f32(app.ctx.height) - 160 }
	}
	app.last_x, app.last_y = mx, my
}

fn main() {
	mut app := &App{ w: 750, h: 200 }
	app.ctx = gg.new_context(
		width:        800
		height:       450
		window_title: 'raylib [text] example - rectangle bounds'
		user_data:    app
		frame_fn:     on_frame
		event_fn:     on_event
		update_fn:    on_update
		bg_color:     raywhite
	)
	app.ctx.run()
}
