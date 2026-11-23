module main

// raylib [text] example - input box, ported to gg.
import gg

const (
	max_chars = 9
	raywhite  = gg.Color{245, 245, 245, 255}
	lightgray = gg.Color{200, 200, 200, 255}
	gray_c    = gg.Color{130, 130, 130, 255}
	darkgray  = gg.Color{80, 80, 80, 255}
	red_c     = gg.Color{230, 41, 55, 255}
	maroon    = gg.Color{190, 33, 55, 255}
)

struct App {
mut:
	ctx     &gg.Context = unsafe { nil }
	name    string
	mx, my  f32
	on_text bool
	frames  int
}

fn on_frame(data voidptr) {
	mut app := unsafe { &App(data) }
	ctx := app.ctx
	ctx.begin()
	tx, ty, tw, th := f32(300), f32(180), f32(225), f32(50)
	ctx.draw_text(240, 140, 'PLACE MOUSE OVER INPUT BOX!', size: 20, color: gray_c)
	ctx.draw_rect_filled(tx, ty, tw, th, lightgray)
	ctx.draw_rect_empty(tx, ty, tw, th, if app.on_text { red_c } else { darkgray })
	ctx.draw_text(int(tx) + 5, int(ty) + 8, app.name, size: 40, color: maroon)
	ctx.draw_text(315, 250, 'INPUT CHARS: ${app.name.len}/${max_chars}',
		size:  20
		color: darkgray
	)
	if app.on_text {
		if app.name.len < max_chars {
			if (app.frames / 20) % 2 == 0 {
				ctx.draw_text(int(tx) + 8 + ctx.text_width(app.name), int(ty) + 12, '_',
					size:  40
					color: maroon
				)
			}
		} else {
			ctx.draw_text(230, 300, 'Press BACKSPACE to delete chars...', size: 20, color: gray_c)
		}
	}
	ctx.end()
}

fn on_event(e &gg.Event, data voidptr) {
	mut app := unsafe { &App(data) }
	if e.typ == .char {
		ch := rune(e.char_code)
		if ch >= 32 && ch <= 125 && app.name.len < max_chars {
			app.name += '${ch}'
		}
	}
	if e.typ == .key_down && e.key_code == .backspace && app.name.len > 0 {
		app.name = app.name[..app.name.len - 1]
	}
}

fn on_update(dt f32, data voidptr) {
	mut app := unsafe { &App(data) }
	_ = dt
	app.mx = f32(app.ctx.mouse_pos_x)
	app.my = f32(app.ctx.mouse_pos_y)
	app.on_text = app.mx >= 300 && app.mx <= 525 && app.my >= 180 && app.my <= 230
	if app.on_text { app.frames++ } else { app.frames = 0 }
}

fn main() {
	mut app := &App{}
	app.ctx = gg.new_context(
		width:        800
		height:       450
		window_title: 'raylib [text] example - input box'
		user_data:    app
		frame_fn:     on_frame
		event_fn:     on_event
		update_fn:    on_update
		bg_color:     raywhite
	)
	app.ctx.run()
}
