module main

// raylib [text] example - words alignment, ported to gg.
import gg
import time

const (
	darkblue  = gg.Color{0, 82, 172, 255}
	blue_c    = gg.Color{0, 121, 241, 255}
	lightgray = gg.Color{200, 200, 200, 255}
	raywhite  = gg.Color{245, 245, 245, 255}
	h_names = ['Left', 'Centre', 'Right']!
	v_names = ['Top', 'Middle', 'Bottom']!
	words = [
		'raylib', 'is', 'a', 'simple', 'and', 'easy-to-use', 'library', 'to', 'enjoy',
		'videogames', 'programming',
	]!
)

struct App {
mut:
	ctx    &gg.Context = unsafe { nil }
	h_align int = 1
	v_align int = 1
	start   i64
}

fn on_frame(data voidptr) {
	mut app := unsafe { &App(data) }
	ctx := app.ctx
	ctx.begin()
	ctx.draw_text(20, 20, 'Use Arrow Keys to change the text alignment',
		size:  20
		color: lightgray
	)
	ctx.draw_text(20, 40, 'Alignment: Horizontal = ${h_names[app.h_align]}, Vertical = ${v_names[app.v_align]}',
		size:  20
		color: lightgray
	)
	rx, ry := f32(200), f32(75)
	rw, rh := f32(400), f32(300)
	ctx.draw_rect_filled(rx, ry, rw, rh, blue_c)
	elapsed := f32(time.now().unix() - app.start)
	word := words[int(elapsed) % words.len]
	tw := f32(ctx.text_width(word))
	th := f32(40)
	x := rx + (rw - tw) * f32(app.h_align) / 2
	y := ry + (rh - th) * f32(app.v_align) / 2
	ctx.draw_text(int(x), int(y), word, size: 40, color: raywhite)
	ctx.end()
}

fn on_event(e &gg.Event, data voidptr) {
	mut app := unsafe { &App(data) }
	if e.typ != .key_down { return }
	match e.key_code {
		.left { if app.h_align > 0 { app.h_align-- } }
		.right { if app.h_align < 2 { app.h_align++ } }
		.up { if app.v_align > 0 { app.v_align-- } }
		.down { if app.v_align < 2 { app.v_align++ } }
		else {}
	}
}

fn main() {
	mut app := &App{ start: time.now().unix() }
	app.ctx = gg.new_context(
		width:        800
		height:       450
		window_title: 'raylib [text] example - words alignment'
		user_data:    app
		frame_fn:     on_frame
		event_fn:     on_event
		bg_color:     darkblue
	)
	app.ctx.run()
}
