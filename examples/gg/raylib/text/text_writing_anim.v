module main

// raylib [text] example - writing anim, ported to gg.
import gg

const (
	msg = 'This sample illustrates a text writing\nanimation effect! Check it out! ;)'
	raywhite = gg.Color{245, 245, 245, 255}
	maroon   = gg.Color{190, 33, 55, 255}
	lightgray = gg.Color{200, 200, 200, 255}
)

struct App {
mut:
	ctx    &gg.Context = unsafe { nil }
	frames int
}

fn on_frame(data voidptr) {
	mut app := unsafe { &App(data) }
	ctx := app.ctx
	ctx.begin()
	n := app.frames / 10
	shown := if n >= msg.len { msg } else { msg[..n] }
	ctx.draw_text(210, 160, shown, size: 20, color: maroon)
	ctx.draw_text(240, 260, 'PRESS [ENTER] to RESTART!', size: 20, color: lightgray)
	ctx.draw_text(239, 300, 'HOLD [SPACE] to SPEED UP!', size: 20, color: lightgray)
	ctx.end()
}

fn on_event(e &gg.Event, data voidptr) {
	mut app := unsafe { &App(data) }
	if e.typ == .key_down && e.key_code == .enter {
		app.frames = 0
	}
}

fn on_update(dt f32, data voidptr) {
	mut app := unsafe { &App(data) }
	_ = dt
	app.frames += if app.ctx.pressed_keys[gg.KeyCode.space] { 8 } else { 1 }
}

fn main() {
	mut app := &App{}
	app.ctx = gg.new_context(
		width:        800
		height:       450
		window_title: 'raylib [text] example - writing anim'
		user_data:    app
		frame_fn:     on_frame
		event_fn:     on_event
		update_fn:    on_update
		bg_color:     raywhite
	)
	app.ctx.run()
}
