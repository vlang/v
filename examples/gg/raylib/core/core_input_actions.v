module main

// raylib [core] example - input actions
// Input is decoded into actions that can be remapped with TAB.
// PORT NOTE: gamepad buttons are unavailable in gg, so the alternate
// bindings are keyboard-only; the action abstraction is preserved.

import gg

const screen_width = 800
const screen_height = 450

const gray = gg.Color{130, 130, 130, 255}
const white = gg.Color{255, 255, 255, 255}
const green = gg.Color{0, 158, 47, 255}
const red = gg.Color{255, 0, 0, 255}
const blue = gg.Color{0, 0, 255, 255}

enum Action {
	up
	down
	left
	right
	fire
}

struct App {
mut:
	gg          &gg.Context = unsafe { nil }
	keys        [5]gg.KeyCode
	pos         gg.Vec2 = gg.Vec2{400, 200}
	size        gg.Vec2 = gg.Vec2{40, 40}
	default_set bool
	fire_down   bool
	fire_up     bool
	tab_down    bool
}

fn main() {
	mut app := &App{}
	set_default(mut app)
	app.gg = gg.new_context(
		width:        screen_width
		height:       screen_height
		window_title: 'raylib [core] example - input actions'
		bg_color:     gray
		frame_fn:     frame
		update_fn:    update
		event_fn:     on_event
		user_data:    app
	)
	app.gg.run()
}

fn set_default(mut app App) {
	app.keys = [gg.KeyCode.w, gg.KeyCode.s, gg.KeyCode.a, gg.KeyCode.d, gg.KeyCode.space]!
	app.default_set = true
}

fn set_cursor(mut app App) {
	app.keys = [gg.KeyCode.up, gg.KeyCode.down, gg.KeyCode.left, gg.KeyCode.right, gg.KeyCode.space]!
	app.default_set = false
}

fn on_event(e &gg.Event, data voidptr) {
	mut app := unsafe { &App(data) }
	if e.typ == .key_down {
		if e.key_code == .tab && !e.key_repeat && !app.tab_down {
			if app.default_set {
				set_cursor(mut app)
			} else {
				set_default(mut app)
			}
		}
		if e.key_code == .tab {
			app.tab_down = true
		}
		if e.key_code == app.keys[int(Action.fire)] && !e.key_repeat {
			app.fire_up = false
		}
	}
	if e.typ == .key_up {
		if e.key_code == .tab {
			app.tab_down = false
		}
		if e.key_code == app.keys[int(Action.fire)] {
			app.fire_up = true
		}
	}
}

fn update(dt f32, data voidptr) {
	mut app := unsafe { &App(data) }
	ctx := app.gg
	down := fn (c &gg.Context, k gg.KeyCode) bool {
		return c.pressed_keys[int(k)]
	}
	if down(ctx, app.keys[int(Action.up)]) {
		app.pos.y -= 120 * dt
	}
	if down(ctx, app.keys[int(Action.down)]) {
		app.pos.y += 120 * dt
	}
	if down(ctx, app.keys[int(Action.left)]) {
		app.pos.x -= 120 * dt
	}
	if down(ctx, app.keys[int(Action.right)]) {
		app.pos.x += 120 * dt
	}
	if down(ctx, app.keys[int(Action.fire)]) && !app.fire_down {
		app.pos = gg.Vec2{(screen_width - app.size.x) / 2, (screen_height - app.size.y) / 2}
	}
	app.fire_down = down(ctx, app.keys[int(Action.fire)])
}

fn frame(data voidptr) {
	app := unsafe { &App(data) }
	ctx := app.gg
	ctx.begin()
	ctx.draw_rect(x: app.pos.x, y: app.pos.y, w: app.size.x, h: app.size.y, color: if app.fire_up { blue } else { red })
	ctx.draw_text(10, 10, if app.default_set {
		'Current input set: WASD (default)'
	} else {
		'Current input set: Arrow keys'
	}, size: 20, color: white)
	ctx.draw_text(10, 50, 'Use TAB key to toggles Actions keyset',
		size:  20
		color: green
	)
	ctx.end()
}
