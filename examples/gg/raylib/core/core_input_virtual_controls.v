module main

// raylib [core] example - input virtual controls
// An on-screen D-pad moves the player. Mouse or touch presses on a button
// select the direction; gg has no built-in virtual-control widget.

import gg
import math

const screen_width = 800
const screen_height = 450
const button_radius = f32(30)

const ray_white = gg.Color{245, 245, 245, 255}
const dark_gray = gg.Color{80, 80, 80, 255}
const black = gg.Color{0, 0, 0, 255}
const maroon = gg.Color{128, 0, 0, 255}
const yellow = gg.Color{255, 203, 0, 255}
const blue = gg.Color{0, 0, 255, 255}
const red = gg.Color{255, 0, 0, 255}
const green = gg.Color{0, 158, 47, 255}

struct App {
mut:
	gg      &gg.Context = unsafe { nil }
	pad     gg.Vec2     = gg.Vec2{100, 350}
	buttons [4]gg.Vec2
	player  gg.Vec2 = gg.Vec2{400, 225}
	pressed int     = -1
}

fn main() {
	mut app := &App{}
	r := button_radius * 1.5
	app.buttons = [
		gg.Vec2{app.pad.x, app.pad.y - r},
		gg.Vec2{app.pad.x - r, app.pad.y},
		gg.Vec2{app.pad.x + r, app.pad.y},
		gg.Vec2{app.pad.x, app.pad.y + r},
	]!
	app.gg = gg.new_context(
		width:        screen_width
		height:       screen_height
		window_title: 'raylib [core] example - input virtual controls'
		bg_color:     ray_white
		frame_fn:     frame
		update_fn:    update
		event_fn:     on_event
		user_data:    app
	)
	app.gg.run()
}

fn on_event(e &gg.Event, data voidptr) {
	mut app := unsafe { &App(data) }
	if e.typ == .touches_moved || e.typ == .touches_began {
		if e.num_touches > 0 {
			app.update_pressed(e.touches[0].pos_x, e.touches[0].pos_y, true)
		}
	} else if e.typ == .touches_ended || e.typ == .touches_cancelled {
		app.pressed = -1
	} else if e.typ == .mouse_down {
		app.update_pressed(e.mouse_x, e.mouse_y, true)
	} else if e.typ == .mouse_up {
		app.pressed = -1
	}
}

fn update_pressed(mut app App, x f32, y f32, is_down bool) {
	app.pressed = -1
	if !is_down {
		return
	}
	for i in 0 .. 4 {
		b := app.buttons[i]
		if math.abs(b.x - x) + math.abs(b.y - y) < button_radius {
			app.pressed = i
			break
		}
	}
}

fn update(dt f32, data voidptr) {
	mut app := unsafe { &App(data) }
	match app.pressed {
		0 { app.player.y -= 75 * dt }
		1 { app.player.x -= 75 * dt }
		2 { app.player.x += 75 * dt }
		3 { app.player.y += 75 * dt }
		else {}
	}
}

fn frame(data voidptr) {
	app := unsafe { &App(data) }
	ctx := app.gg
	ctx.begin()
	ctx.draw_circle_filled(app.player.x, app.player.y, 50, maroon)
	labels := [yellow, blue, red, green]
	for i, b in app.buttons {
		ctx.draw_circle_filled(b.x, b.y, button_radius,
			if i == app.pressed { dark_gray } else { black })
		// Simple arrow triangles pointing up/left/right/down.
		match i {
			0 {
				ctx.draw_triangle_filled(b.x, b.y - 12, b.x - 9, b.y + 9, b.x + 9, b.y + 9, labels[i])
			}
			1 {
				ctx.draw_triangle_filled(b.x + 9, b.y - 9, b.x - 12, b.y, b.x + 9, b.y + 9, labels[i])
			}
			2 {
				ctx.draw_triangle_filled(b.x + 12, b.y, b.x - 9, b.y - 9, b.x - 9, b.y + 9, labels[i])
			}
			3 {
				ctx.draw_triangle_filled(b.x - 9, b.y - 9, b.x, b.y + 12, b.x + 9, b.y - 9, labels[i])
			}
			else {}
		}
	}
	ctx.draw_text(10, 10, 'move the player with D-Pad buttons',
		size:  20
		color: dark_gray
	)
	ctx.end()
}
