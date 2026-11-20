module main

// raylib [core] example - input gamepad
// PORT NOTE: gg/sokol does not expose gamepad axes, buttons or vibration.
// This example draws a generic controller and lets you drive its on-screen
// buttons with the keyboard (arrows = d-pad, WASD = face buttons), keeping
// the visual intent of the raylib example.

import gg

const screen_width = 800
const screen_height = 450

const ray_white = gg.Color{245, 245, 245, 255}
const dark_gray = gg.Color{80, 80, 80, 255}
const light_gray = gg.Color{200, 200, 200, 255}
const white = gg.Color{255, 255, 255, 255}
const black = gg.Color{0, 0, 0, 255}
const red = gg.Color{255, 0, 0, 255}
const blue = gg.Color{0, 0, 255, 255}
const lime = gg.Color{124, 252, 0, 255}
const gold = gg.Color{255, 203, 0, 255}
const maroon = gg.Color{128, 0, 0, 255}
const sky_blue = gg.Color{102, 191, 255, 255}

struct App {
mut:
	gg &gg.Context = unsafe { nil }
}

fn main() {
	mut app := &App{}
	app.gg = gg.new_context(
		width:        screen_width
		height:       screen_height
		window_title: 'raylib [core] example - input gamepad'
		bg_color:     ray_white
		frame_fn:     frame
		user_data:    app
	)
	app.gg.run()
}

fn pressed(ctx &gg.Context, key gg.KeyCode) bool {
	return ctx.pressed_keys[int(key)]
}

fn frame(data voidptr) {
	app := unsafe { &App(data) }
	ctx := app.gg
	ctx.begin()
	ctx.draw_text(10, 10, 'GP0: keyboard-emulated controller (no gamepad API in gg)',
		size:  10
		color: black
	)
	ctx.draw_rect(x: 175, y: 110, w: 460, h: 220, color: dark_gray)
	// D-pad
	ctx.draw_rect(x: 245, y: 145, w: 28, h: 88, color: white)
	ctx.draw_rect(x: 215, y: 174, w: 88, h: 29, color: white)
	ctx.draw_rect(x: 247, y: 147, w: 24, h: 84, color: black)
	ctx.draw_rect(x: 217, y: 176, w: 84, h: 25, color: black)
	if pressed(ctx, .up) {
		ctx.draw_rect(x: 247, y: 147, w: 24, h: 29, color: red)
	}
	if pressed(ctx, .down) {
		ctx.draw_rect(x: 247, y: 201, w: 24, h: 30, color: red)
	}
	if pressed(ctx, .left) {
		ctx.draw_rect(x: 217, y: 176, w: 30, h: 25, color: red)
	}
	if pressed(ctx, .right) {
		ctx.draw_rect(x: 271, y: 176, w: 30, h: 25, color: red)
	}
	// Face buttons
	face := [gg.Vec2{516, 191}, gg.Vec2{551, 227}, gg.Vec2{587, 191}, gg.Vec2{551, 155}]
	keys := [gg.KeyCode.w, gg.KeyCode.d, gg.KeyCode.s, gg.KeyCode.a]
	colors := [gold, blue, lime, red]
	for i, p in face {
		ctx.draw_circle_filled(p.x, p.y, 17, if pressed(ctx, keys[i]) { colors[i] } else { white })
	}
	// Sticks controlled with IJKL
	lx := f32(0)
	ly := f32(0)
	if pressed(ctx, .i) { ly -= 20 }
	if pressed(ctx, .k) { ly += 20 }
	if pressed(ctx, .j) { lx -= 20 }
	if pressed(ctx, .l) { lx += 20 }
	ctx.draw_circle_filled(345, 260, 40, black)
	ctx.draw_circle_filled(345, 260, 35, light_gray)
	ctx.draw_circle_filled(345 + lx, 260 + ly, 25, maroon)
	ctx.draw_circle_filled(465, 260, 40, black)
	ctx.draw_circle_filled(465, 260, 35, light_gray)
	ctx.draw_circle_filled(465, 260, 25, black)
	ctx.draw_rect(x: 10, y: 50, w: 220, h: 24, color: sky_blue)
	ctx.draw_text(14, 54, 'VIBRATE (not available)', size: 10, color: dark_gray)
	ctx.draw_text(10, 430, 'PORT NOTE: no gamepad APIs in gg/sokol.',
		size:  10
		color: gray
	)
	ctx.end()
}
