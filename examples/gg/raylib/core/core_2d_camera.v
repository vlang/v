module main

// raylib [core] example - 2d camera
// A player walks through a generated city while a Camera2D follows.
// A/S rotate, the wheel zooms, R resets the view.

import gg
import math
import rand

const screen_width = 800
const screen_height = 450
const max_buildings = 100

const ray_white = gg.Color{245, 245, 245, 255}
const light_gray = gg.Color{200, 200, 200, 255}
const dark_gray = gg.Color{80, 80, 80, 255}
const maroon = gg.Color{128, 0, 0, 255}
const green = gg.Color{0, 158, 47, 255}
const sky_blue = gg.Color{102, 191, 255, 128}
const blue = gg.Color{0, 121, 241, 255}
const black = gg.Color{0, 0, 0, 255}

struct Building {
mut:
	x      f32
	y      f32
	width  f32
	height f32
	color  gg.Color
}

struct App {
mut:
	gg        &gg.Context = unsafe { nil }
	player    gg.Vec2     = gg.Vec2{400, 280}
	buildings []Building  = []
	cam       gg.Camera2D
}

fn main() {
	mut app := &App{}
	mut spacing := f32(-6000)
	for _ in 0 .. max_buildings {
		w := f32(rand.int_in_range(50, 200) or { 100 })
		h := f32(rand.int_in_range(100, 800) or { 200 })
		c := gg.Color{u8(rand.int_in_range(200, 240) or { 220 }), u8(rand.int_in_range(200, 240) or { 220 }), u8(rand.int_in_range(200, 250) or { 230 }), 255}
		app.buildings << Building{
			x:      spacing
			y:      f32(screen_height) - 130 - h
			width:  w
			height: h
			color:  c
		}
		spacing += w
	}
	app.cam = gg.default_camera2d()
	app.cam.offset = gg.Vec2{f32(screen_width) / 2, f32(screen_height) / 2}
	app.cam.target = gg.Vec2{app.player.x + 20, app.player.y + 20}

	app.gg = gg.new_context(
		width:        screen_width
		height:       screen_height
		window_title: 'raylib [core] example - 2d camera'
		bg_color:     ray_white
		frame_fn:     frame
		event_fn:     on_event
		user_data:    app
	)
	app.gg.run()
}

fn on_event(e &gg.Event, data voidptr) {
	mut app := unsafe { &App(data) }
	if e.typ == .key_down && e.key_code == .r && !e.key_repeat {
		app.cam.zoom = 1
		app.cam.rotation = 0
	}
}

fn frame(data voidptr) {
	mut app := unsafe { &App(data) }
	ctx := app.gg

	if ctx.pressed_keys[int(gg.KeyCode.right)] {
		app.player.x += 2
	} else if ctx.pressed_keys[int(gg.KeyCode.left)] {
		app.player.x -= 2
	}
	if ctx.pressed_keys[int(gg.KeyCode.a)] {
		app.cam.rotation -= 1
	} else if ctx.pressed_keys[int(gg.KeyCode.s)] {
		app.cam.rotation += 1
	}
	if app.cam.rotation > 40 {
		app.cam.rotation = 40
	} else if app.cam.rotation < -40 {
		app.cam.rotation = -40
	}

	app.cam.zoom = f32(math.exp(math.log(f64(app.cam.zoom)) + f64(ctx.scroll_y) * 0.1))
	if app.cam.zoom > 3 {
		app.cam.zoom = 3
	} else if app.cam.zoom < 0.1 {
		app.cam.zoom = 0.1
	}
	app.cam.target = gg.Vec2{app.player.x + 20, app.player.y + 20}

	ctx.begin()
	ctx.begin_2d(app.cam)
	ctx.draw_rect(x: -6000, y: 320, w: 13000, h: 8000, color: dark_gray)
	for b in app.buildings {
		ctx.draw_rect(x: b.x, y: b.y, w: b.width, h: b.height, color: b.color)
	}
	ctx.draw_rect(x: app.player.x, y: app.player.y, w: 40, h: 40, color: gg.red)
	ctx.draw_line(app.cam.target.x, -f32(screen_height * 10), app.cam.target.x,
		f32(screen_height * 10), green)
	ctx.draw_line(-f32(screen_width * 10), app.cam.target.y, f32(screen_width * 10),
		app.cam.target.y, green)
	ctx.end_2d()

	ctx.draw_text(640, 10, 'SCREEN AREA', size: 20, color: gg.red)
	ctx.draw_rect(x: 0, y: 0, w: screen_width, h: 5, color: gg.red)
	ctx.draw_rect(x: 0, y: 5, w: 5, h: screen_height - 10, color: gg.red)
	ctx.draw_rect(x: screen_width - 5, y: 5, w: 5, h: screen_height - 10, color: gg.red)
	ctx.draw_rect(x: 0, y: screen_height - 5, w: screen_width, h: 5, color: gg.red)
	ctx.draw_rect(x: 10, y: 10, w: 250, h: 113, color: sky_blue)
	ctx.draw_rect_empty(10, 10, 250, 113, blue)
	ctx.draw_text(20, 20, 'Free 2D camera controls:', size: 10, color: black)
	ctx.draw_text(40, 40, '- Right/Left to move player', size: 10, color: dark_gray)
	ctx.draw_text(40, 60, '- Mouse Wheel to Zoom in-out', size: 10, color: dark_gray)
	ctx.draw_text(40, 80, '- A / S to Rotate', size: 10, color: dark_gray)
	ctx.draw_text(40, 100, '- R to reset Zoom and Rotation', size: 10, color: dark_gray)
	ctx.end()
}
