module main

// raylib [core] example - automation events
// PORT NOTE: gg has no automation-event API. This example records the
// player's key-input snapshots per frame (S to start/stop, A to replay)
// and shows the same recording/playing indicators as the raylib version.

import gg

const screen_width = 800
const screen_height = 450
const gravity = f32(400)
const jump_speed = f32(350)
const walk_speed = f32(200)
const env_count = 5

const light_gray = gg.Color{200, 200, 200, 255}
const gray = gg.Color{130, 130, 130, 255}
const red = gg.Color{255, 0, 0, 255}
const maroon = gg.Color{128, 0, 0, 255}
const sky_blue = gg.Color{102, 191, 255, 128}
const blue = gg.Color{0, 121, 241, 204}
const lime = gg.Color{124, 252, 0, 128}
const dark_green = gg.Color{0, 121, 64, 204}

struct InputFrame {
mut:
	left  bool
	right bool
	space bool
}

struct App {
mut:
	gg         &gg.Context = unsafe { nil }
	pos        gg.Vec2     = gg.Vec2{400, 280}
	speed      f32
	can_jump   bool
	cam        gg.Camera2D
	recording  bool
	playing    bool
	frames     []InputFrame
	play_index int
	frame_no   int
}

fn main() {
	mut app := &App{}
	app.cam = gg.default_camera2d()
	app.cam.offset = gg.Vec2{f32(screen_width) / 2, f32(screen_height) / 2}
	app.cam.target = app.pos
	app.gg = gg.new_context(
		width:        screen_width
		height:       screen_height
		window_title: 'raylib [core] example - automation events'
		bg_color:     light_gray
		frame_fn:     frame
		update_fn:    update
		event_fn:     on_event
		user_data:    app
	)
	app.gg.run()
}

fn reset(mut app App) {
	app.pos = gg.Vec2{400, 280}
	app.speed = 0
	app.can_jump = false
	app.cam.target = app.pos
}

fn on_event(e &gg.Event, data voidptr) {
	mut app := unsafe { &App(data) }
	if e.typ != .key_down || e.key_repeat {
		return
	}
	if e.key_code == .r {
		reset(mut app)
	}
	if e.key_code == .s && !app.playing {
		app.recording = !app.recording
		if !app.recording && app.frames.len > 0 {
			println('RECORDED FRAMES: ${app.frames.len}')
		}
	}
	if e.key_code == .a && !app.recording && app.frames.len > 0 {
		app.playing = true
		app.play_index = 0
		reset(mut app)
	}
}

fn update(dt f32, data voidptr) {
	mut app := unsafe { &App(data) }
	ctx := app.gg
	mut inp := InputFrame{
		left:  ctx.pressed_keys[int(gg.KeyCode.left)]
		right: ctx.pressed_keys[int(gg.KeyCode.right)]
		space: ctx.pressed_keys[int(gg.KeyCode.space)]
	}
	if app.playing {
		if app.play_index < app.frames.len {
			inp = app.frames[app.play_index]
			app.play_index++
		} else {
			app.playing = false
			app.play_index = 0
			println('FINISH PLAYING!')
		}
	}
	if app.recording {
		app.frames << inp
	}

	if inp.right {
		app.pos.x += walk_speed * dt
	}
	if inp.left {
		app.pos.x -= walk_speed * dt
	}
	if inp.space && app.can_jump {
		app.speed = -jump_speed
		app.can_jump = false
	}

	envs := [gg.Rect{0, 400, 1000, 200}, gg.Rect{300, 200, 400, 10}, gg.Rect{250, 300, 100, 10},
		gg.Rect{650, 300, 100, 10}]
	mut hit := false
	for e in envs {
		if e.x <= app.pos.x && e.x + e.width >= app.pos.x && e.y >= app.pos.y
			&& e.y <= app.pos.y + app.speed * dt {
			hit = true
			app.speed = 0
			app.pos.y = e.y
		}
	}
	if !hit {
		app.pos.y += app.speed * dt
		app.speed += gravity * dt
		app.can_jump = false
	} else {
		app.can_jump = true
	}
	app.cam.target = app.pos
	if app.recording || app.playing {
		app.frame_no++
	} else {
		app.frame_no = 0
	}
}

fn frame(data voidptr) {
	app := unsafe { &App(data) }
	ctx := app.gg
	ctx.begin()
	ctx.begin_2d(app.cam)
	ctx.draw_rect(x: 0, y: 0, w: 1000, h: 400, color: light_gray)
	for e in [gg.Rect{0, 400, 1000, 200}, gg.Rect{300, 200, 400, 10}, gg.Rect{250, 300, 100, 10},
		gg.Rect{650, 300, 100, 10}] {
		ctx.draw_rect(x: e.x, y: e.y, w: e.width, h: e.height, color: gray)
	}
	ctx.draw_rect(x: app.pos.x - 20, y: app.pos.y - 40, w: 40, h: 40, color: red)
	ctx.end_2d()

	ctx.draw_rect(x: 10, y: 10, w: 290, h: 145, color: sky_blue)
	ctx.draw_rect_empty(10, 10, 290, 145, blue)
	ctx.draw_text(20, 20, 'Controls:', size: 10, color: gg.black)
	ctx.draw_text(30, 40, '- RIGHT | LEFT: Player movement', size: 10, color: dark_green)
	ctx.draw_text(30, 60, '- SPACE: Player jump', size: 10, color: dark_green)
	ctx.draw_text(30, 80, '- R: Reset game state', size: 10, color: dark_green)
	ctx.draw_text(30, 110, '- S: START/STOP RECORDING INPUT EVENTS', size: 10, color: gg.black)
	ctx.draw_text(30, 130, '- A: REPLAY LAST RECORDED INPUT EVENTS', size: 10, color: gg.black)
	if app.recording {
		ctx.draw_rect(x: 10, y: 160, w: 290, h: 30, color: gg.Color{255, 0, 0, 77})
		ctx.draw_circle_filled(30, 175, 10, maroon)
		if (app.frame_no / 15) % 2 == 1 {
			ctx.draw_text(50, 170, 'RECORDING EVENTS... [${app.frames.len}]',
				size:  10
				color: maroon
			)
		}
	} else if app.playing {
		ctx.draw_rect(x: 10, y: 160, w: 290, h: 30, color: lime)
		if (app.frame_no / 15) % 2 == 1 {
			ctx.draw_text(50, 170, 'PLAYING RECORDED EVENTS... [${app.play_index}]',
				size:  10
				color: dark_green
			)
		}
	}
	ctx.end()
}
