module main

import gg
import rand
import sokol.audio
import math

const paddle_speed_base = f32(400.0)
const ball_speed_base = f32(300.0)

// Colors
const color_bg = gg.Color{10, 15, 30, 255}
const color_foreground = gg.Color{230, 230, 230, 255}
const color_accent = gg.Color{0, 255, 100, 255}
const color_secondary = gg.Color{0, 200, 80, 255}
const color_text_dim = gg.Color{150, 150, 150, 200}
const color_line = gg.Color{100, 100, 100, 150}
const color_p1 = gg.Color{255, 50, 50, 255}
const color_p2 = gg.Color{50, 100, 255, 255}

struct SoundManager {
mut:
	ping []f32
	pong []f32
	buzz []f32
}

fn (mut sm SoundManager) init() {
	sample_rate := f32(audio.sample_rate())

	// Ping (Wall)
	ping_duration := f32(0.1)
	ping_frames := int(sample_rate * ping_duration)
	for i in 0 .. ping_frames {
		t := f32(i) / sample_rate
		// Use a steeper exponential-like decay for a cleaner "pluck"
		env := f32(math.pow(f32(1.0) - t / ping_duration, 3))
		sm.ping << f32(0.3) * math.sinf(t * 800.0 * 2 * math.pi) * env
	}
	// Absolute silence buffer to prevent clicks
	for _ in 0 .. 100 {
		sm.ping << 0
	}

	// Pong (Paddle)
	pong_duration := f32(0.1)
	pong_frames := int(sample_rate * pong_duration)
	for i in 0 .. pong_frames {
		t := f32(i) / sample_rate
		env := f32(math.pow(f32(1.0) - t / pong_duration, 3))
		sm.pong << f32(0.3) * math.sinf(t * 400.0 * 2 * math.pi) * env
	}
	for _ in 0 .. 100 {
		sm.pong << 0
	}

	// Buzz (Reset)
	buzz_duration := f32(0.3)
	buzz_frames := int(sample_rate * buzz_duration)
	for i in 0 .. buzz_frames {
		t := f32(i) / sample_rate
		env := f32(math.pow(f32(1.0) - t / buzz_duration, 2))
		// Add a harmonic for a more "buzzy" feel
		val := f32(0.25) * math.sinf(t * 100.0 * 2 * math.pi) +
			f32(0.1) * math.sinf(t * 200.0 * 2 * math.pi)
		sm.buzz << val * env
	}
	for _ in 0 .. 200 {
		sm.buzz << 0
	}
}

struct App {
mut:
	ctx           &gg.Context = unsafe { nil }
	width         int         = 800
	height        int         = 600
	paddle_width  f32
	paddle_height f32
	ball_size     f32
	p1_y          f32
	p2_y          f32
	ball_x        f32
	ball_y        f32
	ball_vx       f32
	ball_vy       f32
	p1_score      int
	p2_score      int
	paused        bool
	sounds        SoundManager
}

fn (mut app App) update_sizes() {
	size := app.ctx.window_size()
	app.width = size.width
	app.height = size.height

	// Base scales on 800x600
	scale_x := f32(app.width) / 800.0
	scale_y := f32(app.height) / 600.0

	app.paddle_width = 15.0 * scale_x
	app.paddle_height = 80.0 * scale_y
	app.ball_size = 15.0 * scale_x // Keep it circular based on width scale
}

fn (mut app App) reset_ball() {
	app.ball_x = f32(app.width) / 2 - app.ball_size / 2
	app.ball_y = f32(app.height) / 2 - app.ball_size / 2

	scale_x := f32(app.width) / 800.0
	mut vx := ball_speed_base * scale_x
	if rand.intn(2) or { 0 } == 0 {
		vx = -vx
	}
	app.ball_vx = vx
	app.ball_vy = (f32(rand.intn(301) or { 150 } - 150)) * (f32(app.height) / 600.0)
}

fn (mut app App) reset_game() {
	app.p1_score = 0
	app.p2_score = 0
	app.p1_y = f32(app.height) / 2 - app.paddle_height / 2
	app.p2_y = f32(app.height) / 2 - app.paddle_height / 2
	app.reset_ball()
	app.paused = false
}

fn on_frame(data voidptr) {
	mut app := unsafe { &App(data) }
	app.ctx.begin()

	// Draw dashed center line
	line_segments := 20
	segment_height := f32(app.height) / line_segments
	for i in 0 .. line_segments {
		if i % 2 == 0 {
			app.ctx.draw_rect_filled(f32(app.width) / 2 - 1, i * segment_height, 2, segment_height,
				color_line)
		}
	}

	// Draw paddles
	app.ctx.draw_rounded_rect_filled(20, app.p1_y, app.paddle_width, app.paddle_height,
		5, color_p1)
	app.ctx.draw_rounded_rect_filled(f32(app.width) - 20 - app.paddle_width, app.p2_y,
		app.paddle_width, app.paddle_height, 5, color_p2)

	// Draw ball
	app.ctx.draw_circle_filled(app.ball_x + app.ball_size / 2, app.ball_y + app.ball_size / 2,
		app.ball_size / 2, color_foreground)

	// Draw ball speed (10m = 800px => 1m = 80px)
	// We scale the meter definition with width
	meter_px := 80.0 * (f32(app.width) / 800.0)
	speed_px := math.sqrt(app.ball_vx * app.ball_vx + app.ball_vy * app.ball_vy)
	speed_ms := speed_px / meter_px
	app.ctx.draw_text(app.width / 2, 20, 'Ball: ${speed_ms:.1f} m/s',
		size:  20
		color: color_foreground
		align: .center
	)

	// Draw scores and positions
	// Moved up to be less obtrusive
	app.ctx.draw_text(app.width / 4, 30, app.p1_score.str(),
		size:  40
		color: color_accent
		align: .center
	)
	app.ctx.draw_text(app.width / 4, 80, 'Y: ${int(app.p1_y)}',
		size:  16
		color: color_secondary
		align: .center
	)

	app.ctx.draw_text(3 * app.width / 4, 30, app.p2_score.str(),
		size:  40
		color: color_accent
		align: .center
	)
	app.ctx.draw_text(3 * app.width / 4, 80, 'Y: ${int(app.p2_y)}',
		size:  16
		color: color_secondary
		align: .center
	)

	if app.paused {
		app.ctx.draw_text(app.width / 2, app.height / 2 - 50, 'PAUSED',
			size:  64
			color: color_accent
			align: .center
		)
		app.ctx.draw_text(app.width / 2, app.height / 2 + 10, 'Press SPACE to Resume',
			size:  20
			color: color_accent
			align: .center
		)
	} else {
		app.ctx.draw_text(app.width / 2, app.height - 25, 'SPACE: Pause | W/S: P1 | UP/DOWN: P2 | R: Reset',
			size:  16
			color: color_text_dim
			align: .center
		)
	}

	app.ctx.end()
}

fn on_event(e &gg.Event, data voidptr) {
	mut app := unsafe { &App(data) }
	if e.typ == .resized || e.typ == .restored {
		app.update_sizes()
	}
	if e.typ == .key_down {
		match e.key_code {
			.space { app.paused = !app.paused }
			.r { app.reset_game() }
			else {}
		}
	}
}

fn on_update(dt f32, data voidptr) {
	mut app := unsafe { &App(data) }

	if app.paused {
		return
	}

	scale_y := f32(app.height) / 600.0
	paddle_speed := paddle_speed_base * scale_y

	// Paddle 1 movement (W/S)
	if app.ctx.pressed_keys[gg.KeyCode.w] {
		app.p1_y -= paddle_speed * dt
	}
	if app.ctx.pressed_keys[gg.KeyCode.s] {
		app.p1_y += paddle_speed * dt
	}

	// Paddle 2 movement (Up/Down)
	if app.ctx.pressed_keys[gg.KeyCode.up] {
		app.p2_y -= paddle_speed * dt
	}
	if app.ctx.pressed_keys[gg.KeyCode.down] {
		app.p2_y += paddle_speed * dt
	}

	// Constrain paddles
	if app.p1_y < 0 {
		app.p1_y = 0
	}
	if app.p1_y > f32(app.height) - app.paddle_height {
		app.p1_y = f32(app.height) - app.paddle_height
	}
	if app.p2_y < 0 {
		app.p2_y = 0
	}
	if app.p2_y > f32(app.height) - app.paddle_height {
		app.p2_y = f32(app.height) - app.paddle_height
	}

	// Ball movement
	app.ball_x += app.ball_vx * dt
	app.ball_y += app.ball_vy * dt

	// Ball wall collision (Top/Bottom)
	if app.ball_y <= 0 {
		app.ball_y = 0
		app.ball_vy = -app.ball_vy
		audio.push(app.sounds.ping.data, app.sounds.ping.len)
	} else if app.ball_y >= f32(app.height) - app.ball_size {
		app.ball_y = f32(app.height) - app.ball_size
		app.ball_vy = -app.ball_vy
		audio.push(app.sounds.ping.data, app.sounds.ping.len)
	}

	// Ball paddle collision
	// P1
	if app.ball_vx < 0 && app.ball_x <= 20 + app.paddle_width && app.ball_x >= 20 {
		if app.ball_y + app.ball_size >= app.p1_y && app.ball_y <= app.p1_y + app.paddle_height {
			app.ball_x = 20 + app.paddle_width
			app.ball_vx = -app.ball_vx * 1.05 // Slightly speed up
			// Add some vertical velocity based on where it hit the paddle
			hit_pos := (app.ball_y + app.ball_size / 2) - (app.p1_y + app.paddle_height / 2)
			app.ball_vy += hit_pos * 5
			audio.push(app.sounds.pong.data, app.sounds.pong.len)
		}
	}

	// P2
	if app.ball_vx > 0 && app.ball_x + app.ball_size >= f32(app.width) - 20 - app.paddle_width
		&& app.ball_x + app.ball_size <= f32(app.width) - 20 {
		if app.ball_y + app.ball_size >= app.p2_y && app.ball_y <= app.p2_y + app.paddle_height {
			app.ball_x = f32(app.width) - 20 - app.paddle_width - app.ball_size
			app.ball_vx = -app.ball_vx * 1.05 // Slightly speed up
			// Add some vertical velocity based on where it hit the paddle
			hit_pos := (app.ball_y + app.ball_size / 2) - (app.p2_y + app.paddle_height / 2)
			app.ball_vy += hit_pos * 5
			audio.push(app.sounds.pong.data, app.sounds.pong.len)
		}
	}

	// Score
	if app.ball_x < 0 {
		app.p2_score++
		app.reset_ball()
		audio.push(app.sounds.buzz.data, app.sounds.buzz.len)
	} else if app.ball_x > f32(app.width) {
		app.p1_score++
		app.reset_ball()
		audio.push(app.sounds.buzz.data, app.sounds.buzz.len)
	}
}

fn main() {
	mut app := &App{}
	app.width = 800
	app.height = 600
	app.paddle_width = 15.0
	app.paddle_height = 80.0
	app.ball_size = 15.0
	app.p1_y = f32(app.height) / 2 - app.paddle_height / 2
	app.p2_y = f32(app.height) / 2 - app.paddle_height / 2
	app.reset_ball()

	audio.setup(buffer_frames: 512)
	app.sounds.init()

	app.ctx = gg.new_context(
		width:         app.width
		height:        app.height
		window_title:  'V Pong'
		user_data:     app
		frame_fn:      on_frame
		event_fn:      on_event
		update_fn:     on_update
		bg_color:      color_bg
		resizable:     true
		create_window: true
	)

	app.ctx.run()
}
