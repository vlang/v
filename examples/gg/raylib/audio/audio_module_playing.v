// Port of raylib [audio] example - module playing.
// PORT NOTE: gg cannot decode .xm modules and has no pitch control. A looping WAV stands in
// for the module; "speed" only scales the visual progress bar, not the audio pitch.
module main

import gg
import rand

const win_width = 800
const win_height = 450
const max_circles = 64

struct CircleWave {
mut:
	x      f32
	y      f32
	radius f32
	alpha  f32
	speed  f32
	color  gg.Color
}

struct App {
mut:
	gg          &gg.Context = unsafe { nil }
	circles     []CircleWave
	music       &gg.Sound = unsafe { nil }
	pitch       f32       = 1.0
	time_played f32
	pause       bool
}

fn init(data voidptr) {
	mut app := unsafe { &App(data) }
	gg.setup_sound_audio(sample_rate: 44100, channels: 1)
	colors := [gg.orange, gg.red, gg.yellow, gg.green, gg.blue, gg.violet, gg.rgb(165, 42, 42),
		gg.light_gray, gg.pink, gg.yellow, gg.green, gg.light_blue, gg.purple, gg.rgb(245, 245, 220)]
	for _ in 0 .. max_circles {
		r := f32(rand.intn(31) or { 10 } + 10)
		app.circles << CircleWave{
			radius: r
			alpha:  0
			x:      f32(rand.intn(win_width - int(r) * 2) or { 100 }) + r
			y:      f32(rand.intn(win_height - int(r) * 2) or { 100 }) + r
			speed:  f32(rand.intn(100) or { 50 }) / 2000.0
			color:  colors[rand.intn(colors.len) or { 0 }]
		}
	}
	s := gg.load_wav('resources/mini1111.wav') or {
		eprintln('mini1111.wav (XM stand-in): ${err}')
		return
	}
	app.music = &s
	gg.play_sound(app.music)
}

fn event(e &gg.Event, data voidptr) {
	if e.typ != .key_down {
		return
	}
	mut app := unsafe { &App(data) }
	match e.key_code {
		.space {
			if !isnil(app.music) {
				gg.stop_sound(app.music)
				gg.play_sound(app.music)
			}
			app.pause = false
		}
		.p {
			app.pause = !app.pause
			if !isnil(app.music) {
				if app.pause {
					gg.stop_sound(app.music)
				} else {
					gg.play_sound(app.music)
				}
			}
		}
		else {}
	}
}

fn update(dt f32, data voidptr) {
	mut app := unsafe { &App(data) }
	if app.gg.pressed_keys[gg.KeyCode.down] {
		app.pitch -= 0.01 * dt * 60
	} else if app.gg.pressed_keys[gg.KeyCode.up] {
		app.pitch += 0.01 * dt * 60
	}
	if !isnil(app.music) && gg.is_sound_playing(app.music) && !app.pause {
		app.time_played += dt * 0.02 * app.pitch
		if app.time_played > 1 {
			app.time_played = 0
			gg.play_sound(app.music)
		}
	}
	if app.pause {
		return
	}
	for i in 0 .. app.circles.len {
		mut c := &app.circles[i]
		c.alpha += c.speed
		c.radius += c.speed * 10
		if c.alpha > 1 {
			c.speed *= -1
		}
		if c.alpha <= 0 {
			c.alpha = 0
			c.radius = f32(rand.intn(31) or { 10 } + 10)
			c.speed = f32(rand.intn(100) or { 50 }) / 2000.0
		}
	}
}

fn frame(data voidptr) {
	app := unsafe { &App(data) }
	ctx := app.gg
	ctx.begin()
	for c in app.circles {
		color := gg.Color{c.color.r, c.color.g, c.color.b, u8(c.alpha * 255)}
		ctx.draw_circle_filled(c.x, c.y, c.radius, color)
	}
	bar_w := win_width - 40
	ctx.draw_rect_filled(20, f32(win_height) - 32, bar_w, 12, gg.light_gray)
	ctx.draw_rect_filled(20, f32(win_height) - 32, f32(int(app.time_played * bar_w)), 12, gg.dark_red)
	ctx.draw_rect_empty(20, f32(win_height) - 32, bar_w, 12, gg.gray)

	ctx.draw_rect_filled(20, 20, 425, 145, gg.white)
	ctx.draw_rect_empty(20, 20, 425, 145, gg.gray)
	ctx.draw_text(40, 40, 'PRESS SPACE TO RESTART MUSIC', size: 20, color: gg.black)
	ctx.draw_text(40, 70, 'PRESS P TO PAUSE/RESUME', size: 20, color: gg.black)
	ctx.draw_text(40, 100, 'PRESS UP/DOWN TO CHANGE SPEED', size: 20, color: gg.black)
	ctx.draw_text(40, 130, 'SPEED: ${app.pitch:.2f}', size: 20, color: gg.dark_red)
	ctx.end()
}

fn main() {
	mut app := &App{}
	app.gg = gg.new_context(
		width:         win_width
		height:        win_height
		window_title:  'raylib [audio] example - module playing'
		bg_color:      gg.white
		user_data:     app
		init_fn:       init
		update_fn:     update
		event_fn:      event
		frame_fn:      frame
		create_window: true
	)
	app.gg.run()
}
