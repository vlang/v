// Port of raylib [audio] example - music stream.
// PORT NOTE: gg decodes only 16-bit PCM WAV and has no streaming Music object, no pan, no
// volume and no time-length API. A looping Sound stands in for the MP3 music stream; the
// pan/volume/progress UI is driven by local state instead of the audio device.
module main

import gg

const win_width = 800
const win_height = 450

struct App {
mut:
	gg          &gg.Context = unsafe { nil }
	music       &gg.Sound   = unsafe { nil }
	time_played f32
	pause       bool
	pan         f32
	volume      f32 = 0.8
}

fn init(data voidptr) {
	mut app := unsafe { &App(data) }
	gg.setup_sound_audio(sample_rate: 44100, channels: 1)
	// A WAV loop approximates the streaming MP3 music source.
	s := gg.load_wav('resources/country.wav') or {
		eprintln('country.wav: ${err}')
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
	if app.gg.pressed_keys[gg.KeyCode.left] {
		app.pan -= 0.05 * dt * 20
		if app.pan < -1 {
			app.pan = -1
		}
	} else if app.gg.pressed_keys[gg.KeyCode.right] {
		app.pan += 0.05 * dt * 20
		if app.pan > 1 {
			app.pan = 1
		}
	}
	if app.gg.pressed_keys[gg.KeyCode.down] {
		app.volume -= 0.05 * dt * 20
		if app.volume < 0 {
			app.volume = 0
		}
	} else if app.gg.pressed_keys[gg.KeyCode.up] {
		app.volume += 0.05 * dt * 20
		if app.volume > 1 {
			app.volume = 1
		}
	}
	// Without a music-length API, approximate progress from the active play state.
	if !isnil(app.music) && gg.is_sound_playing(app.music) && !app.pause {
		app.time_played += dt * 0.01
		if app.time_played > 1 {
			app.time_played = 0
			gg.play_sound(app.music)
		}
	}
}

fn frame(data voidptr) {
	app := unsafe { &App(data) }
	ctx := app.gg
	ctx.begin()
	col := gg.light_gray
	ctx.draw_text(255, 150, 'MUSIC SHOULD BE PLAYING!', size: 20, color: col)

	ctx.draw_text(320, 74, 'LEFT-RIGHT for PAN CONTROL', size: 10, color: gg.dark_blue)
	ctx.draw_rect_filled(300, 100, 200, 12, gg.light_gray)
	ctx.draw_rect_empty(300, 100, 200, 12, gg.gray)
	ctx.draw_rect_filled(int(300 + (app.pan + 1) / 2 * 200 - 5), 92, 10, 28, gg.dark_gray)

	ctx.draw_rect_filled(200, 200, 400, 12, gg.light_gray)
	ctx.draw_rect_filled(200, 200, int(app.time_played * 400), 12, gg.dark_red)
	ctx.draw_rect_empty(200, 200, 400, 12, gg.gray)

	ctx.draw_text(215, 250, 'PRESS SPACE TO RESTART MUSIC', size: 20, color: col)
	ctx.draw_text(208, 280, 'PRESS P TO PAUSE/RESUME MUSIC', size: 20, color: col)

	ctx.draw_text(320, 334, 'UP-DOWN for VOLUME CONTROL', size: 10, color: gg.dark_green)
	ctx.draw_rect_filled(300, 360, 200, 12, gg.light_gray)
	ctx.draw_rect_empty(300, 360, 200, 12, gg.gray)
	ctx.draw_rect_filled(int(300 + app.volume * 200 - 5), 352, 10, 28, gg.dark_gray)
	ctx.end()
}

fn main() {
	mut app := &App{}
	app.gg = gg.new_context(
		width:         win_width
		height:        win_height
		window_title:  'raylib [audio] example - music stream'
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
