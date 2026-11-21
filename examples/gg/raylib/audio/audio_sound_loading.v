// Port of raylib [audio] example - sound loading (raylib shaders/audio example port).
// PORT NOTE: gg.load_wav decodes 16-bit PCM WAV only; the OGG file is approximated with a WAV.
module main

import gg

const win_width = 800
const win_height = 450

struct App {
mut:
	gg  &gg.Context = unsafe { nil }
	wav &gg.Sound   = unsafe { nil }
	ogg &gg.Sound   = unsafe { nil }
}

fn init(data voidptr) {
	mut app := unsafe { &App(data) }
	// Install the streaming callback that mixes all active gg.Sound values.
	gg.setup_sound_audio(sample_rate: 44100, channels: 1)

	// Load WAV audio file. The Sound must stay alive while it is playing, so we
	// keep a stable heap pointer to it in the App struct.
	wav := gg.load_wav('resources/sound.wav') or {
		eprintln('sound.wav: ${err}')
		return
	}
	app.wav = &wav

	// The original loads target.ogg; gg cannot decode OGG, so use a WAV instead.
	ogg := gg.load_wav('resources/target.wav') or {
		eprintln('target.wav (OGG stand-in): ${err}')
		return
	}
	app.ogg = &ogg
}

fn event(e &gg.Event, data voidptr) {
	if e.typ != .key_down {
		return
	}
	app := unsafe { &App(data) }
	match e.key_code {
		.space {
			if !isnil(app.wav) {
				gg.play_sound(app.wav)
			}
		}
		.enter {
			if !isnil(app.ogg) {
				gg.play_sound(app.ogg)
			}
		}
		else {}
	}
}

fn frame(data voidptr) {
	app := unsafe { &App(data) }
	ctx := app.gg
	ctx.begin()
	ctx.draw_text(200, 180, 'Press SPACE to PLAY the WAV sound!', size: 20, color: gg.light_gray)
	ctx.draw_text(200, 220, 'Press ENTER to PLAY the OGG sound (WAV stand-in)!',
		size:  20
		color: gg.light_gray
	)
	ctx.end()
}

fn main() {
	mut app := &App{}
	app.gg = gg.new_context(
		width:         win_width
		height:        win_height
		window_title:  'raylib [audio] example - sound loading'
		bg_color:      gg.white
		user_data:     app
		init_fn:       init
		event_fn:      event
		frame_fn:      frame
		create_window: true
	)
	app.gg.run()
}
