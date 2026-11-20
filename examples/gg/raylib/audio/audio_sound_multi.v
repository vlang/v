// Port of raylib [audio] example - sound multi.
// PORT NOTE: raylib LoadSoundAlias shares one buffer; gg has no aliases, so each slot is an
// independent WAV load (or a repeated play of the same Sound when the file is missing).
module main

import gg

const win_width = 800
const win_height = 450
const max_sounds = 10

struct App {
mut:
	gg            &gg.Context = unsafe { nil }
	sounds        []&gg.Sound
	current_sound int
}

fn init(data voidptr) {
	mut app := unsafe { &App(data) }
	gg.setup_sound_audio(sample_rate: 44100, channels: 1)
	first := gg.load_wav('resources/sound.wav') or {
		eprintln('sound.wav: ${err}')
		return
	}
	app.sounds << &first
	// Slots 1..9 are "aliases" in raylib. gg cannot share sample data, so play the
	// first sound again for those slots; the active-sound map keys on Sound address.
	for _ in 1 .. max_sounds {
		app.sounds << app.sounds[0]
	}
}

fn event(e &gg.Event, data voidptr) {
	if e.typ != .key_down {
		return
	}
	mut app := unsafe { &App(data) }
	if e.key_code == .space && app.sounds.len > 0 {
		gg.play_sound(app.sounds[app.current_sound])
		app.current_sound++
		if app.current_sound >= max_sounds {
			app.current_sound = 0
		}
	}
}

fn frame(data voidptr) {
	app := unsafe { &App(data) }
	ctx := app.gg
	ctx.begin()
	ctx.draw_text(200, 180, 'Press SPACE to PLAY a WAV sound!', size: 20, color: gg.light_gray)
	ctx.draw_text(200, 220, 'Next slot: ${app.current_sound + 1}/${max_sounds}',
		size:  16
		color: gg.gray
	)
	ctx.end()
}

fn main() {
	mut app := &App{}
	app.gg = gg.new_context(
		width:         win_width
		height:        win_height
		window_title:  'raylib [audio] example - sound multi'
		bg_color:      gg.white
		user_data:     app
		init_fn:       init
		event_fn:      event
		frame_fn:      frame
		create_window: true
	)
	app.gg.run()
}
