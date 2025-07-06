import time
import math
import term
import sokol.audio

struct Note {
mut:
	phase     f32
	freq      f32
	amplitude f32
}

struct App {
mut:
	notes shared []Note
}

fn main() {
	println('Press the keys (a, s, d, f, g, h, j, k) to play notes. Press ESC to quit.')
	mut app := &App{
		notes: []Note{len: 32}
	}
	audio.setup(
		stream_userdata_cb: audio_callback
		user_data:          app
	)
	defer { audio.shutdown() }
	term.enable_echo(false)
	defer { term.enable_echo(true) }
	for {
		key := term.key_pressed(blocking: false)
		app.handle_key(key) or { break }
		time.sleep(5 * time.millisecond)
	}
}

const c_2_pi = 2 * math.pi
const c_phase_step_per_freq = c_2_pi / 44100.0
const c_note_start_amplitude = 0.33
const c_note_decay = 0.3 / 44100.0

fn (mut app App) handle_key(key i64) ? {
	if key == -1 {
		return
	}
	mut freq := f32(0.0)
	match key {
		27 { return none }
		` ` { app.silence() }
		`a` { freq = 261.63 }
		`s` { freq = 293.66 }
		`d` { freq = 329.63 }
		`f` { freq = 349.23 }
		`g` { freq = 392.00 }
		`h` { freq = 440.00 }
		`j` { freq = 493.88 }
		`k` { freq = 523.25 }
		else {}
	}
	if freq > 0 {
		app.play(freq, c_note_start_amplitude)
	}
}

fn (mut app App) play(freq f32, volume f32) {
	lock app.notes {
		for mut note in app.notes {
			note.amplitude -= 0.2
		}
		for mut note in app.notes {
			if note.amplitude <= 0 {
				note = Note{
					phase:     0.0
					freq:      freq
					amplitude: volume
				}
				break
			}
		}
	}
}

fn (mut app App) silence() {
	lock app.notes {
		for mut note in app.notes {
			note.amplitude = 0
		}
	}
}

fn audio_callback(mut soundbuffer &f32, num_frames int, num_channels int, mut app App) {
	lock app.notes {
		for frame in 0 .. num_frames {
			mut sample := f32(0.0)
			for mut note in app.notes {
				if note.amplitude <= 0 {
					continue
				}
				sample += note.amplitude * math.sinf(note.phase)
				sample += note.amplitude * math.sinf(note.phase * 2.718)
				sample += note.amplitude * math.sinf(note.phase * 3.141)

				note.phase += note.freq * c_phase_step_per_freq
				for note.phase >= c_2_pi {
					note.phase -= c_2_pi
				}
				note.amplitude -= c_note_decay
			}
			for ch in 0 .. num_channels {
				soundbuffer[frame * num_channels + ch] = sample
			}
		}
	}
}
