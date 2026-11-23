// Port of raylib [audio] example - stream callback.
// PORT NOTE: gg has no AudioStreamCallback; a sokol.audio streaming callback
// (gg.set_audio_callback) synthesizes sine/square/triangle/sawtooth waves instead.
@[has_globals]
module main

import gg
import math

const win_width = 800
const win_height = 450
const sample_rate = 44100

enum WaveType {
	sine
	square
	triangle
	sawtooth
}

__global (
	wave_frequency     = 440
	new_wave_frequency = 440
	wave_index         = 0
	wave_type          = WaveType.sine
	wave_history       = []f32{len: sample_rate, init: 0}
)

fn wave_value() f32 {
	wavelength := if wave_frequency > 0 { sample_rate / wave_frequency } else { sample_rate }
	if wavelength <= 0 {
		return 0
	}
	phase := wave_index % wavelength
	value := match wave_type {
		.sine { f32(math.sin(2.0 * math.pi * f64(phase) / f64(wavelength))) }
		.square {
			if phase < wavelength / 2 { f32(1) } else { f32(-1) }
		}
		.triangle {
			if phase < wavelength / 2 {
				f32(-1 + 2.0 * f64(phase) / f64(wavelength / 2))
			} else {
				f32(1 - 2.0 * f64(phase - wavelength / 2) / f64(wavelength / 2))
			}
		}
		.sawtooth { f32(-1 + 2.0 * f64(phase) / f64(wavelength)) }
	}
	wave_index++
	if wave_index >= wavelength {
		wave_frequency = new_wave_frequency
		wave_index = 0
	}
	return value
}

fn wave_callback(buffer &f32, num_frames i32, num_channels i32) {
	frames := int(num_frames)
	channels := int(num_channels)
	if frames <= 0 || channels <= 0 {
		return
	}
	unsafe {
		for i in 0 .. frames {
			v := wave_value()
			for ch in 0 .. channels {
				buffer[i * channels + ch] = v
			}
		}
	}
	if frames <= wave_history.len {
		// Unsafe is needed to index the raw audio buffer pointer supplied by sokol.
		unsafe {
			for i in 0 .. wave_history.len - frames {
				wave_history[i] = wave_history[i + frames]
			}
			for i in 0 .. frames {
				wave_history[wave_history.len - frames + i] = buffer[i]
			}
		}
	}
}

__global (
	app_ref &App = unsafe { nil }
)

struct App {
mut:
	gg &gg.Context = unsafe { nil }
}

fn update(dt f32, data voidptr) {
	_ = dt
	_ = data
	if !isnil(app_ref) && !isnil(app_ref.gg) {
		if app_ref.gg.pressed_keys[gg.KeyCode.up] {
			new_wave_frequency += 10
			if new_wave_frequency > 12500 {
				new_wave_frequency = 12500
			}
		}
		if app_ref.gg.pressed_keys[gg.KeyCode.down] {
			new_wave_frequency -= 10
			if new_wave_frequency < 20 {
				new_wave_frequency = 20
			}
		}
	}
}

fn event(e &gg.Event, data voidptr) {
	_ = data
	if e.typ != .key_down {
		return
	}
	match e.key_code {
		.left {
			wave_type = match wave_type {
				.sine { .sawtooth }
				.square { .sine }
				.triangle { .square }
				.sawtooth { .triangle }
			}
		}
		.right {
			wave_type = match wave_type {
				.sine { .square }
				.square { .triangle }
				.triangle { .sawtooth }
				.sawtooth { .sine }
			}
		}
		else {}
	}
}

fn frame(data voidptr) {
	_ = data
	ctx := app_ref.gg
	ctx.begin()
	ctx.draw_text(win_width - 220, 10, 'frequency: ${new_wave_frequency}', size: 20, color: gg.red)
	ctx.draw_text(win_width - 220, 30, 'wave type: ${wave_type}', size: 20, color: gg.red)
	ctx.draw_text(10, 10, 'Up/down to change frequency', size: 20, color: gg.dark_gray)
	ctx.draw_text(10, 30, 'Left/right to change wave type', size: 20, color: gg.dark_gray)

	start := sample_rate - sample_rate / 100
	window := sample_rate / 100
	for i in 0 .. win_width - 1 {
		i0 := start + i * window / win_width
		i1 := start + (i + 1) * window / win_width
		ctx.draw_line(f32(i), f32(250 - 50 * wave_history[i0]), f32(i + 1), f32(250 - 50 * wave_history[i1]),
			gg.red)
	}
	ctx.end()
}

fn main() {
	mut app := &App{}
	app_ref = app
	gg.set_audio_callback(wave_callback, sample_rate: sample_rate, channels: 1)
	app.gg = gg.new_context(
		width:         win_width
		height:        win_height
		window_title:  'raylib [audio] example - stream callback'
		bg_color:      gg.white
		user_data:     app
		update_fn:     update
		event_fn:      event
		frame_fn:      frame
		create_window: true
	)
	app.gg.run()
}
