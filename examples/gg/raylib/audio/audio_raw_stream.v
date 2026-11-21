// Port of raylib [audio] example - raw stream.
// PORT NOTE: gg has no AudioStream/pan API. A sokol.audio streaming callback (via
// gg.set_audio_callback) generates the sine wave directly; pan is tracked for the UI only.
@[has_globals]
module main

import gg
import math

const win_width = 800
const win_height = 450
const buffer_size = 4096
const sample_rate = 44100

__global (
	sine_frequency     = 440
	new_sine_frequency = 440
	sine_index         = 0
	pan                = 0.0
	last_samples       = []f32{len: sample_rate, init: 0}
	app_ref            &App = unsafe { nil }
)

struct App {
mut:
	gg &gg.Context = unsafe { nil }
}

// sine_callback fills the output buffer with a sine of the current frequency and
// keeps the last second of samples for the on-screen waveform.
fn sine_callback(buffer &f32, num_frames i32, num_channels i32) {
	frames := int(num_frames)
	channels := int(num_channels)
	if frames <= 0 || channels <= 0 {
		return
	}
	unsafe {
		for i in 0 .. frames {
			mut value := f32(0)
			if sine_frequency > 0 {
				wavelength := sample_rate / sine_frequency
				if wavelength > 0 {
					value = f32(math.sin(2.0 * math.pi * f64(sine_index % wavelength) / f64(wavelength)))
				}
				sine_index++
				if sine_index >= wavelength {
					sine_frequency = new_sine_frequency
					sine_index = 0
				}
			}
			for ch in 0 .. channels {
				buffer[i * channels + ch] = value
			}
		}
	}
	// Shift the visualization history and append the fresh mono samples.
	// Unsafe is needed to index the raw audio buffer pointer supplied by sokol.
	if channels == 1 && frames <= last_samples.len {
		unsafe {
			for i in 0 .. last_samples.len - frames {
				last_samples[i] = last_samples[i + frames]
			}
			for i in 0 .. frames {
				last_samples[last_samples.len - frames + i] = buffer[i]
			}
		}
	}
}

fn update(dt f32, data voidptr) {
	_ = dt
	_ = data
	if app_left() {
		new_sine_frequency -= 10
		if new_sine_frequency < 20 {
			new_sine_frequency = 20
		}
	}
	if app_right() {
		new_sine_frequency += 10
		if new_sine_frequency > 12500 {
			new_sine_frequency = 12500
		}
	}
}

// Input is read in the event callback; these read the pressed-key array via the
// one App instance stored in a global (set from main).
fn app_left() bool {
	return !isnil(app_ref) && !isnil(app_ref.gg) && app_ref.gg.pressed_keys[gg.KeyCode.left]
}

fn app_right() bool {
	return !isnil(app_ref) && !isnil(app_ref.gg) && app_ref.gg.pressed_keys[gg.KeyCode.right]
}

fn event(e &gg.Event, data voidptr) {
	_ = data
	if e.typ != .key_down {
		return
	}
	match e.key_code {
		.left {
			pan -= 0.01
			if pan < -1 {
				pan = -1
			}
		}
		.right {
			pan += 0.01
			if pan > 1 {
				pan = 1
			}
		}
		else {}
	}
}

fn frame(data voidptr) {
	_ = data
	ctx := app_ref.gg
	ctx.begin()
	ctx.draw_text(win_width - 220, 10, 'sine frequency: ${sine_frequency}', size: 20, color: gg.red)
	ctx.draw_text(win_width - 220, 30, 'pan: ${pan:.2f} (visual only)', size: 20, color: gg.red)
	ctx.draw_text(10, 10, 'Up/down to change frequency', size: 20, color: gg.dark_gray)
	ctx.draw_text(10, 30, 'Left/right to pan (visual only)', size: 20, color: gg.dark_gray)

	window_size := sample_rate / 10
	wavelength := if sine_frequency > 0 { sample_rate / sine_frequency } else { sample_rate }
	for i in 0 .. win_width - 1 {
		t0 := i * window_size / win_width
		t1 := (i + 1) * window_size / win_width
		y0 := 250 + 50 * math.sin(2.0 * math.pi * f64(t0 % wavelength) / f64(wavelength))
		y1 := 250 + 50 * math.sin(2.0 * math.pi * f64(t1 % wavelength) / f64(wavelength))
		ctx.draw_line(f32(i), f32(y0), f32(i + 1), f32(y1), gg.red)
	}
	ctx.end()
}

fn main() {
	mut app := &App{}
	app_ref = app
	gg.set_audio_callback(sine_callback, sample_rate: sample_rate, channels: 1)
	app.gg = gg.new_context(
		width:         win_width
		height:        win_height
		window_title:  'raylib [audio] example - raw stream'
		bg_color:      gg.white
		user_data:     app
		update_fn:     update
		event_fn:      event
		frame_fn:      frame
		create_window: true
	)
	app.gg.run()
}
