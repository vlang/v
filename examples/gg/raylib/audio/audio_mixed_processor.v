// Port of raylib [audio] example - mixed processor.
// PORT NOTE: gg has no AttachAudioMixedProcessor and cannot decode MP3. A sokol.audio callback
// synthesizes a tone, the exponent distortion is applied in that callback, and the volume
// history is drawn directly instead of being sampled from a global mixed-output hook.
@[has_globals]
module main

import gg
import math

const win_width = 800
const win_height = 450
const sample_rate = 44100
const history_len = 400

__global (
	exponent       f32 = 1.0
	average_volume = []f32{len: history_len, init: 0}
	processor_time f64
	app_ref        &App = unsafe { nil }
)

fn processor_callback(buffer &f32, num_frames i32, num_channels i32) {
	frames := int(num_frames)
	channels := int(num_channels)
	if frames <= 0 || channels <= 0 {
		return
	}
	mut average := f32(0)
	unsafe {
		for frame in 0 .. frames {
			base := f32(math.sin(2.0 * math.pi * 220.0 * processor_time) * 0.6 +
				0.3 * math.sin(2.0 * math.pi * 440.0 * processor_time))
			processor_time += 1.0 / f64(sample_rate)
			for ch in 0 .. channels {
				at := frame * channels + ch
				mut sample := base
				magnitude := math.abs(f64(sample))
				sign := if sample < 0 { f32(-1) } else { f32(1) }
				sample = f32(math.pow(magnitude, f64(exponent))) * sign
				buffer[at] = sample
				average += math.abs(f64(sample)) / f64(frames) / f64(channels)
			}
		}
	}
	for i in 0 .. history_len - 1 {
		average_volume[i] = average_volume[i + 1]
	}
	average_volume[history_len - 1] = f32(average)
}

struct App {
mut:
	gg &gg.Context = unsafe { nil }
}

fn event(e &gg.Event, data voidptr) {
	_ = data
	if e.typ != .key_down {
		return
	}
	match e.key_code {
		.left { exponent -= 0.05 }
		.right { exponent += 0.05 }
		.space {
			if !isnil(app_ref) {
				// "other sound" trigger is visual only: the callback tone is always on.
			}
		}
		else {}
	}
	if exponent <= 0.5 {
		exponent = 0.5
	}
	if exponent >= 3.0 {
		exponent = 3.0
	}
}

fn frame(data voidptr) {
	_ = data
	ctx := app_ref.gg
	ctx.begin()
	ctx.draw_text(255, 150, 'MUSIC SHOULD BE PLAYING!', size: 20, color: gg.light_gray)
	ctx.draw_text(215, 180, 'EXPONENT = ${exponent:.2f}', size: 20, color: gg.light_gray)
	ctx.draw_rect_filled(199, 199, 402, 34, gg.light_gray)
	for i in 0 .. history_len {
		h := int(average_volume[i] * 32)
		ctx.draw_line(f32(201 + i), f32(232 - h), f32(201 + i), 232, gg.dark_red)
	}
	ctx.draw_rect_empty(199, 199, 402, 34, gg.gray)
	ctx.draw_text(200, 250, 'PRESS SPACE TO PLAY OTHER SOUND', size: 20, color: gg.light_gray)
	ctx.draw_text(140, 280, 'USE LEFT AND RIGHT ARROWS TO ALTER DISTORTION',
		size:  20
		color: gg.light_gray
	)
	ctx.end()
}

fn main() {
	mut app := &App{}
	app_ref = app
	gg.set_audio_callback(processor_callback, sample_rate: sample_rate, channels: 1)
	app.gg = gg.new_context(
		width:         win_width
		height:        win_height
		window_title:  'raylib [audio] example - mixed processor'
		bg_color:      gg.white
		user_data:     app
		event_fn:      event
		frame_fn:      frame
		create_window: true
	)
	app.gg.run()
}
