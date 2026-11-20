// Port of raylib [audio] example - stream effects.
// PORT NOTE: gg has no music stream/stream processors and no MP3 decoder. The LPF and delay
// effects are applied inside a sokol.audio callback to a synthesized source tone.
@[has_globals]
module main

import gg
import math

const win_width = 800
const win_height = 450
const sample_rate = 44100
const delay_buffer_len = sample_rate

__global (
	enable_lpf   bool
	enable_delay bool
	low          = [f32(0), f32(0)]
	delay_buffer = []f32{len: delay_buffer_len, init: 0}
	delay_read   = 2
	delay_write  = 0
	effect_time  f64
	time_played  f32
	app_ref      &App = unsafe { nil }
)

fn effects_callback(buffer &f32, num_frames i32, num_channels i32) {
	frames := int(num_frames)
	channels := int(num_channels)
	if frames <= 0 || channels <= 0 {
		return
	}
	cutoff := 70.0 / f64(sample_rate)
	k := f32(cutoff / (cutoff + 0.1591549431))
	unsafe {
		for frame in 0 .. frames {
			for ch in 0 .. channels {
				at := frame * channels + ch
				mut s := f32(math.sin(2.0 * math.pi * 330.0 * effect_time) * 0.5)
				effect_time += 1.0 / f64(sample_rate)
				if enable_lpf && ch < 2 {
					low[ch] += k * (s - low[ch])
					s = low[ch]
				}
				if enable_delay {
					left_delayed := delay_buffer[delay_read % delay_buffer_len]
					delay_read++
					right_delayed := delay_buffer[delay_read % delay_buffer_len]
					delay_read++
					s = 0.5 * s + 0.5 * if ch == 0 { left_delayed } else { right_delayed }
					delay_buffer[delay_write % delay_buffer_len] = s
					delay_write++
				}
				buffer[at] = s
			}
		}
	}
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
		.space { time_played = 0 }
		.p { /* pause is visual only in this approximation */
		}
		.f { enable_lpf = !enable_lpf }
		.d { enable_delay = !enable_delay }
		else {}
	}
}

fn update(dt f32, data voidptr) {
	_ = data
	time_played += dt * 0.02
	if time_played > 1 {
		time_played = 1
	}
}

fn frame(data voidptr) {
	_ = data
	ctx := app_ref.gg
	ctx.begin()
	ctx.draw_text(245, 150, 'MUSIC SHOULD BE PLAYING!', size: 20, color: gg.light_gray)
	ctx.draw_rect_filled(200, 180, 400, 12, gg.light_gray)
	ctx.draw_rect_filled(200, 180, int(time_played * 400), 12, gg.dark_red)
	ctx.draw_rect_empty(200, 180, 400, 12, gg.gray)
	ctx.draw_text(215, 230, 'PRESS SPACE TO RESTART MUSIC', size: 20, color: gg.light_gray)
	ctx.draw_text(208, 260, 'PRESS P TO PAUSE/RESUME MUSIC', size: 20, color: gg.light_gray)
	lpf := if enable_lpf { 'ON' } else { 'OFF' }
	dly := if enable_delay { 'ON' } else { 'OFF' }
	ctx.draw_text(200, 320, 'PRESS F TO TOGGLE LPF EFFECT: ${lpf}', size: 20, color: gg.gray)
	ctx.draw_text(180, 350, 'PRESS D TO TOGGLE DELAY EFFECT: ${dly}', size: 20, color: gg.gray)
	ctx.end()
}

fn main() {
	mut app := &App{}
	app_ref = app
	gg.set_audio_callback(effects_callback, sample_rate: sample_rate, channels: 1)
	app.gg = gg.new_context(
		width:         win_width
		height:        win_height
		window_title:  'raylib [audio] example - stream effects'
		bg_color:      gg.white
		user_data:     app
		update_fn:     update
		event_fn:      event
		frame_fn:      frame
		create_window: true
	)
	app.gg.run()
}
