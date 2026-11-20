// Port of raylib [audio] example - amp envelope (ADSR).
// PORT NOTE: gg has no AudioStream API and no raygui. A sokol.audio callback synthesizes the
// enveloped tone; the sliders are driven by mouse drags and drawn with gg primitives.
@[has_globals]
module main

import gg
import math

const win_width = 800
const win_height = 450
const sample_rate = 44100

enum ADSRState {
	idle
	attack
	decay
	sustain
	release
}

struct Envelope {
mut:
	attack_time   f32 = 1.0
	decay_time    f32 = 1.0
	sustain_level f32 = 0.5
	release_time  f32 = 1.0
	current_value f32
	state         ADSRState = .idle
}

__global (
	env        = Envelope{}
	audio_time f32
	space_down bool
	app_ref    &App = unsafe { nil }
)

fn update_envelope() {
	sample_time := 1.0 / f32(sample_rate)
	match env.state {
		.attack {
			env.current_value += (1.0 / env.attack_time) * sample_time
			if env.current_value >= 1 {
				env.current_value = 1
				env.state = .decay
			}
		}
		.decay {
			env.current_value -= ((1 - env.sustain_level) / env.decay_time) * sample_time
			if env.current_value <= env.sustain_level {
				env.current_value = env.sustain_level
				env.state = .sustain
			}
		}
		.sustain {
			env.current_value = env.sustain_level
		}
		.release {
			env.current_value -= (env.sustain_level / env.release_time) * sample_time
			if env.current_value <= 0.001 {
				env.current_value = 0
				env.state = .idle
			}
		}
		.idle {}
	}
}

fn envelope_callback(buffer &f32, num_frames i32, num_channels i32) {
	frames := int(num_frames)
	channels := int(num_channels)
	if frames <= 0 || channels <= 0 {
		return
	}
	unsafe {
		for i in 0 .. frames {
			if env.state != .idle || env.current_value > 0 {
				update_envelope()
			} else {
				env.current_value = 0
				audio_time = 0
			}
			value := env.current_value * f32(math.sin(2.0 * math.pi * 440.0 * f64(audio_time)))
			audio_time += 1.0 / f64(sample_rate)
			for ch in 0 .. channels {
				buffer[i * channels + ch] = value
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
	match e.typ {
		.key_down {
			if e.key_code == .space {
				space_down = true
				env.state = .attack
			}
		}
		.key_up {
			if e.key_code == .space {
				space_down = false
				if env.state != .idle {
					env.state = .release
				}
			}
		}
		.mouse_down {
			// Click/drag inside a slider to edit its value.
			x := e.mouse_x
			y := e.mouse_y
			if x >= 100 && x <= 500 {
				t := (x - 100) / 400
				if y >= 60 && y <= 90 {
					env.attack_time = 0.1 + t * 2.9
				} else if y >= 100 && y <= 130 {
					env.decay_time = 0.1 + t * 2.9
				} else if y >= 140 && y <= 170 {
					env.sustain_level = t
				} else if y >= 180 && y <= 210 {
					env.release_time = 0.1 + t * 2.9
				}
			}
		}
		else {}
	}
}

fn draw_slider(ctx &gg.Context, y int, label string, value f32, min f32, max f32, text string) {
	ctx.draw_rect_filled(100, y, 400, 30, gg.Color{220, 220, 220, 255})
	ctx.draw_rect_empty(100, y, 400, 30, gg.gray)
	frac := (value - min) / (max - min)
	ctx.draw_rect_filled(100, y, int(frac * 400), 30, gg.Color{150, 150, 200, 255})
	ctx.draw_text(105, y + 7, '${label}: ${text}', size: 14, color: gg.black)
}

fn draw_adsr(ctx &gg.Context) {
	bounds_x, bounds_y, bounds_w, bounds_h := 100, 250, 400, 100
	ctx.draw_rect_filled(bounds_x, bounds_y, bounds_w, bounds_h, gg.Color{211, 211, 211, 90})
	ctx.draw_rect_empty(bounds_x, bounds_y, bounds_w, bounds_h, gg.gray)
	sustain_width := 1.0
	total := env.attack_time + env.decay_time + sustain_width + env.release_time
	if total <= 0 {
		total = 1
	}
	sx := f32(bounds_w) / total
	sy := f32(bounds_h)
	start_x, start_y := f32(bounds_x), f32(bounds_y + bounds_h)
	peak_x, peak_y := start_x + env.attack_time * sx, f32(bounds_y)
	sus_x, sus_y := peak_x + env.decay_time * sx, f32(bounds_y) + (1 - env.sustain_level) * sy
	rel_x := sus_x + sustain_width * sx
	end_x := rel_x + env.release_time * sx
	ctx.draw_line(start_x, start_y, peak_x, peak_y, gg.light_blue)
	ctx.draw_line(peak_x, peak_y, sus_x, sus_y, gg.blue)
	ctx.draw_line(sus_x, sus_y, rel_x, sus_y, gg.dark_blue)
	ctx.draw_line(rel_x, sus_y, end_x, start_y, gg.orange)
	ctx.draw_text(bounds_x, bounds_y - 20, 'ADSR Visualizer', size: 10, color: gg.dark_gray)
}

fn frame(data voidptr) {
	_ = data
	ctx := app_ref.gg
	ctx.begin()
	draw_slider(ctx, 60, 'Attack (s)', env.attack_time, 0.1, 3.0, '${env.attack_time:.2f}s')
	draw_slider(ctx, 100, 'Decay (s)', env.decay_time, 0.1, 3.0, '${env.decay_time:.2f}s')
	draw_slider(ctx, 140, 'Sustain', env.sustain_level, 0.0, 1.0, '${env.sustain_level:.2f}')
	draw_slider(ctx, 180, 'Release (s)', env.release_time, 0.1, 3.0, '${env.release_time:.2f}s')
	draw_adsr(ctx)
	gy := 350 - env.current_value * 100
	ctx.draw_circle_filled(520, gy, 5, gg.dark_red)
	ctx.draw_text(535, gy - 5, 'Current Gain: ${env.current_value:.2f}',
		size:  10
		color: gg.dark_red
	)
	ctx.draw_text(200, 400, 'Hold SPACE to PLAY the sound!', size: 20, color: gg.light_gray)
	ctx.end()
}

fn main() {
	mut app := &App{}
	app_ref = app
	gg.set_audio_callback(envelope_callback, sample_rate: sample_rate, channels: 1)
	app.gg = gg.new_context(
		width:         win_width
		height:        win_height
		window_title:  'raylib [audio] example - amp envelope'
		bg_color:      gg.white
		user_data:     app
		event_fn:      event
		frame_fn:      frame
		create_window: true
	)
	app.gg.run()
}
