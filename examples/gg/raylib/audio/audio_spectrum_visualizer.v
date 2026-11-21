// Port of raylib [audio] example - spectrum visualizer.
// PORT NOTE: gg has no user-shader support, so the fft.fs ShaderToy-style full-screen effect is
// approximated with an Image of per-bin amplitudes drawn as 2D bars with gg primitives. The
// MP3 source is replaced by a synthesized tone captured through a sokol.audio callback.
@[has_globals]
module main

import gg
import math

const win_width = 800
const win_height = 450
const sample_rate = 44100
const fft_window_size = 1024
const bin_count = 512
const min_decibels = f64(-100)
const max_decibels = f64(-30)
const smoothing = f32(0.8)

struct Complex {
mut:
	real f64
	imag f64
}

__global (
	capture_buffer  = []f32{len: fft_window_size, init: 0}
	capture_pos     = 0
	spectrum        = []f64{len: fft_window_size, init: 0}
	prev_magnitudes = []f32{len: bin_count, init: 0}
	amplitudes      = []f32{len: bin_count, init: 0}
	visual_time     f64
	app_ref         &App = unsafe { nil }
)

fn capture_callback(buffer &f32, num_frames i32, num_channels i32) {
	frames := int(num_frames)
	channels := int(num_channels)
	if frames <= 0 || channels <= 0 {
		return
	}
	unsafe {
		for i in 0 .. frames {
			s := f32(math.sin(2.0 * math.pi * 220.0 * visual_time) * 0.4 +
				0.2 * math.sin(2.0 * math.pi * 880.0 * visual_time))
			visual_time += 1.0 / f64(sample_rate)
			for ch in 0 .. channels {
				buffer[i * channels + ch] = s
			}
			capture_buffer[capture_pos % fft_window_size] = buffer[i * channels]
			capture_pos++
		}
	}
}

// fft performs an in-place iterative Cooley-Tukey radix-2 FFT.
fn fft(mut a []Complex) {
	n := a.len
	// Bit-reversal permutation.
	mut j := 0
	for i := 1; i < n - 1; i++ {
		mut bit := n >> 1
		for j >= bit {
			j -= bit
			bit >>= 1
		}
		j += bit
		if i < j {
			tmp := a[i]
			a[i] = a[j]
			a[j] = tmp
		}
	}
	mut len := 2
	for len <= n {
		angle := -2.0 * math.pi / f64(len)
		unit := Complex{math.cos(angle), math.sin(angle)}
		mut i := 0
		for i < n {
			mut twiddle := Complex{1.0, 0.0}
			half := len / 2
			for k := 0; k < half; k++ {
				even := a[i + k]
				odd := a[i + k + half]
				t_real := odd.real * twiddle.real - odd.imag * twiddle.imag
				t_imag := odd.real * twiddle.imag + odd.imag * twiddle.real
				a[i + k] = Complex{even.real + t_real, even.imag + t_imag}
				a[i + k + half] = Complex{even.real - t_real, even.imag - t_imag}
				next_real := twiddle.real * unit.real - twiddle.imag * unit.imag
				twiddle.imag = twiddle.real * unit.imag + twiddle.imag * unit.real
				twiddle.real = next_real
			}
			i += len
		}
		len <<= 1
	}
}

fn capture_frame() {
	mut work := []Complex{len: fft_window_size, init: Complex{0, 0}}
	for i in 0 .. fft_window_size {
		x := 2.0 * math.pi * f64(i) / f64(fft_window_size - 1)
		blackman := 0.42 - 0.5 * math.cos(x) + 0.08 * math.cos(2.0 * x)
		sample := f64(capture_buffer[(capture_pos + i) % fft_window_size])
		work[i] = Complex{sample * blackman, 0}
	}
	fft(mut work)
	for i in 0 .. fft_window_size {
		spectrum[i] = work[i].real
	}
	inverse_range := 1.0 / (max_decibels - min_decibels)
	db_scale := 20.0 / 2.302585092994046
	for bin in 0 .. bin_count {
		magnitude := math.sqrt(spectrum[bin] * spectrum[bin]) / f64(fft_window_size)
		smoothed := smoothing * prev_magnitudes[bin] + (1 - smoothing) * f32(magnitude)
		prev_magnitudes[bin] = smoothed
		value := math.log(math.max(f64(smoothed), 1e-40)) * db_scale
		mut normalized := f32((value - min_decibels) * inverse_range)
		if normalized < 0 {
			normalized = 0
		} else if normalized > 1 {
			normalized = 1
		}
		amplitudes[bin] = normalized
	}
}

struct App {
mut:
	gg &gg.Context = unsafe { nil }
}

fn frame(data voidptr) {
	_ = data
	capture_frame()
	ctx := app_ref.gg
	ctx.begin()
	bar_w := f32(win_width) / f32(bin_count)
	for i in 0 .. bin_count {
		h := amplitudes[i] * (win_height - 40)
		ctx.draw_rect_filled(f32(i) * bar_w, f32(win_height) - h, bar_w, h,
			gg.Color{u8(40 + amplitudes[i] * 200), 80, u8(255 - amplitudes[i] * 100), 255})
	}
	ctx.draw_text(10, 10, 'Spectrum visualizer (CPU FFT approximation)', size: 16, color: gg.white)
	ctx.draw_text(10, 30, 'sg user-shader fft.fs effect not available in gg',
		size:  12
		color: gg.light_gray
	)
	ctx.end()
}

fn main() {
	mut app := &App{}
	app_ref = app
	gg.set_audio_callback(capture_callback, sample_rate: sample_rate, channels: 1)
	app.gg = gg.new_context(
		width:         win_width
		height:        win_height
		window_title:  'raylib [audio] example - spectrum visualizer'
		bg_color:      gg.black
		user_data:     app
		frame_fn:      frame
		create_window: true
	)
	app.gg.run()
}
