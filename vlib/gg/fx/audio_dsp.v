// Copyright (c) 2019-2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module fx

import math

fn dsp_clamp_i16(v f32) i16 {
	if v > 32767 {
		return 32767
	}
	if v < -32768 {
		return -32768
	}
	return i16(v)
}

// dsp_gain scales every sample by gain and clamps to the i16 range.
pub fn dsp_gain(samples []i16, gain f32) []i16 {
	mut out := []i16{len: samples.len}
	for i, s in samples {
		out[i] = dsp_clamp_i16(f32(s) * gain)
	}
	return out
}

// dsp_fade applies linear fade-in and fade-out ramps measured in samples.
pub fn dsp_fade(samples []i16, fade_in int, fade_out int) []i16 {
	mut out := []i16{len: samples.len}
	for i, s in samples {
		mut factor := f32(1)
		if fade_in > 0 && i < fade_in {
			factor = f32(i) / f32(fade_in)
		}
		if fade_out > 0 && i >= samples.len - fade_out {
			remaining := samples.len - 1 - i
			factor = math.min(factor, f32(remaining) / f32(fade_out))
		}
		out[i] = dsp_clamp_i16(f32(s) * factor)
	}
	return out
}

// dsp_normalize scales samples so the peak absolute value reaches target,
// which is normally in [0, 1]. Silent input is returned unchanged.
pub fn dsp_normalize(samples []i16, target f32) []i16 {
	mut peak := f32(0)
	for s in samples {
		v := if s < 0 { f32(-s) } else { f32(s) }
		if v > peak {
			peak = v
		}
	}
	if peak == 0 {
		return samples.clone()
	}
	return dsp_gain(samples, target * 32767 / peak)
}

// dsp_generate_sine creates count mono samples of a sine wave at frequency.
pub fn dsp_generate_sine(frequency f32, count int, sample_rate int) []i16 {
	mut out := []i16{len: count}
	if sample_rate <= 0 {
		return out
	}
	for i in 0 .. count {
		phase := 2 * math.pi * f64(frequency) * f64(i) / f64(sample_rate)
		out[i] = dsp_clamp_i16(f32(math.sin(phase) * 0.8 * 32767))
	}
	return out
}

// dsp_lowpass applies a one-pole low-pass filter to a float signal. alpha is
// the smoothing factor in (0, 1]; smaller values cut more highs.
pub fn dsp_lowpass(samples []f32, alpha f32) []f32 {
	mut out := []f32{len: samples.len}
	if samples.len == 0 {
		return out
	}
	a := math.clip(alpha, 0.0, 1.0)
	out[0] = samples[0]
	for i in 1 .. samples.len {
		out[i] = out[i - 1] + a * (samples[i] - out[i - 1])
	}
	return out
}

// dsp_mix_into adds src into dst scaled by gain, clamping each output sample.
// dst is normally an interleaved float output buffer.
pub fn dsp_mix_into(mut dst []f32, src []i16, gain f32) {
	n := math.min(dst.len, src.len)
	for i in 0 .. n {
		v := dst[i] + f32(src[i]) / 32768.0 * gain
		dst[i] = math.clip(v, -1.0, 1.0)
	}
}

// dsp_to_stereo converts mono samples to interleaved stereo. pan ranges from
// -1 (left) to 1 (right); zero balances both channels.
pub fn dsp_to_stereo(samples []i16, pan f32) []i16 {
	p := math.clip(pan, -1.0, 1.0)
	left_gain := f32(math.sqrt(f64((1 - p) / 2))) * 1.41421356
	right_gain := f32(math.sqrt(f64((1 + p) / 2))) * 1.41421356
	mut out := []i16{len: samples.len * 2}
	for i, s in samples {
		out[i * 2] = dsp_clamp_i16(f32(s) * left_gain)
		out[i * 2 + 1] = dsp_clamp_i16(f32(s) * right_gain)
	}
	return out
}
