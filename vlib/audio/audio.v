// Copyright (c) 2019-2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
// Module audio provides backend-independent PCM buffers, decoding, mixing and
// spatial audio helpers. Device output lives in device.v and its backends.
module audio

import math
import os

// Vec3 is a small position vector kept local so this module has no dependency
// on gg or any rendering library.
pub struct Vec3 {
pub mut:
	x f32
	y f32
	z f32
}

pub fn (a Vec3) + (b Vec3) Vec3 {
	return Vec3{a.x + b.x, a.y + b.y, a.z + b.z}
}

pub fn (a Vec3) - (b Vec3) Vec3 {
	return Vec3{a.x - b.x, a.y - b.y, a.z - b.z}
}

pub fn (a Vec3) scale(s f32) Vec3 {
	return Vec3{a.x * s, a.y * s, a.z * s}
}

pub fn (a Vec3) dot(b Vec3) f32 {
	return a.x * b.x + a.y * b.y + a.z * b.z
}

pub fn (a Vec3) cross(b Vec3) Vec3 {
	return Vec3{a.y * b.z - a.z * b.y, a.z * b.x - a.x * b.z, a.x * b.y - a.y * b.x}
}

pub fn (a Vec3) normalized() Vec3 {
	len := f32(math.sqrt(f64(a.dot(a))))
	return if len == 0 { Vec3{} } else { a.scale(1 / len) }
}

// Buffer holds interleaved signed 16-bit PCM samples. channels is 1 for mono
// or 2 for stereo; pos is the next frame an output device should consume.
pub struct Buffer {
pub mut:
	samples     []i16
	channels    int = 1
	sample_rate int = 44100
	pos         int
}

pub fn new_buffer(samples []i16, channels int, sample_rate int) !Buffer {
	if channels < 1 || sample_rate < 1 {
		return error('audio: invalid buffer format')
	}
	if samples.len % channels != 0 {
		return error('audio: sample count is not a multiple of channels')
	}
	return Buffer{samples, channels, sample_rate, 0}
}

fn u16_at(b []u8, at int) u16 {
	return u16(b[at]) | u16(b[at + 1]) << 8
}

fn u32_at(b []u8, at int) u32 {
	return u32(b[at]) | u32(b[at + 1]) << 8 | u32(b[at + 2]) << 16 | u32(b[at + 3]) << 24
}

// decode_wav parses a 16-bit PCM RIFF/WAVE file into a Buffer. Compressed or
// higher bit-depth formats are rejected with an error.
pub fn decode_wav(bytes []u8) !Buffer {
	if bytes.len < 12 || string(bytes[0..4].bytestr()) != 'RIFF' || string(bytes[8..12].bytestr()) != 'WAVE' {
		return error('audio: not a RIFF/WAVE file')
	}
	mut offset := 12
	mut channels := 0
	mut bits := 0
	mut rate := 44100
	mut data_at := -1
	mut data_len := 0
	for offset + 8 <= bytes.len {
		id := bytes[offset..offset + 4].bytestr()
		size := int(u32_at(bytes, offset + 4))
		if size < 0 || size > 0x7fffffff {
			return error('audio: chunk too large')
		}
		offset += 8
		if id == 'fmt ' {
			if size < 16 || offset + 16 > bytes.len {
				return error('audio: truncated fmt chunk')
			}
			format := int(u16_at(bytes, offset))
			channels = int(u16_at(bytes, offset + 2))
			rate = int(u32_at(bytes, offset + 4))
			bits = int(u16_at(bytes, offset + 14))
			if format != 1 || channels < 1 || channels > 2 || bits != 16 {
				return error('audio: only 16-bit mono/stereo PCM is supported')
			}
		} else if id == 'data' {
			if channels == 0 {
				return error('audio: data chunk precedes fmt chunk')
			}
			data_at = offset
			data_len = size
			break
		}
		next_offset := i64(offset) + i64(size) + i64(size % 2)
		if next_offset > i64(bytes.len) {
			return error('audio: chunk size out of range')
		}
		offset = int(next_offset)
	}
	if data_at < 0 || i64(data_at) + i64(data_len) > i64(bytes.len) {
		return error('audio: no valid data chunk')
	}
	frame_bytes := channels * 2
	if data_len % frame_bytes != 0 {
		return error('audio: partial sample frame')
	}
	mut samples := []i16{len: data_len / 2}
	for i in 0 .. samples.len {
		samples[i] = i16(u16_at(bytes, data_at + i * 2))
	}
	return Buffer{samples, channels, rate, 0}
}

// load_wav reads and decodes a WAV file from disk.
pub fn load_wav(path string) !Buffer {
	bytes := os.read_file(path) or { return error('audio: cannot read "${path}": ${err}') }
	return decode_wav(bytes.bytes())
}

// resample linearly interpolates a buffer to target_rate.
pub fn resample(buf &Buffer, target_rate int) !Buffer {
	if target_rate <= 0 || buf.sample_rate <= 0 || buf.channels < 1 {
		return error('audio: invalid resample parameters')
	}
	if target_rate == buf.sample_rate {
		return Buffer{buf.samples.clone(), buf.channels, buf.sample_rate, 0}
	}
	frames := buf.samples.len / buf.channels
	new_frames := int(i64(frames) * i64(target_rate) / i64(buf.sample_rate))
	if new_frames < 1 {
		return Buffer{[], buf.channels, target_rate, 0}
	}
	mut out := []i16{len: new_frames * buf.channels}
	ratio := f64(buf.sample_rate) / f64(target_rate)
	for i in 0 .. new_frames {
		src := f64(i) * ratio
		i0 := int(src)
		frac := f32(src - f64(i0))
		for c in 0 .. buf.channels {
			a := f32(buf.samples[i0 * buf.channels + c])
			next := if i0 + 1 < frames { f32(buf.samples[(i0 + 1) * buf.channels + c]) } else { a }
			out[i * buf.channels + c] = i16(a + (next - a) * frac)
		}
	}
	return Buffer{out, buf.channels, target_rate, 0}
}

// mix_into adds a buffer into a float interleaved destination with gain.
pub fn mix_into(mut dst []f32, buf &Buffer, gain f32) {
	n := if dst.len < buf.samples.len { dst.len } else { buf.samples.len }
	for i in 0 .. n {
		dst[i] += f32(buf.samples[i]) / 32768.0 * gain
	}
}

fn clamp_i16(v f32) i16 {
	return i16(math.clip(v, -32768, 32767))
}

// gain scales all samples and clamps to the i16 range.
pub fn gain(samples []i16, factor f32) []i16 {
	mut out := []i16{len: samples.len}
	for i, s in samples {
		out[i] = clamp_i16(f32(s) * factor)
	}
	return out
}

// generate_sine creates count mono samples of a sine wave.
pub fn generate_sine(frequency f32, count int, sample_rate int) []i16 {
	mut out := []i16{len: count}
	if sample_rate <= 0 {
		return out
	}
	for i in 0 .. count {
		phase := 2 * math.pi * f64(frequency) * f64(i) / f64(sample_rate)
		out[i] = clamp_i16(f32(math.sin(phase) * 0.8 * 32767))
	}
	return out
}
