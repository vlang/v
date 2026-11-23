module audio

import math

// Bus is a gain/pan/lowpass stage for interleaved float stereo streams.
pub struct Bus {
pub mut:
	name   string
	gain   f32 = 1
	pan    f32
	cutoff f32 = 1
	state  f32
}

pub fn new_bus(name string) Bus {
	return Bus{ name: name }
}

pub fn (b &Bus) left_gain() f32 {
	return b.gain * f32(math.sqrt(f64((1 - math.clip(b.pan, -1, 1)) / 2))) * 1.41421356
}

pub fn (b &Bus) right_gain() f32 {
	return b.gain * f32(math.sqrt(f64((1 + math.clip(b.pan, -1, 1)) / 2))) * 1.41421356
}

// process applies the bus to interleaved stereo samples in place.
pub fn (mut b Bus) process(mut samples []f32) {
	lg := b.left_gain()
	rg := b.right_gain()
	a := math.clip(b.cutoff, 0, 1)
	for i in 0 .. samples.len / 2 {
		b.state += a * (samples[i * 2] * lg - b.state)
		samples[i * 2] = b.state
		b.state += a * (samples[i * 2 + 1] * rg - b.state)
		samples[i * 2 + 1] = b.state
	}
}

// Listener is the position and orientation used by positional voices.
pub struct Listener {
pub mut:
	position Vec3
	forward  Vec3 = Vec3{0, 0, -1}
	up       Vec3 = Vec3{0, 1, 0}
}

fn attenuation(distance f32, ref_dist f32, max_dist f32) f32 {
	if distance <= ref_dist {
		return 1
	}
	if distance >= max_dist {
		return 0
	}
	return ref_dist / distance
}

// PositionalVoice plays a buffer with distance attenuation and stereo pan.
pub struct PositionalVoice {
pub mut:
	buffer   &Buffer
	position Vec3
	volume   f32 = 1
	ref_dist f32 = 1
	max_dist f32 = 30
	playing  bool
	pos      int
}

pub fn new_positional_voice(buffer &Buffer, position Vec3) PositionalVoice {
	return unsafe { PositionalVoice{ buffer: buffer, position: position } }
}

// mix renders the next frames into an interleaved stereo buffer.
pub fn (mut v PositionalVoice) mix(mut dst []f32, frames int, listener &Listener) {
	if !v.playing || v.buffer.samples.len == 0 || v.buffer.channels < 1 {
		return
	}
	delta := v.position - listener.position
	distance := f32(math.sqrt(f64(delta.dot(delta))))
	atten := attenuation(distance, v.ref_dist, v.max_dist)
	right := listener.forward.cross(listener.up).normalized()
	pan := math.clip(delta.dot(right) / (distance + 0.001), -1, 1)
	lg := f32(math.sqrt(f64((1 - pan) / 2))) * 1.41421356 * v.volume * atten
	rg := f32(math.sqrt(f64((1 + pan) / 2))) * 1.41421356 * v.volume * atten
	mut frame := 0
	for frame < frames {
		mut ch := 0
		for ch < 2 {
			source := ch % v.buffer.channels
			idx := v.pos + frame * v.buffer.channels + source
			at := frame * 2 + ch
			if idx < v.buffer.samples.len && at + 1 < dst.len {
				g := if ch == 0 { lg } else { rg }
				dst[at] += f32(v.buffer.samples[idx]) / 32768.0 * g
			}
			ch++
		}
		frame++
	}
	v.pos += frames * v.buffer.channels
	if v.pos >= v.buffer.samples.len {
		v.playing = false
	}
}

// Stream plays a long sample buffer in chunks with optional looping.
pub struct Stream {
pub mut:
	samples []i16
	pos     int
	looping bool
	playing bool
}

pub fn new_stream(samples []i16, looping bool) Stream {
	return Stream{samples, 0, looping, false}
}

pub fn (mut s Stream) next(mut dst []i16, frames int) int {
	if !s.playing || s.samples.len == 0 {
		return 0
	}
	mut written := 0
	for written < frames && written < dst.len {
		if s.pos >= s.samples.len {
			if !s.looping {
				s.playing = false
				break
			}
			s.pos = 0
		}
		dst[written] = s.samples[s.pos]
		s.pos++
		written++
	}
	return written
}
