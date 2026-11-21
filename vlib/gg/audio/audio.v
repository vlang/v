// Copyright (c) 2019-2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module audio

import gg
import math

// Bus processes a float stereo stream. Buses can be chained for gain, pan
// and filtering without touching device APIs.
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

// process applies gain, pan and a one-pole low-pass to interleaved stereo.
pub fn (mut b Bus) process(mut samples []f32) {
	lg := b.left_gain()
	rg := b.right_gain()
	a := math.clip(b.cutoff, 0, 1)
	for i in 0 .. samples.len / 2 {
		left := samples[i * 2] * lg
		right := samples[i * 2 + 1] * rg
		b.state += a * (left - b.state)
		samples[i * 2] = b.state
		b.state += a * (right - b.state)
		samples[i * 2 + 1] = b.state
	}
}

// Listener is a position and orientation used by positional voices.
pub struct Listener {
pub mut:
	position gg.Vec3
	forward  gg.Vec3 = gg.Vec3{0, 0, -1}
	up       gg.Vec3 = gg.Vec3{0, 1, 0}
}

// PositionalVoice plays a gg.Sound with distance attenuation and panning.
pub struct PositionalVoice {
pub mut:
	sound    &gg.Sound
	position gg.Vec3
	volume   f32 = 1
	ref_dist f32 = 1
	max_dist f32 = 30
	playing  bool
	pos      int
}

pub fn new_positional_voice(sound &gg.Sound, position gg.Vec3) PositionalVoice {
	return unsafe { PositionalVoice{ sound: sound, position: position } }
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

// mix renders the next frames into an interleaved stereo buffer for listener.
pub fn (mut v PositionalVoice) mix(mut buffer []f32, frames int, listener &Listener) {
	if !v.playing || v.sound.samples.len == 0 || v.sound.channels < 1 {
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
			source := ch % v.sound.channels
			idx := v.pos + frame * v.sound.channels + source
			at := frame * 2 + ch
			if idx < v.sound.samples.len && at + 1 < buffer.len {
				gain := if ch == 0 { lg } else { rg }
				buffer[at] += f32(v.sound.samples[idx]) / 32768.0 * gain
			}
			ch++
		}
		frame++
	}
	v.pos += frames * v.sound.channels
	if v.pos >= v.sound.samples.len {
		v.playing = false
	}
}

// Stream plays a long sample buffer in chunks, optionally looping.
pub struct Stream {
pub mut:
	samples []i16
	pos     int
	looping bool
	playing bool
}

pub fn new_stream(samples []i16, looping bool) Stream {
	return Stream{ samples: samples, looping: looping }
}

// next fills dst with the next frames and returns how many were written.
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
