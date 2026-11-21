module fx

import gg
import math

// SoundVoice plays one gg.Sound with independent volume, pan and looping. Unlike
// play_sound, voices are mixed by mix_voices so several can play at once with
// their own settings. The referenced gg.Sound must stay alive while playing.
pub struct SoundVoice {
pub mut:
	sound   &gg.Sound
	volume  f32 = 1
	pan     f32
	looping bool
	playing bool
	pos     int
}

// new_voice returns a stopped voice for sound.
pub fn new_voice(sound &gg.Sound) &SoundVoice {
	return unsafe { &SoundVoice{ sound: sound } }
}

// play starts or restarts the voice from the first sample.
pub fn (mut v SoundVoice) play() {
	v.pos = 0
	v.playing = true
}

// stop stops the voice without resetting its position.
pub fn (mut v SoundVoice) stop() {
	v.playing = false
}

// set_volume sets linear gain, clamped to [0, 1].
pub fn (mut v SoundVoice) set_volume(value f32) {
	v.volume = math.clip(value, 0, 1)
}

// set_pan sets equal-power panning, clamped to [-1, 1].
pub fn (mut v SoundVoice) set_pan(value f32) {
	v.pan = math.clip(value, -1, 1)
}

// mix adds this voice's next frames into an interleaved float buffer.
pub fn (mut v SoundVoice) mix(mut buffer []f32, frames int, channels int) {
	if !v.playing || frames <= 0 || channels <= 0 || v.sound.samples.len == 0
		|| v.sound.channels < 1 {
		return
	}
	sound_channels := v.sound.channels
	total_frames := v.sound.samples.len / sound_channels
	if total_frames == 0 {
		return
	}
	left_gain := f32(math.sqrt(f64((1 - v.pan) / 2))) * 1.41421356 * v.volume
	right_gain := f32(math.sqrt(f64((1 + v.pan) / 2))) * 1.41421356 * v.volume
	start_frame := v.pos / sound_channels
	unsafe {
		for frame in 0 .. frames {
			mut fi := start_frame + frame
			if v.looping {
				fi = fi % total_frames
			} else if fi >= total_frames {
				continue
			}
			for ch in 0 .. channels {
				source := ch % sound_channels
				idx := fi * sound_channels + source
				if idx >= v.sound.samples.len {
					continue
				}
				gain := if ch % 2 == 0 { left_gain } else { right_gain }
				at := frame * channels + ch
				if at < buffer.len {
					buffer[at] += f32(v.sound.samples[idx]) / 32768.0 * gain
				}
			}
		}
	}
	v.pos += frames * sound_channels
	if v.pos >= v.sound.samples.len {
		if v.looping {
			// Stay on a frame boundary so the next mix starts at a whole frame.
			v.pos = (v.pos % v.sound.samples.len) / sound_channels * sound_channels
		} else {
			v.playing = false
		}
	}
}

// mix_voices mixes every voice into buffer, in order.
pub fn mix_voices(mut buffer []f32, frames int, channels int, voices []&SoundVoice) {
	for v in voices {
		v.mix(mut buffer, frames, channels)
	}
}
