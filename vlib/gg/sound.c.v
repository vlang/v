@[has_globals]
module gg

import sync
import audio as std_audio

// Sound holds decoded 16-bit PCM samples ready for playback. Decoding is
// delegated to the shared vlib/audio module; gg keeps its public type.
pub struct Sound {
pub mut:
	samples     []i16
	channels    int = 1
	sample_rate int = 44100
	pos         int
}

__global sound_playing = map[voidptr]bool{}
__global sound_mutex = sync.Mutex{}

// play_sound starts (or restarts) playback of s.
pub fn play_sound(s &Sound) {
	mut sound := unsafe { &Sound(voidptr(s)) }
	key := voidptr(s)
	sound_mutex.lock()
	sound.pos = 0
	sound_playing[key] = true
	sound_mutex.unlock()
}

// stop_sound stops playback of s.
pub fn stop_sound(s &Sound) {
	key := voidptr(s)
	sound_mutex.lock()
	sound_playing[key] = false
	sound_mutex.unlock()
}

// is_sound_playing reports whether s is currently marked as active.
pub fn is_sound_playing(s &Sound) bool {
	key := voidptr(s)
	sound_mutex.lock()
	playing := sound_playing[key]
	sound_mutex.unlock()
	return playing
}

// sound_samples returns the raw signed 16-bit PCM samples of s.
pub fn sound_samples(s &Sound) []i16 {
	return s.samples
}

// mix_sound_frames is the sokol.audio streaming callback that mixes all
// active sounds into buffer.
pub fn mix_sound_frames(buffer &f32, num_frames i32, num_channels i32) {
	frames := int(num_frames)
	channels := int(num_channels)
	if frames <= 0 || channels <= 0 {
		return
	}
	unsafe {
		for i in 0 .. frames * channels {
			buffer[i] = 0
		}
	}
	mut finished := []voidptr{}
	sound_mutex.lock()
	for key, active in sound_playing {
		if !active {
			continue
		}
		mut sound := unsafe { &Sound(key) }
		if sound.samples.len == 0 || sound.channels < 1 {
			finished << key
			continue
		}
		unsafe {
			for frame in 0 .. frames {
				for ch in 0 .. channels {
					source := ch % sound.channels
					idx := sound.pos + frame * sound.channels + source
					if idx < sound.samples.len {
						at := frame * channels + ch
						buffer[at] += f32(sound.samples[idx]) / 32768.0
					}
				}
			}
		}
		sound.pos += frames * sound.channels
		if sound.pos >= sound.samples.len {
			finished << key
		}
	}
	for key in finished {
		sound_playing[key] = false
	}
	sound_mutex.unlock()
}

// SoundAudioConfig holds optional audio setup parameters.
@[params]
pub struct SoundAudioConfig {
pub mut:
	sample_rate int = 44100
	channels    int = 1
}
