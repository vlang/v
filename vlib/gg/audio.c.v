@[has_globals]
module gg

import sync
import audio as std_audio
import sokol.audio as sokol_audio

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

// load_wav decodes a 16-bit mono/stereo PCM WAV file via vlib/audio.
pub fn load_wav(path string) !Sound {
	buf := std_audio.load_wav(path) or { return error('${err}') }
	return Sound{
		samples:     buf.samples
		channels:    buf.channels
		sample_rate: buf.sample_rate
	}
}

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

// push_sounds mixes all active sounds and forwards them to sokol.audio.push.
pub fn push_sounds() int {
	frames := sokol_audio.expect()
	if frames <= 0 {
		return 0
	}
	mut channels := sokol_audio.channels()
	if channels <= 0 {
		channels = 1
	}
	mut buffer := []f32{len: frames * channels, init: 0}
	mix_sound_frames(buffer.data, frames, channels)
	return sokol_audio.push(buffer.data, frames)
}

// SoundAudioConfig holds optional audio setup parameters.
@[params]
pub struct SoundAudioConfig {
pub mut:
	sample_rate int = 44100
	channels    int = 1
}

// set_audio_callback installs cb on sokol.audio.
pub fn set_audio_callback(cb sokol_audio.FNStreamingCB, cfg SoundAudioConfig) {
	sokol_audio.setup(sample_rate: cfg.sample_rate, num_channels: cfg.channels, stream_cb: cb)
}

// setup_sound_audio installs mix_sound_frames as sokol's stream callback.
pub fn setup_sound_audio(cfg SoundAudioConfig) {
	set_audio_callback(mix_sound_frames, cfg)
}

// new_shared_audio_device returns a vlib/audio device backed by sokol. Use it
// when you want the shared audio API instead of gg's Sound helpers.
pub fn new_shared_audio_device(cfg std_audio.DeviceConfig) std_audio.Device {
	return std_audio.sokol_device(cfg)
}

// shared_audio_null_device returns a headless audio device for tests.
pub fn shared_audio_null_device() std_audio.Device {
	return std_audio.null_device()
}
