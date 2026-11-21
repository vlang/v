@[has_globals]
module gg

import os
import sync
import sokol.audio

// A small high-level audio wrapper around sokol.audio. It only handles raw
// signed 16-bit PCM samples loaded from classic RIFF/WAVE files; there is no
// decoding of compressed formats here.

// sound_playing records which Sound pointers currently have active playback.
// The keys are the addresses of Sound values, so a Sound must stay alive (and
// must not be copied or moved) while it is playing. The map is shared with
// sokol's audio thread, so every access is protected by sound_mutex.
__global sound_playing = map[voidptr]bool{}
__global sound_mutex = sync.Mutex{}

// Sound holds decoded 16-bit PCM samples ready for playback. channels is 1 for
// mono or 2 for interleaved stereo; pos is the next sample frame to play.
pub struct Sound {
pub mut:
	samples     []i16
	channels    int = 1
	sample_rate int = 44100
	pos         int
}

// read_u16_le reads a little-endian unsigned 16-bit value at byte offset at.
fn read_u16_le(bytes []u8, at int) u16 {
	return u16(bytes[at]) | (u16(bytes[at + 1]) << 8)
}

// read_u32_le reads a little-endian unsigned 32-bit value at byte offset at.
fn read_u32_le(bytes []u8, at int) u32 {
	return u32(bytes[at]) | (u32(bytes[at + 1]) << 8) | (u32(bytes[at + 2]) << 16) |
		(u32(bytes[at + 3]) << 24)
}

// load_wav decodes a 16-bit mono or stereo PCM WAV file into a Sound.
// Compressed formats, 8/24/32-bit samples and malformed files are rejected
// with an error. The file's sample rate is kept in Sound.sample_rate, but the
// actual output rate is chosen by sokol.audio.setup.
pub fn load_wav(path string) !Sound {
	bytes := os.read_bytes(path)!
	if bytes.len < 12 {
		return error('WAV: file too short')
	}
	if bytes[0] != `R` || bytes[1] != `I` || bytes[2] != `F` || bytes[3] != `F` {
		return error('WAV: missing RIFF header')
	}
	if bytes[8] != `W` || bytes[9] != `A` || bytes[10] != `V` || bytes[11] != `E` {
		return error('WAV: missing WAVE form type')
	}
	mut offset := 12
	mut channels := 0
	mut bits := 0
	mut rate := 44100
	mut data_offset := -1
	mut data_len := 0
	for offset + 8 <= bytes.len {
		is_fmt := bytes[offset] == `f` && bytes[offset + 1] == `m` && bytes[offset + 2] == `t`
			&& bytes[offset + 3] == ` `
		is_data := bytes[offset] == `d` && bytes[offset + 1] == `a` && bytes[offset + 2] == `t`
			&& bytes[offset + 3] == `a`
		size32 := read_u32_le(bytes, offset + 4)
		if size32 > 0x7fffffff {
			return error('WAV: chunk too large')
		}
		size := int(size32)
		offset += 8
		if is_fmt {
			if size < 16 || offset + 16 > bytes.len {
				return error('WAV: truncated fmt chunk')
			}
			format := read_u16_le(bytes, offset)
			channels = int(read_u16_le(bytes, offset + 2))
			rate = int(read_u32_le(bytes, offset + 4))
			bits = int(read_u16_le(bytes, offset + 14))
			if format != 1 {
				return error('WAV: only PCM is supported')
			}
			if channels < 1 || channels > 2 {
				return error('WAV: only mono and stereo are supported')
			}
			if bits != 16 {
				return error('WAV: only 16-bit samples are supported')
			}
		} else if is_data {
			if channels == 0 {
				return error('WAV: data chunk precedes fmt chunk')
			}
			data_offset = offset
			data_len = size
			break
		}
		step := size + size % 2
		if step < 0 || offset > bytes.len - step {
			break
		}
		offset += step
	}
	if data_offset < 0 {
		return error('WAV: no data chunk found')
	}
	if bits != 16 {
		return error('WAV: only 16-bit samples are supported')
	}
	frame_bytes := channels * 2
	if data_len % frame_bytes != 0 {
		return error('WAV: partial sample frame')
	}
	// Compare in i64: data_offset + data_len can exceed i32 range even when
	// each value individually passed the 0x7fffffff chunk-size guard above.
	if i64(data_offset) + i64(data_len) > i64(bytes.len) {
		return error('WAV: data chunk out of bounds')
	}
	mut samples := []i16{cap: data_len / 2}
	for i in 0 .. data_len / 2 {
		samples << i16(read_u16_le(bytes, data_offset + i * 2))
	}
	return Sound{ samples: samples, channels: channels, sample_rate: rate }
}

// play_sound starts (or restarts from the beginning) playback of s.
// The Sound must remain alive and unchanged until stop_sound is called or the
// samples are exhausted.
pub fn play_sound(s &Sound) {
	mut sound := unsafe { &Sound(voidptr(s)) }
	key := voidptr(s)
	sound_mutex.lock()
	sound.pos = 0
	sound_playing[key] = true
	sound_mutex.unlock()
}

// stop_sound stops playback of s and clears its active flag.
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

// sound_samples returns the raw signed 16-bit PCM samples of s. It is useful
// when you want to feed the samples to your own audio pipeline.
pub fn sound_samples(s &Sound) []i16 {
	return s.samples
}

// mix_sound_frames is a sokol.audio streaming callback. It clears buffer and
// mixes every active Sound into it, converting i16 samples to floats and
// duplicating channels when a mono sound plays on a stereo output. Sounds
// that reach the end of their samples are marked inactive and can be played
// again with play_sound. Use setup_sound_audio to install this callback.
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

// push_sounds mixes all active sounds into a temporary buffer and forwards it
// to sokol.audio.push. Call it once per frame from the main thread when using
// the push model. sokol uses either the streaming callback or the push model,
// so do not combine this with setup_sound_audio/set_audio_callback using a
// stream callback.
pub fn push_sounds() int {
	frames := audio.expect()
	if frames <= 0 {
		return 0
	}
	mut channels := audio.channels()
	if channels <= 0 {
		channels = 1
	}
	mut buffer := []f32{len: frames * channels, init: 0}
	mix_sound_frames(buffer.data, frames, channels)
	return audio.push(buffer.data, frames)
}

// SoundAudioConfig holds the optional parameters for audio setup.
@[params]
pub struct SoundAudioConfig {
pub mut:
	sample_rate int = 44100
	channels    int = 1
}

// set_audio_callback forwards cb to sokol.audio.setup with the given sample
// rate and channel count. Pass one of the stream callbacks (for example
// mix_sound_frames) or your own function of the same signature.
pub fn set_audio_callback(cb audio.FNStreamingCB, cfg SoundAudioConfig) {
	audio.setup(sample_rate: cfg.sample_rate, num_channels: cfg.channels, stream_cb: cb)
}

// setup_sound_audio installs mix_sound_frames as sokol.audio's streaming
// callback so that play_sound/stop_sound control playback automatically.
pub fn setup_sound_audio(cfg SoundAudioConfig) {
	set_audio_callback(mix_sound_frames, cfg)
}
