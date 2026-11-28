module gg

import sokol.audio as sokol_audio

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

// set_audio_callback installs cb on sokol.audio.
pub fn set_audio_callback(cb sokol_audio.FNStreamingCB, cfg SoundAudioConfig) {
	sokol_audio.setup(sample_rate: cfg.sample_rate, num_channels: cfg.channels, stream_cb: cb)
}

// setup_sound_audio installs mix_sound_frames as sokol's stream callback.
pub fn setup_sound_audio(cfg SoundAudioConfig) {
	set_audio_callback(mix_sound_frames, cfg)
}
