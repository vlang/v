module gg

import audio as std_audio

// load_wav decodes a 16-bit mono/stereo PCM WAV file via vlib/audio.
pub fn load_wav(path string) !Sound {
	buf := std_audio.load_wav(path) or { return error('${err}') }
	return Sound{
		samples:     buf.samples
		channels:    buf.channels
		sample_rate: buf.sample_rate
	}
}

// new_shared_audio_device returns a vlib/audio device backed by sokol.
pub fn new_shared_audio_device(cfg std_audio.DeviceConfig) std_audio.Device {
	return std_audio.sokol_device(cfg)
}

// shared_audio_null_device returns a headless audio device for tests.
pub fn shared_audio_null_device() std_audio.Device {
	return std_audio.null_device()
}
