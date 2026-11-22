@[has_globals]
module audio

import sokol.audio as sokol_audio

__global sokol_volume_value f32 = 1
__global sokol_channels int = 2

// sokol_device returns a device backed by sokol.audio's push model.
pub fn sokol_device(cfg DeviceConfig) Device {
	return Device{
		name:      'sokol'
		config:    cfg
		setup_fn:  sokol_setup
		push_fn:   sokol_push
		volume_fn: sokol_volume
		close_fn:  sokol_close
		available: true
	}
}

fn sokol_setup(cfg DeviceConfig) !bool {
	sokol_channels = if cfg.channels > 0 { cfg.channels } else { 2 }
	// sokol asserts on a second setup; gg may already have initialized audio.
	if sokol_audio.is_valid() {
		return true
	}
	sokol_audio.setup(sample_rate: cfg.sample_rate, num_channels: cfg.channels)
	return true
}

// audio_push_frames converts an interleaved sample count to a frame count.
fn audio_push_frames(sample_count int, channels int) int {
	if channels <= 0 {
		return 0
	}
	return sample_count / channels
}

fn sokol_push(samples []f32) !int {
	frames := audio_push_frames(samples.len, sokol_channels)
	if frames <= 0 {
		return 0
	}
	mut scaled := []f32{len: samples.len}
	for i, s in samples {
		scaled[i] = s * sokol_volume_value
	}
	return sokol_audio.push(scaled.data, frames)
}

fn sokol_volume(value f32) {
	sokol_volume_value = value
}

fn sokol_close() {
	sokol_audio.shutdown()
}
