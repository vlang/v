@[has_globals]
module audio

import sokol.audio as sokol_audio

__global sokol_volume_value f32 = 1

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
	sokol_audio.setup(sample_rate: cfg.sample_rate, num_channels: cfg.channels)
	return true
}

fn sokol_push(samples []f32) !int {
	mut scaled := []f32{len: samples.len}
	for i, s in samples {
		scaled[i] = s * sokol_volume_value
	}
	return sokol_audio.push(scaled.data, scaled.len)
}

fn sokol_volume(value f32) {
	sokol_volume_value = value
}

fn sokol_close() {
	sokol_audio.shutdown()
}
