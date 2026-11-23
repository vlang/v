module audio

// DeviceConfig selects the output format of an audio device.
pub struct DeviceConfig {
pub mut:
	sample_rate int = 44100
	channels    int = 2
}

// Device is a small function-pointer interface so the core module never
// depends on a concrete backend. Backends fill these fields in their ctor.
pub struct Device {
pub mut:
	name      string
	config    DeviceConfig
	setup_fn  fn (DeviceConfig) !bool = null_setup
	push_fn   fn ([]f32) !int         = null_push
	volume_fn fn (f32)                = null_volume
	close_fn  fn ()                   = null_close
	available bool
}

fn null_setup(cfg DeviceConfig) !bool {
	return false
}

fn null_push(samples []f32) !int {
	return 0
}

fn null_volume(value f32) {
	_ = value
}

fn null_close() {}

// null_device is a safe headless backend for tests and servers.
pub fn null_device() Device {
	return Device{ name: 'null', available: false }
}

pub fn (d &Device) setup() !bool {
	if !d.available {
		return false
	}
	return d.setup_fn(d.config)
}

pub fn (d &Device) push(samples []f32) !int {
	if !d.available {
		return 0
	}
	return d.push_fn(samples)
}

pub fn (d &Device) set_volume(value f32) {
	if d.available {
		d.volume_fn(value)
	}
}

pub fn (d &Device) close() {
	if d.available {
		d.close_fn()
	}
}
