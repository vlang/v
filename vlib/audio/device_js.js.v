@[has_globals]
module audio

// Minimal WebAudio bindings. Types are declared as JS interfaces so V emits
// them verbatim when targeting the browser.
pub interface JS.AudioNode {
	connect(destination JS.AudioNode)
	disconnect()
}

pub interface JS.AudioDestinationNode {
	JS.AudioNode
}

pub interface JS.AudioBuffer {
	numberOfChannels int
	length           int
	sampleRate       f32
	getChannelData(channel int) []f32
}

pub interface JS.AudioBufferSourceNode {
	JS.AudioNode
	buffer JS.AudioBuffer
	start(when f32)
	stop(when f32)
}

pub interface JS.AudioContext {
	sampleRate  f32
	currentTime f32
	destination JS.AudioDestinationNode
	createBuffer(channels int, length int, sampleRate int) JS.AudioBuffer
	createBufferSource() JS.AudioBufferSourceNode
}

pub fn js_audio_context() JS.AudioContext {
	mut x := JS.Any(unsafe { voidptr(u64(0)) })
	#x = new (window.AudioContext || window.webkitAudioContext)();
	return x
}

__global web_ctx JS.AudioContext = unsafe { nil }
__global web_channels int = 2
__global web_volume f32 = 1

fn web_setup(cfg DeviceConfig) !bool {
	web_ctx = js_audio_context()
	web_channels = if cfg.channels > 0 { cfg.channels } else { 2 }
	return true
}

fn web_push(samples []f32) !int {
	if isnil(web_ctx) || web_channels <= 0 {
		return 0
	}
	frames := samples.len / web_channels
	if frames <= 0 {
		return 0
	}
	buf := web_ctx.createBuffer(web_channels, frames, int(web_ctx.sampleRate))
	for ch in 0 .. web_channels {
		channel := buf.getChannelData(ch)
		for i in 0 .. frames {
			if i < channel.len {
				channel[i] = samples[i * web_channels + ch] * web_volume
			}
		}
	}
	source := web_ctx.createBufferSource()
	source.buffer = buf
	source.connect(web_ctx.destination)
	source.start(web_ctx.currentTime)
	return frames
}

fn web_set_volume(value f32) {
	web_volume = value
}

fn web_close() {
	web_ctx = unsafe { nil }
}

// webaudio_device returns a browser output device backed by WebAudio.
pub fn webaudio_device(cfg DeviceConfig) Device {
	return Device{
		name:      'webaudio'
		config:    cfg
		setup_fn:  web_setup
		push_fn:   web_push
		volume_fn: web_set_volume
		close_fn:  web_close
		available: true
	}
}
