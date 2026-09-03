// vtest build: !openbsd && !sanitize-memory-clang // Fails compilation with: `ld: /lib/x86_64-linux-gnu/libpthread.so.0: error adding symbols: DSO missing from command line`
import time
import math
import sokol.audio

const sw = time.new_stopwatch()
const sw_start_ms = sw.elapsed().milliseconds()

@[inline]
fn sintone(periods int, frame int, num_frames int) f32 {
	return math.sinf(f32(periods) * (2 * math.pi) * f32(frame) / f32(num_frames))
}

fn my_audio_stream_callback(mut soundbuffer &f32, num_frames i32, num_channels i32) {
	frame_count := int(num_frames)
	channel_count := int(num_channels)
	ms := sw.elapsed().milliseconds() - sw_start_ms
	for frame := 0; frame < frame_count; frame++ {
		for ch := 0; ch < channel_count; ch++ {
			idx := frame * channel_count + ch
			// The sokol audio callback guarantees `soundbuffer` points to at least
			// `num_frames * num_channels` writable samples, so `idx` (`frame < num_frames`,
			// `ch < num_channels`) is always in bounds for the writes below.
			if ms < 250 {
				unsafe {
					soundbuffer[idx] = 0.5 * sintone(20, frame, frame_count)
				}
			} else if ms < 300 {
				unsafe {
					soundbuffer[idx] = 0.5 * sintone(25, frame, frame_count)
				}
			} else if ms < 1500 {
				unsafe {
					soundbuffer[idx] *= sintone(22, frame, frame_count)
				}
			} else {
				unsafe {
					soundbuffer[idx] = 0.5 * sintone(25, frame, frame_count)
				}
			}
		}
	}
}

fn main() {
	audio.setup(
		stream_cb: my_audio_stream_callback
	)
	time.sleep(2000 * time.millisecond)
	audio.shutdown()
}
