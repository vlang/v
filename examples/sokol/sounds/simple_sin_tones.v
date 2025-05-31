// vtest build: !sanitize-memory-clang // Fails compilation with: `ld: /lib/x86_64-linux-gnu/libpthread.so.0: error adding symbols: DSO missing from command line`
import time
import math
import sokol.audio

const sw = time.new_stopwatch()
const sw_start_ms = sw.elapsed().milliseconds()

@[inline]
fn sintone(periods int, frame int, num_frames int) f32 {
	return math.sinf(f32(periods) * (2 * math.pi) * f32(frame) / f32(num_frames))
}

fn my_audio_stream_callback(mut soundbuffer &f32, num_frames int, num_channels int) {
	ms := sw.elapsed().milliseconds() - sw_start_ms
	for frame := 0; frame < num_frames; frame++ {
		for ch := 0; ch < num_channels; ch++ {
			idx := frame * num_channels + ch
			if ms < 250 {
				soundbuffer[idx] = 0.5 * sintone(20, frame, num_frames)
			} else if ms < 300 {
				soundbuffer[idx] = 0.5 * sintone(25, frame, num_frames)
			} else if ms < 1500 {
				soundbuffer[idx] *= sintone(22, frame, num_frames)
			} else {
				soundbuffer[idx] = 0.5 * sintone(25, frame, num_frames)
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
