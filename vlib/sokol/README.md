## Description

`sokol` is a thin wrapper around [sokol](https://github.com/floooh/sokol),
which in turn is a library of "Simple STB-style cross-platform libraries
for C and C++, written in C.", that provide access to graphics/audio/input
processing.

Each `.h` file in the sokol source code is well-documented as can be seen here:

[sokol_audio.h](https://github.com/floooh/sokol/blob/master/sokol_audio.h)

## Example from `@VROOTDIR/examples/sokol/sounds/simple_sin_tones.v`

```v cgen
import time
import math
import sokol.audio

const sw = time.new_stopwatch()
const sw_start_ms = sw.elapsed().milliseconds()

@[inline]
fn sintone(periods int, frame int, num_frames int) f32 {
	return math.sinf(f32(periods) * (2 * math.pi) * f32(frame) / f32(num_frames))
}

fn my_audio_stream_callback(buffer &f32, num_frames int, num_channels int) {
	ms := sw.elapsed().milliseconds() - sw_start_ms
	unsafe {
		mut soundbuffer := buffer
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
}

fn main() {
	audio.setup(
		stream_cb: my_audio_stream_callback
	)
	time.sleep(2000 * time.millisecond)
	audio.shutdown()
}
```
