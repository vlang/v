// vtest build: !openbsd

// This program illustrates how to use sokol.audio in a simple console app, *without a gui*.
// See melody.v for an example of how it can be integrated into a graphics app.
import time
import sokol.audio

struct ByteBeatState {
mut:
	frame_0 int
}

fn audio_callback(mut soundbuffer &f32, num_frames i32, num_channels i32, mut state ByteBeatState) {
	frame_count := int(num_frames)
	channel_count := int(num_channels)
	for frame := 0; frame < frame_count; frame++ {
		t := i32(f32(state.frame_0 + frame) * 0.245)
		y := (t * (((t / 10 | 0) ^ ((t / 10 | 0) - 1280)) % 11) / 2 & 127) +
			(t * (((t / 640 | 0) ^ ((t / 640 | 0) - 2)) % 13) / 2 & 127)
		for ch := 0; ch < channel_count; ch++ {
			idx := frame * channel_count + ch
			a := f32(y - 127) / 255.0
			unsafe {
				soundbuffer[idx] = a
			}
		}
	}
	state.frame_0 += frame_count
}

// The example uses \r, to show a simple progress bar, while the music is playing.
// That works best, if the output is not buffered at all.
unbuffer_stdout()

println('The ByteBeat formula used in this example is from https://www.youtube.com/watch?v=V4GfkFbDojc , "Techno" by Gabriel Miceli')

mut state := &ByteBeatState{}
audio.setup(stream_userdata_cb: audio_callback, user_data: state)
if !audio.is_valid() {
	println('audio.setup failed')
	return
}
defer {
	audio.shutdown()
}

for t in 0 .. 600 {
	print('\r> t: ${t:5}s , frame_0: ${state.frame_0:10} samples')
	time.sleep(1 * time.second)
}
println('\nGood bye.')
