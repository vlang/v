// This program illustrates how to use sokol.audio in a simple console app, *without a gui*.
// See melody.v for an example of how it can be integrated into a graphics app.
import time
import sokol.audio

fn audio_callback(mut soundbuffer &f32, num_frames int, num_channels int, mut frame_0 &i32) {
	for frame := 0; frame < num_frames; frame++ {
		t := i32(f32(*frame_0 + frame) * 0.245)
		y := (t * (((t / 10 | 0) ^ ((t / 10 | 0) - 1280)) % 11) / 2 & 127) +
			(t * (((t / 640 | 0) ^ ((t / 640 | 0) - 2)) % 13) / 2 & 127)
		for ch := 0; ch < num_channels; ch++ {
			idx := frame * num_channels + ch
			a := f32(y - 127) / 255.0
			soundbuffer[idx] = a
		}
	}
	frame_0 += num_frames
}

// The example uses \r, to show a simple progress bar, while the music is playing.
// That works best, if the output is not buffered at all.
unbuffer_stdout()

println('The ByteBeat formula used in this example is from https://www.youtube.com/watch?v=V4GfkFbDojc , "Techno" by Gabriel Miceli')

mut frame_0 := i32(0) // offset of the current audio frames, relative to the start of the music
audio.setup(stream_userdata_cb: audio_callback, user_data: &frame_0) // our state/user_data can be just a simple integer

for t in 0 .. 600 {
	print('\r> t: ${t:5}s , frame_0: ${frame_0:10} samples')
	time.sleep(1 * time.second)
}
println('\nGood bye.')

audio.shutdown()
