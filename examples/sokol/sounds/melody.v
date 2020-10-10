import os
import sokol.audio

struct AContext {
mut:
	frame_0 int
}

fn my_audio_stream_callback(buffer &f32, num_frames, num_channels int, mut acontext AContext) {
	mut soundbuffer := buffer
	for frame := 0; frame < num_frames; frame++ {
		t := int(f32(acontext.frame_0 + frame) * 0.245)
		// Credits for the formula below: https://www.youtube.com/watch?v=V4GfkFbDojc
		// "Techno" by Gabriel Miceli
		y := (t * (((t / 10 | 0) ^ ((t / 10 | 0) -
			1280)) % 11) / 2 & 127) +
			(t * (((t / 640 | 0) ^ ((t / 640 | 0) - 2)) % 13) / 2 & 127)
		for ch := 0; ch < num_channels; ch++ {
			idx := frame * num_channels + ch
			unsafe {
				soundbuffer[idx] = f32(byte(y) - 127) / 255.0
			}
		}
	}
	acontext.frame_0 += num_frames
}

fn main() {
	audio.setup({
		stream_userdata_cb: my_audio_stream_callback
		user_data: &AContext{}
	})
	os.input('Press Enter to exit')
	audio.shutdown()
}
