import gg
import gx
import sokol.audio

const credits = 'Based on the ByteBeat formula from: https://www.youtube.com/watch?v=V4GfkFbDojc \n "Techno" by Gabriel Miceli'

struct AppState {
mut:
	gframe  int         // the current graphical frame
	frame_0 int         // offset of the current audio frames, relative to the start of the music
	frames  [2048]f32   // a copy of the last rendered audio frames
	gg      &gg.Context // used for drawing
}

fn my_audio_stream_callback(buffer &f32, num_frames int, num_channels int, mut acontext AppState) {
	mut soundbuffer := buffer
	for frame := 0; frame < num_frames; frame++ {
		t := int(f32(acontext.frame_0 + frame) * 0.245)
		// "Techno" by Gabriel Miceli
		y := (t * (((t / 10 | 0) ^ ((t / 10 | 0) - 1280)) % 11) / 2 & 127) +
			(t * (((t / 640 | 0) ^ ((t / 640 | 0) - 2)) % 13) / 2 & 127)
		for ch := 0; ch < num_channels; ch++ {
			idx := frame * num_channels + ch
			unsafe {
				a := f32(byte(y) - 127) / 255.0
				soundbuffer[idx] = a
				acontext.frames[idx & 2047] = a
			}
		}
	}
	acontext.frame_0 += num_frames
}

fn main() {
	println(credits)
	mut state := &AppState{
		gg: 0
	}
	audio.setup(
		stream_userdata_cb: my_audio_stream_callback
		user_data: state
	)
	state.gg = gg.new_context(
		bg_color: gx.rgb(50, 50, 50)
		width: 1024
		height: 400
		use_ortho: true
		create_window: true
		window_title: 'ByteBeat Music'
		frame_fn: graphics_frame
		user_data: state
	)
	state.gg.run()
	audio.shutdown()
}

fn graphics_frame(mut state AppState) {
	state.gframe++
	state.gg.begin()
	state.draw()
	state.gg.end()
}

[inline]
fn (mut state AppState) bsample(idx int) byte {
	return byte(127 + state.frames[(state.gframe + idx) & 2047] * 128)
}

fn (mut state AppState) draw() {
	// first, reset and setup ortho projection
	for x in 0 .. 1024 {
		mut y := 100 * (state.frames[2 * x] + state.frames[2 * x + 1])
		state.gg.draw_line(x, 200, x, 200 + y, gx.rgba(state.bsample(x), state.bsample(x + 300),
			state.bsample(x + 700), 255))
	}
}
