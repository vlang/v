import gg
import sokol.audio

struct AppState {
mut:
	frame_0 int       // offset of the current audio frames, relative to the start of the music
	frames  [2048]f32 // a copy of the last rendered audio frames
	gg      &gg.Context = unsafe { nil } // used for drawing
}

fn my_audio_stream_callback(mut soundbuffer &f32, num_frames int, num_channels int, mut acontext AppState) {
	for frame := 0; frame < num_frames; frame++ {
		t := int(f32(acontext.frame_0 + frame) * 0.245)
		// "Techno" by Gabriel Miceli
		y := (t * (((t / 10 | 0) ^ ((t / 10 | 0) - 1280)) % 11) / 2 & 127) +
			(t * (((t / 640 | 0) ^ ((t / 640 | 0) - 2)) % 13) / 2 & 127)
		for ch := 0; ch < num_channels; ch++ {
			idx := frame * num_channels + ch
			a := f32(y - 127) / 255.0
			soundbuffer[idx] = a
			acontext.frames[idx & 2047] = a
		}
	}
	acontext.frame_0 += num_frames
}

fn graphics_frame(mut state AppState) {
	ws := gg.window_size()
	center_y := f32(ws.height / 2)
	state.gg.begin()
	for x in 0 .. 1024 {
		vx := ws.width * f32(x) / 1024.0
		vy := center_y * 3.0 / 4.0 * (state.frames[2 * x] + state.frames[2 * x + 1])
		color := gg.Color{f(state, x), f(state, x + 300), f(state, x + 700), 255}
		state.gg.draw_line(vx, center_y, vx, center_y + vy, color)
	}
	state.gg.end()
}

fn f(state &AppState, idx int) u8 {
	return u8(127 + state.frames[(int(state.gg.frame) + idx) & 2047] * 128)
}

fn main() {
	println('Based on the ByteBeat formula from: https://www.youtube.com/watch?v=V4GfkFbDojc \n "Techno" by Gabriel Miceli')
	mut state := &AppState{}
	audio.setup(stream_userdata_cb: my_audio_stream_callback, user_data: state)
	defer { audio.shutdown() }
	state.gg = gg.new_context(
		bg_color:     gg.Color{50, 50, 50, 255}
		width:        800
		height:       600
		window_title: 'ByteBeat Music'
		frame_fn:     graphics_frame
		user_data:    state
	)
	state.gg.run()
}
