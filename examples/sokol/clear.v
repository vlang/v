// This example shows how to clear your window on each frame, with a different color, using a Sokol pass.
// It is ported from https://github.com/floooh/sokol-samples/blob/master/sapp/clear-sapp.c .
import sokol.gfx
import sokol.sapp

fn frame(mut action gfx.PassAction) {
	g := f32(action.colors[0].clear_value.g + 0.01)
	action.colors[0].clear_value.g = if g > 1.0 { 0 } else { g }
	gfx.begin_pass(sapp.create_default_pass(action))
	gfx.end_pass()
	gfx.commit()
}

fn main() {
	action := gfx.create_clear_pass_action(1.0, 0, 0, 1.0)
	sapp.run(
		window_title:      c'Clear (sokol app)'
		width:             400
		height:            300
		init_cb:           || gfx.setup(sapp.create_desc())
		cleanup_cb:        || gfx.shutdown()
		frame_userdata_cb: frame
		user_data:         &action
	)
}
