// This example shows how to clear your window on each frame, with a different color, using a Sokol pass.
// It is ported from https://github.com/floooh/sokol-samples/blob/master/sapp/clear-sapp.c .
import sokol.gfx
import sokol.sapp

fn init(action &gfx.PassAction) {
	gfx.setup(sapp.create_desc())
}

fn frame(mut action gfx.PassAction) {
	g := f32(action.colors[0].clear_value.g + 0.01)
	action.colors[0].clear_value.g = if g > 1.0 { 0 } else { g }
	gfx.begin_pass(sapp.create_default_pass(action))
	gfx.end_pass()
	gfx.commit()
}

fn main() {
	mut action := gfx.create_clear_pass_action(1.0, 0, 0, 1.0)
	desc := sapp.Desc{
		window_title: 'Clear (sokol app)'.str
		width: 400
		height: 300
		frame_userdata_cb: frame
		init_userdata_cb: init
		user_data: &action
	}
	sapp.run(&desc)
}
