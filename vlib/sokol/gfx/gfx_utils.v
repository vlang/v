module gfx

pub fn create_clear_pass(r f32, g f32, b f32, a f32) C.sg_pass_action {
	mut color_action := C.sg_color_attachment_action{
		action: gfx.Action(C.SG_ACTION_CLEAR)
	}
	// color_action.set_color_values(r, g, b, a)
	color_action.val[0] = r
	color_action.val[1] = g
	color_action.val[2] = b
	color_action.val[3] = a
	mut pass_action := C.sg_pass_action{}
	pass_action.colors[0] = color_action
	return pass_action
}
