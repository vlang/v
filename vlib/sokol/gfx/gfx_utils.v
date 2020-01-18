module gfx

pub fn create_clear_pass(r, g, b, a f32) C.sg_pass_action {
	mut color_action := sg_color_attachment_action {
		action: C.SG_ACTION_CLEAR
	}
	color_action.set_color_values(r, g, b, a)

	mut pass_action := sg_pass_action{}
	pass_action.colors[0] = color_action
	return pass_action
}