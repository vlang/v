module gfx

@[deprecated: 'use create_clear_pass_action instead']
@[deprecated_after: '2024-09-03']
pub fn create_clear_pass(r f32, g f32, b f32, a f32) PassAction {
	return create_clear_pass_action(r, g, b, a)
}

pub fn create_clear_pass_action(r f32, g f32, b f32, a f32) PassAction {
	mut color_action := ColorAttachmentAction{
		load_action: .clear // unsafe { Action(C.SG_ACTION_CLEAR) }
		clear_value: Color{
			r: r
			g: g
			b: b
			a: a
		}
	}
	// color_action.set_color_values(r, g, b, a)
	mut pass_action := PassAction{}
	pass_action.colors[0] = color_action
	return pass_action
}
