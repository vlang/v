module gfx

// create_clear_pass_action returns a *clearing* `PassAction` that clears the `Pass`
// with the color defined by the color components `r`ed,`g`reen, `b`lue and `a`lpha.
pub fn create_clear_pass_action(r f32, g f32, b f32, a f32) PassAction {
	mut color_action := ColorAttachmentAction{
		load_action: .clear
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
