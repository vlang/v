module gfx

// `create_clear_pass_action` builds the PassAction struct without touching the
// GPU, so the struct it produces can be inspected directly.
fn test_a_clear_pass_action_clears_with_the_given_color() {
	mut pa := create_clear_pass_action(0.1, 0.2, 0.3, 0.4)
	assert pa.colors[0].load_action == .clear
	assert pa.colors[0].clear_value.r == 0.1
	assert pa.colors[0].clear_value.g == 0.2
	assert pa.colors[0].clear_value.b == 0.3
	assert pa.colors[0].clear_value.a == 0.4
}

fn test_the_remaining_attachments_are_left_at_their_default() {
	mut pa := create_clear_pass_action(1.0, 1.0, 1.0, 1.0)
	// only attachment 0 is configured
	for i in 1 .. 4 {
		assert pa.colors[i].load_action == ._default
	}
	// the store action of the cleared attachment is left at the default too
	assert pa.colors[0].store_action == ._default
	assert pa.depth.load_action == ._default
	assert pa.stencil.load_action == ._default
}

fn test_zero_alpha_produces_a_fully_transparent_clear() {
	mut pa := create_clear_pass_action(0.0, 0.0, 0.0, 0.0)
	assert pa.colors[0].clear_value.a == 0.0
	assert pa.colors[0].load_action == .clear
}
