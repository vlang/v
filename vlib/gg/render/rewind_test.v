module render

fn rewind_test_quad(ctx &Context) {
	ctx.begin_quads()
	ctx.v2f(1, 1)
	ctx.v2f(5, 1)
	ctx.v2f(5, 5)
	ctx.v2f(1, 5)
	ctx.end()
}

fn rewind_test_context() &Context {
	ctx := new_context(Desc{
		max_vertices: 32
		max_commands: 8
	})
	ctx.matrix_mode_projection()
	ctx.ortho(0, 16, 16, 0, -1, 1)
	ctx.matrix_mode_modelview()
	ctx.translate(4, 0, 0)
	ctx.c4b(255, 0, 0, 255)
	return ctx
}

fn test_rewind_preserves_drawing_state_and_other_contexts() {
	ctx := rewind_test_context()
	other := rewind_test_context()
	rewind_test_quad(ctx)
	rewind_test_quad(other)
	clear := u32(0xffffffff)
	expected := ctx.render_cpu(16, 16, clear)
	assert expected.pixels[2 * 16 + 6] != clear
	assert expected.pixels[2 * 16 + 2] == clear
	frame_id := ctx.st.frame_id
	other_frame_id := other.st.frame_id
	ctx.rewind()
	assert ctx.st.frame_id == frame_id
	assert other.st.frame_id == other_frame_id
	assert other.num_vertices() == 6
	assert other.num_commands() == 1
	assert other.render_cpu(16, 16, clear).pixels == expected.pixels
	rewind_test_quad(ctx)
	assert ctx.num_vertices() == 6
	assert ctx.num_commands() == 1
	assert ctx.render_cpu(16, 16, clear).pixels == expected.pixels
}

fn test_rewind_recovers_from_an_unfinished_overflowing_primitive() {
	ctx := new_context(Desc{
		max_vertices: 6
		max_commands: 8
	})
	ctx.begin_triangles()
	for _ in 0 .. 7 {
		ctx.v2f(0, 0)
	}
	assert ctx.error() == .vertices_full
	ctx.rewind()
	assert ctx.error() == .no_error
	ctx.v2f(0, 0)
	ctx.end()
	assert ctx.num_vertices() == 0
	assert ctx.num_commands() == 0
	ctx.matrix_mode_projection()
	ctx.ortho(0, 16, 16, 0, -1, 1)
	rewind_test_quad(ctx)
	assert ctx.num_vertices() == 6
	assert ctx.num_commands() == 1
	assert ctx.error() == .no_error
	assert ctx.render_cpu(16, 16, 0xffffffff).pixels[2 * 16 + 2] != 0xffffffff
}

fn test_recording_counters_include_expanded_vertices_and_control_commands() {
	ctx := rewind_test_context()
	assert ctx.num_vertices() == 0
	assert ctx.num_commands() == 0
	rewind_test_quad(ctx)
	rewind_test_quad(ctx)
	assert ctx.num_vertices() == 12
	assert ctx.num_commands() == 1
	ctx.viewport(0, 0, 16, 16, true)
	ctx.scissor_rect(0, 0, 16, 16, true)
	assert ctx.num_vertices() == 12
	assert ctx.num_commands() == 3
	ctx.render_cpu(16, 16, 0)
	assert ctx.num_vertices() == 12
	assert ctx.num_commands() == 3
	ctx.rewind()
	assert ctx.num_vertices() == 0
	assert ctx.num_commands() == 0
}

fn test_commit_rebuilds_uniforms_without_resetting_drawing_state() {
	ctx := rewind_test_context()
	rewind_test_quad(ctx)
	expected := ctx.render_cpu(16, 16, 0xffffffff)
	assert expected.pixels[2 * 16 + 6] != 0xffffffff
	frame_id := ctx.st.frame_id
	commit_listener_fn(voidptr(ctx))
	assert ctx.st.frame_id == frame_id + 1
	assert ctx.num_vertices() == 0
	assert ctx.num_commands() == 0
	rewind_test_quad(ctx)
	assert ctx.render_cpu(16, 16, 0xffffffff).pixels == expected.pixels
}
