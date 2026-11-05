module render

import sokol.gfx

fn test_loaded_pipeline_persists_across_batches() {
	ctx := new_context(max_vertices: 32, max_commands: 8)
	mut pipeline := Pipeline{
		id:         42
		blend_mode: .multiply
	}
	for i in 0 .. num_primitive_types {
		pipeline.pips[i] = gfx.Pipeline{ id: u32(100 + i) }
	}
	ctx.load_pipeline(pipeline)
	for _ in 0 .. 2 {
		ctx.begin_triangles()
		ctx.v2f(0, 0)
		ctx.v2f(1, 0)
		ctx.v2f(0, 1)
		ctx.end()
	}
	assert ctx.st.commands_count == 1
	assert ctx.st.commands[0].pipeline.id == 103
	assert ctx.st.commands[0].num_vertices == 6
	assert ctx.st.commands[0].blend_mode == .multiply
	assert ctx.st.pip_stack[ctx.st.pip_tos].id == pipeline.id

	ctx.push_pipeline()
	ctx.load_default_pipeline()
	assert ctx.current_pipeline().id == ctx.st.def_pip.id
	ctx.pop_pipeline()
	assert ctx.current_pipeline().id == pipeline.id
}
