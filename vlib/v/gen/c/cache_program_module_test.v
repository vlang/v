module c

import v.flat

fn test_cache_program_module_stays_file_scoped_when_module_has_cached_sources() {
	mut a := flat.FlatAst.new()
	cached_file_id := a.add_node(flat.Node{
		kind:  .file
		value: 'runtime_builtin.v'
	})
	cached_module_id := a.add_node(flat.Node{
		kind:  .module_decl
		value: 'builtin'
	})
	program_file_id := a.add_node(flat.Node{
		kind:  .file
		value: 'custom_builtin.v'
	})
	program_module_id := a.add_node(flat.Node{
		kind:  .module_decl
		value: 'builtin'
	})
	mut g := FlatGen.new()
	g.a = &a
	g.top_level_node_ids = [int(cached_file_id), int(cached_module_id), int(program_file_id),
		int(program_module_id)]
	g.set_cache_program_files(['custom_builtin.v'])
	_, _, program_modules := g.fn_gen_selection_info()
	assert !program_modules['builtin']
}

fn test_synthesized_helpers_follow_the_requesting_file_scope() {
	mut a := flat.FlatAst.new()
	mut g := FlatGen.new()
	g.a = &a
	g.set_cache_program_files(['main.v'])
	for helper in ['__v3_sum_eq_reflection__Type', '__v3_autostr_reflection__Type',
		'__v3_default_clone_reflection__Type'] {
		node_id := a.add_node(flat.Node{
			kind:  .fn_decl
			value: helper
		})
		node := a.nodes[int(node_id)]
		assert g.is_program_specialization_fn_node_with_qfn(node, int(node_id), helper,
			'main.v')
		assert !g.is_program_specialization_fn_node_with_qfn(node, int(node_id), helper,
			'reflection/type.v')
	}
}
