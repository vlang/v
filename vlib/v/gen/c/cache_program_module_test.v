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
