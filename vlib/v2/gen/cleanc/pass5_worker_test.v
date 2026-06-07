module cleanc

fn test_pass5_worker_clones_global_type_lookup() {
	mut g := Gen.new([])
	g.global_var_types['main__settings'] = 'main__Settings'
	mut worker := g.new_pass5_worker([], 0)
	assert worker.global_var_types['main__settings'] == 'main__Settings'
	g.global_var_types['main__settings'] = 'main__OtherSettings'
	assert worker.global_var_types['main__settings'] == 'main__Settings'
}

fn test_specialized_receiver_method_index_tracks_ambiguity() {
	mut g := Gen.new([])
	g.fn_return_types['Box_T_i32__get'] = 'int'
	g.remember_specialized_fn_base('Box_T_i32__get')
	got := g.resolve_specialized_receiver_method('Box', 'get') or { '' }
	assert got == 'Box_T_i32__get'

	g.fn_return_types['Box_T_string__get'] = 'string'
	g.remember_specialized_fn_base('Box_T_string__get')
	if _ := g.resolve_specialized_receiver_method('Box', 'get') {
		assert false
	} else {
		assert true
	}
}
