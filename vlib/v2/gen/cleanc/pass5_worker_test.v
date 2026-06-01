module cleanc

fn test_pass5_worker_clones_global_type_lookup() {
	mut g := Gen.new([])
	g.global_var_types['main__settings'] = 'main__Settings'
	mut worker := g.new_pass5_worker([], 0)
	assert worker.global_var_types['main__settings'] == 'main__Settings'
	g.global_var_types['main__settings'] = 'main__OtherSettings'
	assert worker.global_var_types['main__settings'] == 'main__Settings'
}
