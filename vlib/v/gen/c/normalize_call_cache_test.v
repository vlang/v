module c

import v.flat
import v.types

fn test_normalize_call_key_cache_follows_alternating_file_and_module_contexts() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.fn_param_types['first.target'] = []types.Type{}
	tc.fn_param_types['second.target'] = []types.Type{}
	tc.file_selective_imports['first.v\ntarget'] = ['first.target']
	tc.file_selective_imports['second.v\ntarget'] = ['second.target']
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	for _ in 0 .. 3 {
		tc.cur_module = 'main'
		tc.cur_file = 'first.v'
		assert g.normalize_call_key('target') == 'first.target'
		assert g.normalize_call_key('target') == 'first.target'
		tc.cur_file = 'second.v'
		assert g.normalize_call_key('target') == 'second.target'
		tc.cur_file = 'shared.v'
		tc.cur_module = 'first'
		assert g.normalize_call_key('target') == 'first.target'
		tc.cur_module = 'second'
		assert g.normalize_call_key('target') == 'second.target'
	}
}

fn test_normalize_call_key_worker_cache_has_independent_context() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.fn_param_types['first.target'] = []types.Type{}
	tc.fn_param_types['second.target'] = []types.Type{}
	tc.cur_module = 'first'
	tc.cur_file = 'shared.v'
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	assert g.normalize_call_key('target') == 'first.target'
	mut worker := g.new_parallel_worker(1)
	assert worker.normalize_call_cache != g.normalize_call_cache
	worker.tc.cur_module = 'second'
	assert worker.normalize_call_key('target') == 'second.target'
	assert g.normalize_call_key('target') == 'first.target'
	assert g.normalize_call_cache.module == 'first'
}
