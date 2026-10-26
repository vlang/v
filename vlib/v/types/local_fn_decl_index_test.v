module types

import v.flat

fn local_fn_index_fixture() &flat.FlatAst {
	mut a := flat.FlatAst.new()
	for module_name in ['main', 'other', 'builtin'] {
		a.add_val(.module_decl, module_name)
		for name in ['helper', 'C.uname', 'Thing.method', '__anon_fn_1', 'Box[int].__anon_fn_2',
			'pick[int]'] {
			a.add_val(.fn_decl, name)
		}
	}
	return &a
}

fn test_local_fn_index_matches_scan_for_every_callable_name() {
	a := local_fn_index_fixture()
	mut tc := TypeChecker.new(a)
	tc.set_fresh_type_cache(true)
	for module_name in ['main', 'other', 'builtin', 'missing'] {
		tc.cur_module = module_name
		for name in ['helper', 'C.uname', 'Thing.method', '__anon_fn_1', 'Box[int].__anon_fn_2',
			'pick[int]', '', 'missing'] {
			assert tc.local_fn_decl_exists(name) == (!name.contains('.')
				&& tc.local_fn_decl_exists_scan(name))
		}
	}
}

fn test_local_fn_index_tracks_modules_in_top_level_and_incremental_paths() {
	mut a := local_fn_index_fixture()
	mut tc := TypeChecker.new(a)
	tc.set_fresh_type_cache(true)
	for i in 0 .. a.nodes.len {
		tc.top_level_idx << i
	}
	tc.top_level_idx_nodes_len = a.nodes.len
	tc.cur_module = 'main'
	assert tc.local_fn_decl_exists('helper')
	a.add_val(.module_decl, 'generated')
	a.add_val(.fn_decl, '__anon_fn_late')
	a.add_val(.fn_decl, '__v3_generic[int]')
	a.add_val(.fn_decl, 'Box[int].__anon_fn_method')
	a.add_val(.fn_decl, 'ordinary_late')
	tc.cur_module = 'generated'
	assert tc.local_fn_decl_exists('__anon_fn_late')
	assert tc.local_fn_decl_exists('__v3_generic[int]')
	assert !tc.local_fn_decl_exists('ordinary_late')
	assert !tc.local_fn_decl_exists('Box[int].__anon_fn_method')
	assert !tc.local_fn_decl_exists('helper')
	tc.cur_module = 'builtin'
	assert tc.local_fn_decl_exists('helper')
}

fn test_local_fn_index_overlay_reads_frozen_base_without_indexing_private_tail() {
	mut a := local_fn_index_fixture()
	mut tc := TypeChecker.new(a)
	tc.set_fresh_type_cache(true)
	tc.cur_module = 'main'
	assert tc.local_fn_decl_exists('helper')
	base := tc.type_cache
	tc.type_cache = new_type_cache_with_base(true, base)
	a.add_val(.module_decl, 'main')
	a.add_val(.fn_decl, '__anon_fn_private')
	assert tc.local_fn_decl_exists('helper')
	assert !tc.local_fn_decl_exists('__anon_fn_private')
	assert tc.type_cache.local_fn_decl_index.len == 0
}

fn test_local_fn_index_stores_only_queryable_bare_names() {
	for top_level in [false, true] {
		mut a := local_fn_index_fixture()
		mut tc := TypeChecker.new(a)
		tc.set_fresh_type_cache(true)
		if top_level {
			for i in 0 .. a.nodes.len { tc.top_level_idx << i }
			tc.top_level_idx_nodes_len = a.nodes.len
		}
		tc.cur_module = 'main'
		assert tc.local_fn_decl_exists('helper')
		assert tc.type_cache.local_fn_decl_index.len == 9
		for key, _ in tc.type_cache.local_fn_decl_index {
			assert !key.contains('.')
		}
		a.add_val(.fn_decl, '__v3_late')
		a.add_val(.fn_decl, 'Box[int].__anon_fn_late')
		tc.cur_module = 'builtin'
		assert tc.local_fn_decl_exists('__v3_late')
		assert tc.type_cache.local_fn_decl_index.len == 10
	}
}
