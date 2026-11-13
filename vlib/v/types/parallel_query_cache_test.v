module types

import v.flat

fn test_nested_query_views_preserve_parallel_node_cache_ownership() {
	mut a := flat.FlatAst.new()
	for _ in 0 .. 3 {
		a.add(.ident)
	}
	mut master := TypeChecker.new(&a)
	master.extend_node_caches(a.nodes.len)
	outside := flat.NodeId(0)
	inside := flat.NodeId(1)
	original := Type(Array{ elem_type: &builtin_int_type })
	replacement := Type(Array{ elem_type: &builtin_string_type })
	master.remember_expr_type(outside, original)
	master.remember_expr_type(inside, original)
	master.remember_resolved_call(outside, 'main.original')
	master.remember_resolved_call(inside, 'main.inside')
	mut worker := master.fork_for_parallel_check()
	worker.check_range_lo = 1
	worker.check_range_hi = 1
	mut views := [worker.fork_type_parse_view('query.v', 'main'), worker.fork_smartcast_query_view()]
	for mut view in views {
		// A helper may read its worker's item, but another item's dense slots
		// can be changing concurrently or point into a disposable worker arena.
		assert !isnil(view.cached_expr_type_ref(inside))
		assert view.cached_resolved_call(inside)? == 'main.inside'
		assert isnil(view.cached_expr_type_ref(outside))
		assert view.cached_resolved_call(outside) == none
		view.remember_expr_type(outside, replacement)
		view.remember_resolved_call(outside, 'main.replacement')
		assert view.cached_expr_type_ref(outside).name() == '[]string'
		assert view.cached_resolved_call(outside)? == 'main.replacement'
		assert master.cached_expr_type_ref(outside).name() == '[]int'
		assert master.cached_resolved_call(outside)? == 'main.original'
		assert isnil(worker.cached_expr_type_ref(outside))
		assert worker.cached_resolved_call(outside) == none
	}
	worker.free_parallel_check_worker_cache()
}

fn test_reachability_query_views_keep_all_node_cache_writes_private() {
	mut a := flat.FlatAst.new()
	id := a.add(.ident)
	mut master := TypeChecker.new(&a)
	master.extend_node_caches(a.nodes.len)
	master.remember_expr_type(id, Type(Array{ elem_type: &builtin_int_type }))
	mut worker := master.fork_for_parallel_check()
	// Reachability forks do not own a checked function's node range.
	mut views := [worker.fork_type_parse_view('query.v', 'main'), worker.fork_smartcast_query_view()]
	for mut view in views {
		assert isnil(view.cached_expr_type_ref(id))
		view.remember_expr_type(id, Type(Array{ elem_type: &builtin_string_type }))
		assert view.cached_expr_type_ref(id).name() == '[]string'
		assert master.cached_expr_type_ref(id).name() == '[]int'
	}
	worker.free_parallel_check_worker_cache()
}

fn test_cached_parse_views_refresh_node_ownership_on_recent_and_map_hits() {
	mut a := flat.FlatAst.new()
	for _ in 0 .. 3 {
		a.add(.ident)
	}
	mut tc := TypeChecker.new(&a)
	tc.extend_node_caches(a.nodes.len)
	for i in 0 .. a.nodes.len {
		tc.remember_expr_type(flat.NodeId(i), Type(Array{ elem_type: &builtin_int_type }))
	}
	tc.file_modules['left.v'] = 'left'
	tc.file_modules['right.v'] = 'right'
	tc.structs[flat.source_type_id('left.Item')] = []StructField{}
	tc.structs[flat.source_type_id('right.Item')] = []StructField{}
	source := flat.source_type_id('[]Item')
	// Top-level checks can warm both caches before the master enters sparse mode.
	for file in ['left.v', 'right.v'] {
		tc.cur_file = file
		tc.cur_module = tc.file_modules[file]
		_ = tc.parse_resolution_type(source)
		_ = tc.parse_resolution_type_in_file(source, file)
	}
	for i, file in ['right.v', 'right.v', 'left.v', 'left.v', 'right.v'] {
		tc.cur_file = file
		tc.cur_module = tc.file_modules[file]
		tc.parallel_check_sparse = i < 4
		tc.check_range_lo = i % 3
		tc.check_range_hi = i % 3
		_ = tc.parse_resolution_type(source)
		direct := tc.resolution_type_views.by_file[file] or { panic('missing direct parse view') }
		assert !isnil(direct.cached_expr_type_ref(flat.NodeId(i % 3)))
		assert isnil(direct.cached_expr_type_ref(flat.NodeId((i + 1) % 3))) == tc.parallel_check_sparse
		scoped := tc.type_parse_view_in_file(file)
		_ = scoped.parse_resolution_type(source)
		unscoped := tc.resolution_type_views.by_file[file] or { panic('missing parse view') }
		for view in [scoped, unscoped] {
			for j in 0 .. a.nodes.len {
				cached := view.cached_expr_type_ref(flat.NodeId(j))
				if tc.parallel_check_sparse && j != tc.check_range_lo {
					assert isnil(cached)
				} else {
					assert !isnil(cached)
					assert cached.name() == '[]int'
				}
			}
		}
	}
}
