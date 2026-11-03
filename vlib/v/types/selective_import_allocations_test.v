module types

import v.flat

// selective_import_test_info returns the import table of a file that selects
// `name` with the module-qualified `candidates`.
fn selective_import_test_info(name string, candidates []string) &FileImportInfo {
	mut info := &FileImportInfo{}
	info.selective_imports[flat.source_type_id(name)] = candidates.map(flat.source_type_id(it))
	return info
}

fn test_selective_import_fallback_preserves_module_and_ambiguity() {
	mut a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	tc.resolution_type_mode = true
	tc.cur_module = 'main'
	tc.cur_file = 'generated.v'
	tc.file_modules['first.v'] = 'main'
	tc.file_modules['second.v'] = 'main'
	tc.file_modules['other.v'] = 'other'
	tc.selective_import_files['first.v'] = selective_import_test_info('selected', ['dep.selected'])
	tc.selective_import_files['other.v'] = selective_import_test_info('selected', ['other.selected'])
	tc.fn_ret_types['dep.selected'] = String{}
	tc.fn_ret_types['other.selected'] = String{}
	assert tc.resolve_selective_import_symbol('selected') or { '' } == 'dep.selected'
	assert tc.resolve_any_selective_import_fn('selected') or { '' } == 'dep.selected'
	tc.selective_import_files['second.v'] = selective_import_test_info('selected', ['dep.selected'])
	assert tc.resolve_selective_import_symbol('selected') or { '' } == 'dep.selected'
	tc.selective_import_files['second.v'] = selective_import_test_info('selected', ['other.selected'])
	assert tc.resolve_selective_import_symbol('selected') == none
	assert tc.resolve_any_selective_import_fn('selected') == none
}

fn test_selective_import_index_is_private_to_workers_and_reset_with_type_caches() {
	mut a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	tc.resolution_type_mode = true
	tc.cur_module = 'main'
	tc.cur_file = 'generated.v'
	tc.file_modules['source.v'] = 'main'
	tc.selective_import_files['source.v'] = selective_import_test_info('selected', ['dep.selected'])
	tc.fn_ret_types['dep.selected'] = String{}
	assert tc.resolve_selective_import_symbol('selected') or { '' } == 'dep.selected'
	mut worker := tc.fork_for_parallel_check()
	worker.selective_import_files = {
		'source.v': selective_import_test_info('worker', ['dep.selected'])
	}
	assert worker.resolve_selective_import_symbol('worker') or { '' } == 'dep.selected'
	assert tc.resolve_selective_import_symbol('selected') or { '' } == 'dep.selected'
	tc.selective_import_files = {
		'source.v': selective_import_test_info('replacement', ['dep.selected'])
	}
	tc.set_fresh_type_cache(true)
	assert tc.resolve_selective_import_symbol('selected') == none
	assert tc.resolve_selective_import_symbol('replacement') or { '' } == 'dep.selected'
	tc.selective_import_files = {
		'source.v': selective_import_test_info('next', ['dep.selected'])
	}
	tc.set_fresh_type_cache_based_on(worker, true)
	assert tc.resolve_selective_import_symbol('next') or { '' } == 'dep.selected'
}

fn test_selective_import_misses_do_not_copy_unrelated_candidate_arrays() {
	$if gcboehm ? {
		mut a := flat.FlatAst.new()
		mut tc := TypeChecker.new(&a)
		tc.resolution_type_mode = true
		tc.cur_module = 'main'
		tc.cur_file = 'generated.v'
		for i in 0 .. 512 {
			mut candidates := []string{}
			for j in 0 .. 16 {
				candidates << 'dep_${i}.selected_${j}'
			}
			tc.selective_import_files['file_${i}.v'] = selective_import_test_info('selected_${i}',
				candidates)
		}
		// An interned name that no file selects takes the fallback index path.
		_ := flat.source_type_id('missing')
		before := gc_heap_usage().total_bytes
		for _ in 0 .. 128 {
			assert tc.resolve_selective_import_symbol('missing') == none
			assert tc.resolve_any_selective_import_fn('missing') == none
		}
		allocated := gc_heap_usage().total_bytes - before
		assert allocated < 1024 * 1024, 'selective import misses allocated ${allocated} bytes'
	}
}
