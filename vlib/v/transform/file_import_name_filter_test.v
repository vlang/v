module transform

import v.flat
import v.types

fn test_file_import_name_filter_retains_exact_lookup_across_files_and_odd_keys() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	files := ['first.v', 'second.v', 'line\nbreak.v', '']
	aliases := ['dep', 'other', '', 'line\nbreak', 'éclair', '@escaped', '_private']
	for fi, file in files {
		for ai, alias in aliases {
			tc.file_imports[file_import_key(file, alias)] = 'module_${fi}_${ai}'
		}
	}
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.prepare_file_import_name_filter()
	for file in files {
		for alias in aliases {
			assert t.file_import_module(file, alias) or { '' } == tc.file_imports[file_import_key(file, alias)]
		}
		for i in 0 .. 4096 {
			alias := 'missing_${i}'
			assert t.file_import_module(file, alias) == none
		}
	}
	assert t.file_import_module('unknown.v', 'dep') == none
}

fn test_file_import_name_filter_is_shared_by_forks_and_preserves_shadowed_aliases() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.file_imports[file_import_key('first.v', 'dep')] = 'alpha'
	tc.file_imports[file_import_key('second.v', 'dep')] = 'beta'
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.prepare_file_import_name_filter()
	w := t.fork_program_view(&a, &tc, map[string]bool{}, false)
	scan := t.fork_scan_worker(&tc)
	assert voidptr(w.file_import_name_filter) == voidptr(t.file_import_name_filter)
	assert voidptr(scan.file_import_name_filter) == voidptr(t.file_import_name_filter)
	for worker in [&t, &w, scan] {
		assert worker.file_import_module('first.v', 'dep') or { '' } == 'alpha'
		assert worker.file_import_module('second.v', 'dep') or { '' } == 'beta'
		assert worker.file_import_module('first.v', 'unknown') == none
	}
}

fn test_file_import_name_filter_falls_back_after_import_count_changes() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.prepare_file_import_name_filter()
	assert t.file_import_name_filter.excludes('later', 0)
	assert t.file_import_module('first.v', 'later') == none
	tc.file_imports[file_import_key('first.v', 'later')] = 'added'
	assert !t.file_import_name_filter.excludes('later', 1)
	assert t.file_import_module('first.v', 'later') or { '' } == 'added'
}

fn test_file_import_name_filter_keeps_uncached_and_unprepared_lookup_paths() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.file_imports[file_import_key('first.v', 'dep')] = 'alpha'
	mut t := Transformer{ a: &a, tc: &tc }
	assert isnil(t.file_import_name_filter)
	assert t.file_import_module('first.v', 'dep') or { '' } == 'alpha'
	t.prepare_file_import_name_filter()
	assert isnil(t.file_import_cache)
	assert t.file_import_module('first.v', 'dep') or { '' } == 'alpha'
	assert t.file_import_module('first.v', 'unknown') == none
}

fn test_file_import_name_filter_never_rejects_a_hash_collision() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.file_imports[file_import_key('first.v', 'module')] = 'alpha'
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.prepare_file_import_name_filter()
	collision := 'mXdule'
	assert file_import_alias_bit(collision) == file_import_alias_bit('module')
	assert !t.file_import_name_filter.excludes(collision, tc.file_imports.len)
	assert t.file_import_module('first.v', collision) == none
}
