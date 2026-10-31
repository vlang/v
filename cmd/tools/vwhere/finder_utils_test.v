module main

import os

// finder_utils_test covers the file collection and module resolution helpers of
// `v where`. `search_for_matches`/`search_within_file` need the real V tree and
// are already covered by vwhere_test.v; `resolve_module`'s `vmod_dir`/`vlib_dir`
// branches depend on the installed user modules, so they are only checked
// against a synthetic module name that cannot exist.

fn vw_tree() !string {
	root := os.join_path(os.vtmp_dir(), 'vwhere_probe_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root)!
	return root
}

fn test_collect_v_files_rejects_empty_and_missing_paths() {
	if res := collect_v_files('', true) {
		assert false, 'expected an error, got ${res}'
	} else {
		assert err.msg().contains('does not exist or is not a directory'), err.msg()
	}
	if res := collect_v_files(os.join_path(os.vtmp_dir(), 'vwhere_absent_${os.getpid()}'), true) {
		assert false, 'expected an error, got ${res}'
	} else {
		assert err.msg().contains('does not exist or is not a directory'), err.msg()
	}
}

fn test_collect_v_files_keeps_only_v_and_vsh() {
	root := vw_tree()!
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'a.v'), 'module a')!
	os.write_file(os.join_path(root, 'b.vsh'), 'x')!
	os.write_file(os.join_path(root, 'notes.md'), 'x')!
	os.write_file(os.join_path(root, 'Makefile'), 'x')!
	mut got := collect_v_files(root, true)!.map(os.file_name(it))
	got.sort()
	assert got == ['a.v', 'b.vsh']
}

fn test_collect_v_files_skips_git_and_tests_directories() {
	root := vw_tree()!
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'a.v'), 'module a')!
	os.mkdir_all(os.join_path(root, '.git'))!
	os.write_file(os.join_path(root, '.git', 'hidden.v'), 'module hidden')!
	os.mkdir_all(os.join_path(root, 'tests'))!
	os.write_file(os.join_path(root, 'tests', 'a_test.v'), 'module tests')!
	os.mkdir_all(os.join_path(root, 'nested', 'tests'))!
	os.write_file(os.join_path(root, 'nested', 'tests', 'b_test.v'), 'module tests')!
	os.write_file(os.join_path(root, 'nested', 'c.v'), 'module c')!
	mut got := collect_v_files(root, true)!.map(os.file_name(it))
	got.sort()
	assert got == ['a.v', 'c.v']
}

fn test_collect_v_files_non_recursive_stops_at_the_first_level() {
	root := vw_tree()!
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'a.v'), 'module a')!
	os.mkdir_all(os.join_path(root, 'sub'))!
	os.write_file(os.join_path(root, 'sub', 'b.v'), 'module b')!
	got := collect_v_files(root, false)!.map(os.file_name(it))
	assert got == ['a.v']
}

fn test_collect_v_files_returns_absolute_input_paths() {
	root := vw_tree()!
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'a.v'), 'module a')!
	got := collect_v_files(root, true)!
	assert got.len == 1
	assert os.is_abs_path(got[0])
	assert got[0] == os.join_path_single(root, 'a.v')
}

fn test_resolve_module_rejects_an_unknown_name() {
	name := 'vwhere_probe_absent_${os.getpid()}'
	if res := resolve_module(name) {
		assert false, 'expected an error, got ${res}'
	} else {
		assert err.msg() == 'Path: ${name} not found'
	}
}
