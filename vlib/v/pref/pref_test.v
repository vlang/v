module pref

import os

// test_detect_vroot_from_subdir validates detect vroot from subdir behavior in v3 tests.
fn test_detect_vroot_from_subdir() {
	vroot := @VMODROOT
	v3_dir := os.join_path(vroot, 'vlib', 'v3')
	assert detect_vroot_from(v3_dir) == vroot
}

// test_detect_vroot_from_binary_path validates this v3 regression case.
fn test_detect_vroot_from_binary_path() {
	vroot := @VMODROOT
	v3_bin := os.join_path(vroot, 'vlib', 'v3', 'v3')
	assert detect_vroot_from(v3_bin) == vroot
}

fn test_get_module_path_resolves_alias_and_submodule() {
	root := os.join_path(os.temp_dir(), 'v3_pref_module_alias_${os.getpid()}')
	os.rmdir_all(root) or {}
	defer {
		os.rmdir_all(root) or {}
	}
	modules_dir := os.join_path_single(root, 'modules')
	canonical_dir := os.join_path_single(modules_dir, 'canonical')
	os.mkdir_all(os.join_path_single(canonical_dir, 'sub')) or { panic(err) }
	os.mkdir_all(os.join_path_single(modules_dir, 'legacy')) or { panic(err) }
	os.write_file(os.join_path_single(root, 'v.mod'), "Module { name: 'alias_test' }\n") or {
		panic(err)
	}
	os.write_file(os.join_path_single(canonical_dir, 'canonical.v'), 'module canonical\n') or {
		panic(err)
	}
	os.write_file(os.join_path(canonical_dir, 'sub', 'sub.v'), 'module sub\n') or { panic(err) }
	os.write_file(os.join_path(modules_dir, 'legacy', 'alias.v'),
		"@[alias: '@VMODROOT/modules/canonical'] module legacy\n") or { panic(err) }
	main_file := os.join_path_single(root, 'main.v')
	os.write_file(main_file, 'module main\n') or { panic(err) }
	prefs := new_preferences()
	assert prefs.get_module_path('modules.legacy', main_file) == os.real_path(canonical_dir)
	assert prefs.get_module_path('modules.legacy.sub', main_file) == os.real_path(os.join_path_single(canonical_dir,
		'sub'))
}

// test_detect_vroot_from_outside_a_checkout pins the twin of the driver walk in
// https://github.com/vlang/v/issues/28583: the walk must reach a filesystem
// root rather than the relative `.` that `os.dir` returns for a bare Windows
// drive, which would otherwise match the current directory.
fn test_detect_vroot_from_outside_a_checkout() {
	outside := os.join_path(os.vtmp_dir(), 'v_pref_outside_${os.getpid()}', 'a', 'b')
	os.mkdir_all(outside) or { panic(err) }
	defer {
		os.rmdir_all(os.dir(os.dir(outside))) or {}
	}
	cwd := os.getwd()
	os.chdir(@VMODROOT) or { panic(err) }
	defer {
		os.chdir(cwd) or {}
	}
	detected := detect_vroot_from(os.join_path(outside, 'hello.v'))
	assert detected != '.'
	if detected.len > 0 {
		assert os.is_dir(os.join_path(detected, 'vlib', 'builtin'))
	}
}
