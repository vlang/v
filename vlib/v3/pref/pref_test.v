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

fn test_vlib_module_path_maps_temporary_v2_directory() {
	expected := os.join_path('v2_toberemoved', 'ast')
	assert vlib_module_path('v2.ast') == expected
	assert vlib_module_path('v.ast') == os.join_path('v', 'ast')
}

fn test_get_module_path_prefers_project_v2_module_over_vlib_compatibility_path() {
	root := os.join_path(os.vtmp_dir(), 'v3_pref_v2_module_${os.getpid()}')
	project := os.join_path(root, 'project')
	local_module := os.join_path(project, 'v2', 'foo')
	compat_module := os.join_path(root, 'vlib', 'v2_toberemoved', 'foo')
	os.mkdir_all(local_module) or { panic(err) }
	os.mkdir_all(compat_module) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(local_module, 'foo.v'), 'module foo') or { panic(err) }
	os.write_file(os.join_path(compat_module, 'foo.v'), 'module foo') or { panic(err) }
	prefs := Preferences{
		vroot: root
	}
	importing_file := os.join_path(project, 'main.v')
	assert prefs.get_module_path('v2.foo', importing_file) == local_module
	os.rmdir_all(os.join_path(project, 'v2')) or { panic(err) }
	assert prefs.get_module_path('v2.foo', importing_file) == compat_module
}
