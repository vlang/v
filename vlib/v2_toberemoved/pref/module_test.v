module pref

import os

fn test_get_module_path_finds_sibling_module_from_importing_file_ancestor() {
	tmp_dir := os.join_path(os.vtmp_dir(), 'v2_pref_sibling_module_${os.getpid()}')
	project_dir := os.join_path(tmp_dir, 'app')
	project_subdir := os.join_path(project_dir, 'ui')
	sibling_module_dir := os.join_path(tmp_dir, 'viper')
	os.mkdir_all(project_subdir) or { panic(err) }
	os.mkdir_all(sibling_module_dir) or { panic(err) }
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	os.write_file(os.join_path(project_dir, 'v.mod'), "Module { name: 'app' }") or { panic(err) }
	os.write_file(os.join_path(sibling_module_dir, 'v.mod'), "Module { name: 'viper' }") or {
		panic(err)
	}

	prefs := Preferences{
		vroot:         tmp_dir
		vmodules_path: os.join_path(tmp_dir, '.vmodules')
	}
	importing_file := os.join_path(project_subdir, 'text.v')
	assert prefs.get_module_path('viper', importing_file) == sibling_module_dir
}
