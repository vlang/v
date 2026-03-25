import os

fn testsuite_begin() {
	os.setenv('VCOLORS', 'never', true)
	// TODO: remove this, when after vc/v.c *also* uses `_cache`, instead of `cache`:
	old_cache_path := os.join_path(os.vmodules_dir(), 'cache')
	dump(old_cache_path)
	if os.exists(old_cache_path) {
		os.rmdir_all(old_cache_path)!
	}
}

fn vroot_path(relpath string) string {
	return os.real_path(os.join_path(@VMODROOT, relpath))
}

fn vrun_ok(options string, path string) string {
	cmd := '${os.quoted_path(@VEXE)} ${options} ${os.quoted_path(path)}'
	res := os.execute(cmd)
	if res.exit_code != 0 {
		eprintln('> failing vrun cmd: ${cmd}')
		eprintln('> output:\n${res.output}')
		assert res.exit_code == 0
	}
	return res.output
}

fn vrun_ok_in_dir(workdir string, options string, path string) string {
	old_wd := os.getwd()
	os.chdir(workdir) or { panic(err) }
	defer {
		os.chdir(old_wd) or { panic(err) }
	}
	return vrun_ok(options, path)
}

fn write_file(path string, content string) {
	os.write_file(path, content) or { panic(err) }
}

fn setup_module_resolution_workdir_fixture() string {
	workspace := os.join_path(os.vtmp_dir(), 'v_module_resolution_independent_of_workdir_${os.getpid()}')
	interp := r'${'
	os.rmdir_all(workspace) or {}
	os.mkdir_all(os.join_path(workspace, 'app', 'src')) or { panic(err) }
	os.mkdir_all(os.join_path(workspace, 'lib', 'src')) or { panic(err) }
	write_file(os.join_path(workspace, '.v.mod.stop'), '')
	write_file(os.join_path(workspace, 'app', 'v.mod'), "Module {\n\tname: 'app'\n\tdescription: ''\n\tversion: ''\n\tlicense: ''\n\tdependencies: []\n}\n")
	write_file(os.join_path(workspace, 'app', 'src', 'main.v'), "module main\n\nimport lib\n\nfn main() {\n\tprintln('Hello ${interp}lib.square(4)}!')\n}\n")
	write_file(os.join_path(workspace, 'lib', 'v.mod'), "Module {\n\tname: 'lib'\n\tdescription: ''\n\tversion: ''\n\tlicense: ''\n\tdependencies: []\n}\n")
	write_file(os.join_path(workspace, 'lib', 'src', 'lib.v'), 'module lib\n\npub fn square(x int) int {\n\treturn x * x\n}\n')
	return workspace
}

fn test_projects_should_run() {
	$if windows {
		return
	}
	res := vrun_ok('run', vroot_path('vlib/v/tests/testdata/enum_in_builtin') + os.path_separator)
	assert res.trim_space() == 'v0'

	res2 := vrun_ok('run', vroot_path('vlib/v/tests/testdata/modules_in_src/'))
	assert res2.trim_space() == 'somemodule somemoduletwo'

	res_sibling_modules_in_src := vrun_ok('run', vroot_path('vlib/v/tests/testdata/sibling_modules_in_src/'))
	assert res_sibling_modules_in_src.trim_space() == 'b says Hello from a!'

	res_sibling_modules_in_src_single_file := vrun_ok('run', vroot_path('vlib/v/tests/testdata/sibling_modules_in_src/src/main.v'))
	assert res_sibling_modules_in_src_single_file.trim_space() == 'b says Hello from a!'

	res3 := vrun_ok('run', vroot_path('vlib/v/tests/testdata/module_named_cache/'))
	assert res3.trim_space().ends_with('cache.a: 123')
}

fn test_module_resolution_is_independent_of_working_directory() {
	workspace := setup_module_resolution_workdir_fixture()
	defer {
		os.rmdir_all(workspace) or {}
	}
	res_root := vrun_ok_in_dir(workspace, 'run', 'app')
	assert res_root.trim_space() == 'Hello 16!'

	res_app := vrun_ok_in_dir(os.join_path(workspace, 'app'), 'run', '.')
	assert res_app.trim_space() == 'Hello 16!'
}
