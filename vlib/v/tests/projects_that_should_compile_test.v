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

fn test_projects_should_run() {
	$if windows {
		return
	}
	res := vrun_ok('run', vroot_path('vlib/v/tests/testdata/enum_in_builtin') + os.path_separator)
	assert res.trim_space() == 'v0'

	res2 := vrun_ok('run', vroot_path('vlib/v/tests/testdata/modules_in_src/'))
	assert res2.trim_space() == 'somemodule somemoduletwo'

	res3 := vrun_ok('run', vroot_path('vlib/v/tests/testdata/module_named_cache/'))
	assert res3.trim_space().ends_with('cache.a: 123')
}
