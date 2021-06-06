import os

fn testsuite_begin() {
	os.setenv('VCOLORS', 'never', true)
}

fn vroot_path(relpath string) string {
	return os.real_path(os.join_path(@VMODROOT, relpath))
}

fn vrun(options string, path string) os.Result {
	return os.execute('${@VEXE} $options $path')
}

fn test_projects_should_run() {
	res := vrun('run', vroot_path('vlib/v/tests/testdata/enum_in_builtin') + os.path_separator)
	assert res.exit_code == 0
	assert res.output.trim_space() == 'v0'
}
