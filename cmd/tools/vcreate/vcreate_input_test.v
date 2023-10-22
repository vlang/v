import os
import v.vmod

// Note: the following uses `test_vcreate` and NOT `vcreate_input_test` deliberately,
// to both avoid confusions with the name of the current test itself, and to
// avoid clashes with the postfix `_test.v`, that V uses for its own test files.
const (
	// Expect has to be installed for the test.
	expect_exe = os.find_abs_path_of_executable('expect') or {
		eprintln('skipping test, since expect is missing')
		exit(0)
	}
	// Directory where the Expect scripts will create projects.
	test_module_path  = os.join_path(os.vtmp_dir(), 'v', 'test_vcreate_input')
	// Directory that contains the Expect scripts used in the test.
	expect_tests_path = os.join_path(@VMODROOT, 'cmd', 'tools', 'vcreate', 'tests')
)

fn testsuite_begin() {
	dump(expect_exe)
	dump(test_module_path)
	dump(expect_tests_path)
}

fn prepare_test_path() ! {
	os.rmdir_all(test_module_path) or {}
	os.mkdir_all(test_module_path) or {}
	os.chdir(test_module_path)!
}

fn test_new_with_no_arg_input() {
	prepare_test_path()!
	project_name := 'my_project'
	res := os.execute('${os.quoted_path(expect_exe)} ${os.join_path(expect_tests_path,
		'new_with_no_arg.expect')} ${@VMODROOT} ${project_name}')
	if res.exit_code != 0 {
		assert false, res.output
	}
	// Assert mod data set in `new_no_arg.expect`.
	mod := vmod.decode(os.read_file(os.join_path(test_module_path, project_name, 'v.mod')) or {
		assert false, 'Failed reading v.mod of ${project_name}'
		return
	}) or {
		assert false, err.str()
		return
	}
	assert mod.name == project_name
	assert mod.description == 'My Awesome V Project.'
	assert mod.version == '0.1.0'
	assert mod.license == 'GPL'
}

fn test_new_with_name_arg_input() {
	prepare_test_path()!
	project_name := 'my_other_project'
	res := os.execute('${os.quoted_path(expect_exe)} ${os.join_path(expect_tests_path,
		'new_with_name_arg.expect')} ${@VMODROOT} ${project_name}')
	if res.exit_code != 0 {
		assert false, res.output
	}
	// Assert mod data set in `new_with_name_arg.expect`.
	mod := vmod.decode(os.read_file(os.join_path(test_module_path, project_name, 'v.mod')) or {
		assert false, 'Failed reading v.mod of ${project_name}'
		return
	}) or {
		assert false, err.str()
		return
	}
	assert mod.name == project_name
	assert mod.description == ''
	assert mod.version == '0.0.0'
	assert mod.license == 'MIT'
}

fn test_new_with_model_arg_input() {
	prepare_test_path()!
	project_name := 'my_lib'
	model := 'lib'
	res := os.execute('${os.quoted_path(expect_exe)} ${os.join_path(expect_tests_path,
		'new_with_model_arg.expect')} ${@VMODROOT} ${project_name} ${model}')
	if res.exit_code != 0 {
		assert false, res.output
	}
	// Assert mod data set in `new_with_model_arg.expect`.
	mod := vmod.decode(os.read_file(os.join_path(test_module_path, project_name, 'v.mod')) or {
		assert false, 'Failed reading v.mod of ${project_name}'
		return
	}) or {
		assert false, err.str()
		return
	}
	assert mod.name == project_name
	assert mod.description == 'My Awesome V Project.'
	assert mod.version == '0.0.1'
	assert mod.license == 'MIT'
}

fn testsuite_end() {
	os.rmdir_all(test_module_path) or {}
}
