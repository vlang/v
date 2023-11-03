import os
import v.vmod

const (
	vroot      = @VEXEROOT
	// Expect has to be installed for the test.
	expect_exe = os.quoted_path(os.find_abs_path_of_executable('expect') or {
		eprintln('skipping test, since expect is missing')
		exit(0)
	})
	// Directory that contains the Expect scripts used in the test.
	expect_tests_path = os.join_path(vroot, 'cmd', 'tools', 'vcreate', 'tests')
	// Running tests appends a tsession path to VTMP, which is automatically cleaned up after the test.
	// The following will result in e.g. `$VTMP/tsession_7fe8e93bd740_1612958707536/test_vcreate_input/`.
	test_module_path  = os.join_path(os.vtmp_dir(), 'test_vcreate_input')
)

fn testsuite_begin() {
	dump(expect_exe)
	dump(test_module_path)
	dump(expect_tests_path)
}

fn testsuite_end() {
	os.rmdir_all(test_module_path) or {}
}

fn prepare_test_path() ! {
	os.rmdir_all(test_module_path) or {}
	os.mkdir_all(test_module_path) or {}
	os.chdir(test_module_path)!
}

fn test_new_with_no_arg_input() {
	prepare_test_path()!
	project_name := 'my_project'
	os.execute_opt('${expect_exe} ${os.join_path(expect_tests_path, 'new_with_no_arg.expect')} ${vroot} ${project_name}') or {
		assert false, err.msg()
	}
	// Assert mod data set in `new_no_arg.expect`.
	mod := vmod.from_file(os.join_path(test_module_path, project_name, 'v.mod')) or {
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
	os.execute_opt('${expect_exe} ${os.join_path(expect_tests_path, 'new_with_name_arg.expect')} ${vroot} ${project_name}') or {
		assert false, err.msg()
	}
	// Assert mod data set in `new_with_name_arg.expect`.
	mod := vmod.from_file(os.join_path(test_module_path, project_name, 'v.mod')) or {
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
	os.execute_opt('${expect_exe} ${os.join_path(expect_tests_path, 'new_with_model_arg.expect')} ${vroot} ${project_name} ${model}') or {
		assert false, err.msg()
	}
	// Assert mod data set in `new_with_model_arg.expect`.
	mod := vmod.from_file(os.join_path(test_module_path, project_name, 'v.mod')) or {
		assert false, err.str()
		return
	}
	assert mod.name == project_name
	assert mod.description == 'My Awesome V Project.'
	assert mod.version == '0.0.1'
	assert mod.license == 'MIT'
}
