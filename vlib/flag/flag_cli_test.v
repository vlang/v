import os

const cli_test_dir = os.join_path(os.abs_path(os.vtmp_dir()), 'v_flag_cli_test_${os.getpid()}')

fn testsuite_begin() {
	os.chdir(@VMODROOT) or { panic(err) }
	os.mkdir(cli_test_dir) or { panic(err) }
}

fn testsuite_end() {
	os.rmdir_all(cli_test_dir) or {}
}

fn compile_program(source string, name string) string {
	executable := os.join_path(cli_test_dir, name)
	res := os.exec([@VEXE, '-o', executable, source])
	assert res.exit_code == 0, res.output
	return executable
}

fn check_program(executable string, expected string, args ...string) {
	res := os.exec([executable, ...args])
	lines := os.read_lines(expected) or { panic(err) }
	assert res.exit_code == 0, res.output
	assert res.output.split_into_lines() == lines
}

fn test_default_builtin_flag_options() {
	source := 'vlib/flag/testdata/simplest_flag_program'
	executable := compile_program('${source}.v', 'simple_flag_app.exe')
	check_program(executable, '${source}.out')
	check_program(executable, '${source}.dashdash.help.out', '--', '--help')
	check_program(executable, '${source}.dashdash.version.out', '--', '--version')
	check_program(executable, '${source}.help.out', '-h')
	check_program(executable, '${source}.help.out', '--help')
	check_program(executable, '${source}.version.out', '--version')
}

fn test_normal_usage() {
	source := 'vlib/flag/testdata/usage_example'
	executable := compile_program('${source}.v', 'flag_usage_example_app.exe')
	res := os.exec([executable])
	assert res.exit_code == 0, res.output
	check_program(executable, '${source}.out', 'abc', 'def')
	check_program(executable, '${source}.help.out', '--help')
	check_program(executable, '${source}.version.out', '--version')
}
