import os

const source = 'vlib/flag/testdata/simplest_flag_program.v'

const simple_flag_app_executable = os.real_path(os.join_path(os.cache_dir(), 'simple_flag_app.exe'))

fn testsuite_begin() {
	os.chdir(@VMODROOT) or {}
	os.rm(simple_flag_app_executable) or {}
	res := os.execute('${os.quoted_path(@VEXE)} -o ${os.quoted_path(simple_flag_app_executable)} ${os.quoted_path(source)}')
	assert res.exit_code == 0
	assert os.execute(simple_flag_app_executable).exit_code == 0
}

fn testsuite_end() {
	os.rm(simple_flag_app_executable) or {}
	assert true
}

fn check_program(opts string, extension string) {
	result := source.replace('.v', extension)
	res := os.execute('${os.quoted_path(simple_flag_app_executable)} ${opts}')
	lines := os.read_lines(result) or { panic(err) }
	assert res.exit_code == 0
	assert res.output.split_into_lines() == lines
}

fn test_default_builtin_flag_options() {
	check_program('', '.out')
	check_program(' -- --help', '.dashdash.help.out')
	check_program(' -- --version', '.dashdash.version.out')
	check_program(' -h', '.help.out')
	check_program(' --help', '.help.out')
	check_program(' --version', '.version.out')
}
