import os

const the_source = 'vlib/flag/testdata/usage_example.v'

const the_executable = os.real_path(os.join_path(os.cache_dir(), 'flag_usage_example_app.exe'))

fn testsuite_begin() {
	os.chdir(@VMODROOT) or {}
	os.rm(the_executable) or {}
	res := os.execute('${os.quoted_path(@VEXE)} -o ${os.quoted_path(the_executable)} ${os.quoted_path(the_source)}')
	assert res.exit_code == 0
	assert os.execute(os.quoted_path(the_executable)).exit_code == 0
	C.atexit(fn () {
		os.rm(the_executable) or {}
	})
}

fn normalise_lines(lines []string) string {
	return '\n' + lines.join('\n')
}

fn check_program(opts string, extension string) {
	result := the_source.replace('.v', extension)
	res := os.execute('${os.quoted_path(the_executable)} ${opts}')
	assert res.exit_code == 0
	assert normalise_lines(res.output.split_into_lines()) == normalise_lines(os.read_lines(result) or {
		panic(err)
	})
}

fn test_normal_usage() {
	check_program('abc def', '.out')
	check_program(' --help', '.help.out')
	check_program(' --version', '.version.out')
}
