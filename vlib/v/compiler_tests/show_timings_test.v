import os
import time
import v.cmdexec

fn run_show_timings_compiler(args []string) os.Result {
	mut compiler_args := ['-new-compiler', '-no-retry-compilation', '-cc', 'clang', '-nocolor']
	compiler_args << args
	return cmdexec.run(@VEXE, compiler_args)
}

fn assert_compiler_timings(result os.Result, stages []string) {
	assert result.exit_code == 0, result.output
	assert result.output.contains('=== V compiler benchmark ==='), result.output
	lines := result.output.split_into_lines().filter(it.starts_with('  ') && it.contains(' ms'))
	for stage in stages {
		assert lines.any(it.trim_space().starts_with('${stage} ')), result.output
	}
	assert lines.any(it.trim_space().starts_with('total ')), result.output
	assert !result.output.contains('[ttime]'), result.output
	assert !result.output.contains('v.pref.lookup_path:'), result.output
	assert !result.output.contains('  > '), result.output
}

fn assert_binary_compiler_timings(result os.Result) {
	assert_compiler_timings(result, ['parse .v', 'check', 'cgen'])
	lines := result.output.split_into_lines().filter(it.starts_with('  ') && it.contains(' ms'))
	assert lines.any(it.trim_space().starts_with('tcc ') || it.trim_space().starts_with('cc ')), result.output
}

fn test_show_timings_reports_stages_without_verbose_output() {
	root := os.join_path(os.vtmp_dir(), 'show_timings_${os.getpid()}_${time.now().unix_nano()}')
	os.mkdir_all(root)!
	previous_environment := os.environ()
	os.setenv('V3CACHE', os.join_path(root, 'cache'), true)
	os.setenv('VFLAGS', '', true)
	defer {
		for name in ['V3CACHE', 'VFLAGS'] {
			if value := previous_environment[name] {
				os.setenv(name, value, true)
			} else {
				os.unsetenv(name)
			}
		}
		os.rmdir_all(root) or {}
	}
	source := os.join_path(root, 'hello.v')
	os.write_file(source, "import os\nfn main() {\n\tprintln('hello timings')\n\tif os.args.len > 1 {\n\t\tprintln(os.args[1..].join('|'))\n\t}\n}\n")!
	output := os.join_path(root, 'hello')

	plain := run_show_timings_compiler(['-nocache', '-o', output, source])
	assert plain.exit_code == 0, plain.output
	assert plain.output == '', plain.output

	// Exercise both source parsing and cache reuse with the flag on its own.
	cold := run_show_timings_compiler(['-show-timings', '-o', output, source])
	assert_binary_compiler_timings(cold)
	assert !cold.output.contains('hello timings'), cold.output
	warm := run_show_timings_compiler(['-show-timings', '-o', output, source])
	assert_binary_compiler_timings(warm)
	nocache := run_show_timings_compiler(['-show-timings', '-nocache', '-o', output, source])
	assert_binary_compiler_timings(nocache)

	c_output := os.join_path(root, 'hello.c')
	c_only := run_show_timings_compiler(['-show-timings', '-o', c_output, source])
	assert_compiler_timings(c_only, ['parse .v', 'check', 'cgen'])
	assert os.is_file(c_output)
	c_source := os.read_file(c_output)!
	assert c_source.contains('int main('), c_source

	for flags in [['-show-timings', '-silent'], ['-silent', '-show-timings']] {
		mut args := flags.clone()
		args << ['-o', output, source]
		silent := run_show_timings_compiler(args)
		assert silent.exit_code == 0, silent.output
		assert silent.output == '', silent.output
	}

	stdout_c := run_show_timings_compiler(['-show-timings', '-o', '-', source])
	assert stdout_c.exit_code == 0, stdout_c.output
	assert stdout_c.output.contains('int main('), stdout_c.output
	assert !stdout_c.output.contains('=== V compiler benchmark ==='), stdout_c.output
	assert !stdout_c.output.contains('[ttime]'), stdout_c.output
	plain_stdout_c := run_show_timings_compiler(['-o', '-', source])
	assert plain_stdout_c.exit_code == 0, plain_stdout_c.output
	assert stdout_c.output == plain_stdout_c.output, 'timings changed generated C stdout'

	checked := run_show_timings_compiler(['-show-timings', '-check', source])
	assert_compiler_timings(checked, ['parse .v', 'check'])
	syntax := run_show_timings_compiler(['-show-timings', '-check-syntax', source])
	assert_compiler_timings(syntax, ['parse'])

	run := run_show_timings_compiler(['-show-timings', 'run', source, '-show-timings'])
	assert_compiler_timings(run, ['parse .v', 'cgen', 'run'])
	assert 'hello timings' in run.output.split_into_lines(), run.output
	assert '-show-timings' in run.output.split_into_lines(), run.output

	// A flag after the run target belongs to the program and must not enable timings.
	forwarded := run_show_timings_compiler(['run', source, '-show-timings'])
	assert forwarded.exit_code == 0, forwarded.output
	assert forwarded.output.trim_space() == 'hello timings\n-show-timings', forwarded.output
}
