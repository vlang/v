module main

import os

struct TestingScriptCase {
	name     string
	args     []string
	expected []string
	code     int
	reporter string
}

fn test_testing_script_rejects_empty_real_runner_selections() {
	root := os.join_path(os.vtmp_dir(), 'v testing script review ${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	suite := os.join_path(root, 'suite')
	empty := os.join_path(root, 'empty')
	os.mkdir_all(suite)!
	os.mkdir_all(empty)!
	os.write_file(os.join_path(suite, 'alpha_test.v'), 'module main\nimport os\nfn test_codex_one() {\n\tassert \$d("wrapper_proof", "missing") == "present"\n\tprintln("Summary for all V _test.v files: 0 total. test output")\n\tos.write_file(os.getenv("TESTING_SCRIPT_PROOF") + ".one", "ran")!\n\tassert true\n}\nfn test_codex_two() {\n\tos.write_file(os.getenv("TESTING_SCRIPT_PROOF") + ".two", "ran")!\n\tassert true\n}\n')!
	os.write_file(os.join_path(suite, 'beta_test.v'), 'module main\nimport os\nfn test_codex_three() {\n\tos.write_file(os.getenv("TESTING_SCRIPT_PROOF") + ".three", "ran")!\n\tassert true\n}\n')!
	// A .vsh target runs immediately; compile its source as .v to reuse the wrapper.
	source := os.read_file(os.join_path(@VEXEROOT, 'vlib', 'v', 'skills', 'v-testing',
		'scripts', 'run-tests.vsh'))!
	plain_source := os.join_path(root, 'run_tests.v')
	os.write_file(plain_source, source.all_after('\n'))!
	ext := $if windows { '.exe' } $else { '' }
	gate := os.join_path(root, 'run-tests' + ext)
	build := os.exec([@VEXE, '-gc', 'none', '-cc', @CCOMPILER, '-o', gate, plain_source])
	assert build.exit_code == 0, build.output
	cases := [
		TestingScriptCase{ name: 'all', args: [suite], expected: ['one', 'two', 'three'] },
		TestingScriptCase{ name: 'fn_match', args: [suite, '--fn', 'test_codex_one'], expected: ['one'] },
		TestingScriptCase{ name: 'fn_missing', args: [suite, '--fn', '__codex_missing*'], code: 1 },
		TestingScriptCase{
			name:     'file_match'
			args:     [suite, '--file', 'alpha_test']
			expected: ['one', 'two']
		},
		TestingScriptCase{ name: 'file_missing', args: [suite, '--file', '__codex_missing*'], code: 1 },
		TestingScriptCase{
			name:     'both_match'
			args:     [suite, '--fn', 'test_codex_one', '--file', 'alpha_test']
			expected: ['one']
		},
		TestingScriptCase{
			name: 'both_missing'
			args: [suite, '--fn', 'test_codex_one', '--file', 'beta_test']
			code: 1
		},
		TestingScriptCase{ name: 'stats_output', args: [suite, '--stats', '--fn', 'test_codex_one'], expected: ['one'] },
		TestingScriptCase{
			name: 'explicit_file'
			args: [os.join_path(suite, 'alpha_test.v'), '--fn', '__codex_missing*']
			code: 1
		},
		TestingScriptCase{ name: 'empty_directory', args: [empty], code: 1 },
		TestingScriptCase{
			name:     'multiple_targets'
			args:     [empty, suite]
			expected: ['one', 'two', 'three']
			code:     1
		},
		TestingScriptCase{ name: 'no_target', args: ['--fn', 'test_codex_one'], code: 2 },
		TestingScriptCase{
			name:     'dump_match'
			args:     [suite, '--file', 'alpha_test']
			expected: ['one', 'two']
			reporter: 'dump'
		},
		TestingScriptCase{ name: 'dump_empty', args: [suite, '--fn', '__codex_missing*'], code: 1, reporter: 'dump' },
		TestingScriptCase{ name: 'teamcity_match', args: [suite, '--fn', 'test_codex_one'], expected: ['one'], reporter: 'teamcity' },
		TestingScriptCase{ name: 'teamcity_empty', args: [suite, '--file', '__codex_missing*'], code: 1, reporter: 'teamcity' },
	]
	for item in cases {
		proof := os.join_path(root, item.name)
		mut environment := os.environ()
		environment['PATH'] = @VEXEROOT + os.path_delimiter + os.getenv('PATH')
		environment['VEXE'] = @VEXE
		// Runner summaries may contain ANSI color escapes even in captured output.
		environment['VCOLORS'] = 'always'
		// Strict test sessions remove VFLAGS before running test binaries.
		environment['VFLAGS'] = '-gc none -cc ${os.quoted_path(@CCOMPILER)} -d wrapper_proof=present'
		if item.reporter != '' {
			environment['VFLAGS'] += ' -test-runner ${item.reporter}'
		}
		environment['TESTING_SCRIPT_PROOF'] = proof
		mut process := os.new_process(gate)
		process.set_environment(environment)
		process.set_args(item.args)
		process.set_redirect_stdio_merged()
		process.set_stdin_path(os.path_devnull)
		process.run()
		output := process.stdout_slurp()
		process.wait()
		code := process.code
		process.close()
		assert code == item.code, '${item.name}: ${output}'
		mut executed := []string{}
		for name in ['one', 'two', 'three'] {
			if os.exists(proof + '.' + name) {
				executed << name
			}
		}
		assert executed == item.expected, '${item.name}: ${output}'
		if item.code == 0 {
			assert output.contains('run-tests.vsh: all targets passed'), output
		} else {
			assert !output.contains('run-tests.vsh: all targets passed'), output
			if item.code == 1 {
				assert output.contains('run-tests.vsh: no tests selected'), output
			} else {
				assert output.contains('at least one test path is required'), output
			}
		}
	}
}
