import os
import v.cmdexec
import v.pref

// `v test` evaluates `// vtest build:` in cmd/tools/testing against the facts the
// launcher exports, while `v file_test.v` derives them in the compiler. Both must
// see the same host, compiler and defines; these fixtures pin the runner's side.

const facts_root = os.join_path(os.vtmp_dir(), 'vtest.build_facts.${os.getpid()}')
const facts_original_cwd = os.getwd()

fn write_facts_fixture(name string, constraint string, passing bool) {
	body := if passing {
		"import os\nfn test_probe() { os.write_file(@FILE + '.executed', 'passed')!; assert true }\n"
	} else {
		"fn test_probe() { assert false, 'a skipped fixture must not run' }\n"
	}
	os.write_file(os.join_path(facts_root, name), '// vtest build: ${constraint}\n\n' + body) or {
		panic(err)
	}
}

fn testsuite_begin() {
	os.rmdir_all(facts_root) or {}
	os.mkdir_all(facts_root)!
	// The nested `v test` must not rebuild the tool over the runner executing this
	// test; the cached tool binary is keyed by the prefix options and lives elsewhere.
	for name in ['VTEST_ONLY', 'VTEST_ONLY_FN', 'VTEST_RUNNER', 'VBUILD_FACTS', 'VBUILD_DEFINES',
		'VTOOLS_NO_CACHE'] {
		os.unsetenv(name)
	}
	os.setenv('VEXE', @VEXE, true)
	os.setenv('VJOBS', '1', true)
	os.setenv('VCOLORS', 'never', true)
	os.setenv('VTEST_HIDE_OK', '0', true)
	os.setenv('VTEST_HIDE_SKIP', '0', true)
	// One attempt: the runner compiles `VTEST_MAX_COMPILATION_RETRIES` times, so 0 would skip compiling.
	os.setenv('VTEST_MAX_COMPILATION_RETRIES', '1', true)
	os.setenv('V_C_ERROR_BUG_REPORT_DISABLED', '1', true)
	os.setenv('V_MACOS_V3_NO_FALLBACK', '1', true)
	host := pref.host_target()
	write_facts_fixture('host_os_test.v', host.os, true)
	write_facts_fixture('not_host_os_test.v', '!${host.os}', false)
	write_facts_fixture('host_arch_test.v', host.arch, true)
	write_facts_fixture('not_host_arch_test.v', '!${host.arch}', false)
	write_facts_fixture('define_test.v', 'build_facts_probe?', true)
	write_facts_fixture('not_define_test.v', '!build_facts_probe?', true)
	os.chdir(facts_root)!
}

fn testsuite_end() {
	os.chdir(facts_original_cwd) or { panic(err) }
	os.rmdir_all(facts_root) or {}
}

fn run_facts_session(prefix_args []string) os.Result {
	for marker in os.walk_ext(facts_root, '.executed') {
		os.rm(marker) or { panic(err) }
	}
	mut args := prefix_args.clone()
	args << ['test', facts_root]
	return cmdexec.run_with_timeout(@VEXE, args, 300_000)
}

fn assert_facts_summary(result os.Result, expected string, executed []string) {
	assert result.exit_code == 0, result.output
	summaries := result.output.split_into_lines().map(it.trim_space()).filter(it.starts_with('Summary for all V _test.v files:'))
	assert summaries.len == 1, result.output
	assert summaries[0].starts_with('Summary for all V _test.v files: ${expected}.'), result.output
	mut markers := os.walk_ext(facts_root, '.executed').map(os.file_name(it).all_before('.executed'))
	markers.sort()
	assert markers == executed, result.output
}

fn test_the_runner_sees_the_host_facts() {
	result := run_facts_session([])
	assert_facts_summary(result, '3 passed, 3 skipped, 6 total', ['host_arch_test.v', 'host_os_test.v',
		'not_define_test.v'])
}

fn test_the_runner_sees_the_user_defines() {
	result := run_facts_session(['-d', 'build_facts_probe'])
	assert_facts_summary(result, '3 passed, 3 skipped, 6 total', ['define_test.v', 'host_arch_test.v',
		'host_os_test.v'])
}
