import os
import v.cmdexec
import v.pref

const selection_root = os.join_path(os.vtmp_dir(), 'vtest.selection.${os.getpid()}')
const selection_tool = os.join_path(selection_root, 'vtest.exe')
const selection_original_cwd = os.getwd()
const selection_failure_marker = 'VTEST_EXPLICIT_SELECTION_EXECUTED'

fn write_selection_fixture(path string, content string) {
	full_path := os.join_path(selection_root, path)
	os.mkdir_all(os.dir(full_path)) or { panic(err) }
	os.write_file(full_path, content) or { panic(err) }
}

fn testsuite_begin() {
	os.rmdir_all(selection_root) or {}
	os.mkdir_all(selection_root)!
	// Isolate discovery from the outer runner's filters, without discarding the
	// C compiler or libc requested by VFLAGS in the CI lane.
	for name in ['VTEST_ONLY', 'VTEST_ONLY_FN', 'VTEST_RUNNER'] {
		os.unsetenv(name)
	}
	os.setenv('VEXE', @VEXE, true)
	os.setenv('VJOBS', '1', true)
	os.setenv('VCOLORS', 'never', true)
	os.setenv('VTEST_HIDE_OK', '0', true)
	os.setenv('VTEST_HIDE_SKIP', '0', true)
	os.setenv('VTEST_MAX_COMPILATION_RETRIES', '0', true)
	os.setenv('V_C_ERROR_BUG_REPORT_DISABLED', '1', true)
	os.setenv('V_MACOS_V3_NO_FALLBACK', '1', true)
	build := cmdexec.run_with_timeout(@VEXE, ['-new-compiler', '-nocache', '-gc', 'none',
		'-o', selection_tool, os.join_path(@VMODROOT, 'cmd', 'tools', 'vtest.v')], 180_000)
	assert build.exit_code == 0, build.output
	assert os.is_file(selection_tool)

	passing := "import os\nfn test_selected() { os.write_file(@FILE + '.executed', 'passed')!; assert true }\n"
	failing := "fn test_selected() { println('${selection_failure_marker}'); assert false, 'explicit test must run' }\n"
	for path in ['explicit/multiwindow_pass_test.v', 'explicit/multiwindow/pass_test.v',
		'explicit/multiwindow_pass_test.c.v', 'ordinary/pass_test.v',
		'discovery/pass_test.v', 'non_ci/multiwindow/pass_test.v'] {
		write_selection_fixture(path, passing)
	}
	for path in ['explicit/multiwindow_fail_test.v', 'explicit/multiwindow/fail_test.v',
		'discovery/multiwindow_fail_test.v', 'discovery/multiwindow/fail_test.v'] {
		write_selection_fixture(path, failing)
	}
	host_arch := pref.host_arch().str()
	other_arch := if pref.host_arch() == .amd64 { 'arm64' } else { 'amd64' }
	write_selection_fixture('directory space.with.dots/host_test.${host_arch}.v', passing)
	write_selection_fixture('incompatible/arch_test.${other_arch}.v', failing)
	write_selection_fixture('incompatible/backend_test.wasm.v', failing)
	write_selection_fixture('constraints/multiwindow_test.v',
		'// vtest build: selection_missing_define?\n' + failing)
	os.setenv('GITHUB_ACTIONS', 'true', true)
	os.chdir(selection_root)!
}

fn testsuite_end() {
	os.chdir(selection_original_cwd) or { panic(err) }
	os.rmdir_all(selection_root) or {}
}

fn run_selection(args []string) os.Result {
	for marker in os.walk_ext(selection_root, '.executed') {
		os.rm(marker) or { panic(err) }
	}
	mut all_args := ['-gc', 'none']
	all_args << args
	return cmdexec.run_with_timeout(selection_tool, all_args, 60_000)
}

fn assert_selection_summary(result os.Result, exit_code int, expected string) {
	assert result.exit_code == exit_code, result.output
	summaries := result.output.split_into_lines().map(it.trim_space()).filter(it.starts_with('Summary for all V _test.v files:'))
	assert summaries.len == 1, result.output
	assert summaries[0].starts_with('Summary for all V _test.v files: ${expected}.'), result.output
	// A successful compiler exit alone must not count as execution. Every passing
	// fixture writes its own marker from inside test_selected.
	mut expected_passes := 0
	for part in expected.split(', ') {
		if part.ends_with(' passed') {
			expected_passes = part.all_before(' ').int()
		}
	}
	markers := os.walk_ext(selection_root, '.executed')
	assert markers.len == expected_passes, '${result.output}\nExecuted fixtures: ${markers}'
}

fn test_explicit_multiwindow_files_are_not_quarantined_on_ci() {
	for path in ['explicit/multiwindow_pass_test.v', 'explicit/multiwindow/pass_test.v',
		'explicit/multiwindow_pass_test.c.v'] {
		result := run_selection(['test', path])
		assert_selection_summary(result, 0, '1 passed, 1 total')
	}
}

fn test_explicit_multiwindow_failures_are_not_reported_as_skips() {
	for path in ['explicit/multiwindow_fail_test.v', 'explicit/multiwindow/fail_test.v'] {
		for filtered in [false, true] {
			mut args := ['test']
			if filtered {
				args << ['-run-only', 'test_selected']
			}
			args << path
			result := run_selection(args)
			assert_selection_summary(result, 1, '1 failed, 1 total')
			assert result.output.split_into_lines().any(it.trim_space() == selection_failure_marker),
				result.output
		}
	}
}

fn test_multiwindow_define_does_not_skip_unrelated_tests() {
	for path in ['ordinary/pass_test.v', 'ordinary'] {
		result := run_selection(['-d', 'gg_multiwindow', 'test', path])
		assert_selection_summary(result, 0, '1 passed, 1 total')
	}
	old_vflags := os.getenv_opt('VFLAGS')
	defer {
		if value := old_vflags {
			os.setenv('VFLAGS', value, true)
		} else {
			os.unsetenv('VFLAGS')
		}
	}
	os.setenv('VFLAGS', os.getenv('VFLAGS') + ' -d gg_multiwindow', true)
	for path in ['ordinary', 'explicit/multiwindow_pass_test.v'] {
		result := run_selection(['test', path])
		assert_selection_summary(result, 0, '1 passed, 1 total')
	}
}

fn test_recursive_ci_discovery_keeps_the_multiwindow_quarantine() {
	result := run_selection(['test', 'discovery'])
	assert_selection_summary(result, 0, '1 passed, 2 skipped, 3 total')
	assert !result.output.split_into_lines().any(it.trim_space() == selection_failure_marker),
		result.output
}

fn test_explicit_and_discovered_paths_keep_independent_selection() {
	result := run_selection(['test', 'discovery', 'explicit/multiwindow_pass_test.v'])
	assert_selection_summary(result, 0, '2 passed, 2 skipped, 4 total')
}

fn test_non_ci_directory_discovery_is_unchanged() {
	os.setenv('GITHUB_ACTIONS', 'false', true)
	defer {
		os.setenv('GITHUB_ACTIONS', 'true', true)
	}
	result := run_selection(['test', 'non_ci'])
	assert_selection_summary(result, 0, '1 passed, 1 total')
}

fn test_architecture_suffix_ignores_dots_in_the_parent_path() {
	host_arch := pref.host_arch().str()
	for path in ['directory space.with.dots/host_test.${host_arch}.v',
		'./directory space.with.dots/host_test.${host_arch}.v',
		os.join_path(selection_root, 'directory space.with.dots', 'host_test.${host_arch}.v'),
		'./directory space.with.dots'] {
		result := run_selection(['test', path])
		assert_selection_summary(result, 0, '1 passed, 1 total')
	}
}

fn test_architecture_suffix_is_discovered_from_the_current_directory() {
	os.chdir(os.join_path(selection_root, 'directory space.with.dots'))!
	defer {
		os.chdir(selection_root) or { panic(err) }
	}
	result := run_selection(['test', '.'])
	assert_selection_summary(result, 0, '1 passed, 1 total')
}

fn test_explicit_incompatible_architecture_and_backend_still_skip() {
	other_arch := if pref.host_arch() == .amd64 { 'arm64' } else { 'amd64' }
	for path in ['incompatible/arch_test.${other_arch}.v', 'incompatible/backend_test.wasm.v'] {
		result := run_selection(['test', os.join_path(selection_root, path)])
		assert_selection_summary(result, 0, '1 skipped, 1 total')
	}
}

fn test_explicit_multiwindow_build_constraints_still_apply() {
	result := run_selection(['test', 'constraints/multiwindow_test.v'])
	assert_selection_summary(result, 0, '1 skipped, 1 total')
	assert !result.output.split_into_lines().any(it.trim_space() == selection_failure_marker),
		result.output
}
