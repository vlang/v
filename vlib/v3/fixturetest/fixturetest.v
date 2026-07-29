module fixturetest

import os
import v3.cmdexec

const max_reported_mismatches = 20
const max_parallel_fixtures = 1
const diagnostic_fixture_suffixes = ['/vlib/v/checker/tests', '/vlib/v/parser/tests',
	'/vlib/v/scanner/tests']
// Keep these exclusions aligned with v/compiler_errors_test.v.
const diagnostic_fixture_skip_paths = ['non_existing.vv',
	'vlib/v/checker/tests/var_duplicate_const.vv']
const diagnostic_fixture_specialized_paths = [
	'vlib/v/checker/tests/index_expr_implicit_int_downcast_err.vv',
	'vlib/v/checker/tests/js_number_requires_explicit_cast.vv',
	'vlib/v/checker/tests/disable_explicit_mutability.vv',
]
const diagnostic_fixture_skip_missing_headers = [
	'vlib/v/checker/tests/missing_c_lib_header_1.vv',
	'vlib/v/checker/tests/missing_c_lib_header_with_explanation_2.vv',
	'vlib/v/checker/tests/comptime_value_d_in_include_errors.vv',
	'vlib/v/checker/tests/missing_shader_header_1.vv',
]
const diagnostic_fixture_skip_on_ubuntu_musl = [
	'vlib/v/checker/tests/orm_op_with_option_and_none.vv',
	'vlib/v/checker/tests/orm_unused_var.vv',
	'vlib/v/tests/skip_unused/gg_code.vv',
]
const diagnostic_fixture_skip_on_ci_musl = ['vlib/v/tests/skip_unused/gg_code.vv']
const diagnostic_fixture_skip_on_msvc = [
	'vlib/v/checker/tests/asm_alias_does_not_exist.vv',
	'vlib/v/checker/tests/asm_immutable_err.vv',
]
const diagnostic_fixture_skip_on_windows = [
	'vlib/v/checker/tests/invalid_utf8_string.vv',
	'vlib/v/checker/tests/modules/deprecated_module',
]

struct FixtureResult {
	index     int
	exit_code int
}

// is_diagnostic_fixture_dir reports whether path is a known diagnostic `.vv`/`.out` suite.
pub fn is_diagnostic_fixture_dir(path string) bool {
	if !os.is_dir(path) {
		return false
	}
	normalized := os.real_path(path).replace('\\', '/').trim_right('/')
	if !diagnostic_fixture_suffixes.any(normalized.ends_with(it)) {
		return false
	}
	files := os.ls(path) or { return false }
	if files.any(is_standard_test_file(it)) {
		return false
	}
	return files.any(is_comparable_fixture(path, it))
}

fn is_standard_test_file(file string) bool {
	if file.contains('_d_test.') || file.contains('_notd_test.') {
		return false
	}
	if file.ends_with('_test.v') {
		return true
	}
	if !file.ends_with('.v') {
		return false
	}
	base := file[..file.len - 2]
	return base.contains('.') && base.all_before_last('.').ends_with('_test')
}

fn is_comparable_fixture(dir string, name string) bool {
	if !name.ends_with('.vv') || !os.is_file(os.join_path(dir, name))
		|| is_excluded_diagnostic_fixture(dir, name) {
		return false
	}
	return autofix_enabled() || os.is_file(os.join_path(dir, name.all_before_last('.vv') + '.out'))
}

fn autofix_enabled() bool {
	return os.getenv('VAUTOFIX') != ''
}

fn is_excluded_diagnostic_fixture(dir string, name string) bool {
	normalized := os.real_path(os.join_path(dir, name)).replace('\\', '/')
	for excluded in excluded_diagnostic_fixture_paths() {
		if (!excluded.contains('/') && name == excluded)
			|| normalized.ends_with('/${excluded.replace('\\', '/')}') {
			return true
		}
	}
	return false
}

fn excluded_diagnostic_fixture_paths() []string {
	mut excluded := diagnostic_fixture_skip_paths.clone()
	if os.getenv('V_CI_UBUNTU_MUSL').len > 0 {
		excluded << diagnostic_fixture_skip_on_ubuntu_musl
	}
	if os.getenv('V_CI_MUSL').len > 0 {
		excluded << diagnostic_fixture_skip_on_ci_musl
	}
	if os.getenv('V_CI_CSTRICT').len > 0 {
		excluded << diagnostic_fixture_skip_missing_headers
	}
	$if noskip ? {
		excluded = []
	}
	$if tinyc {
		excluded << diagnostic_fixture_skip_missing_headers
	}
	$if msvc {
		excluded << diagnostic_fixture_skip_on_msvc
		excluded << diagnostic_fixture_skip_missing_headers
	}
	$if windows {
		excluded << diagnostic_fixture_skip_on_windows
	}
	// These have dedicated JS or explicit-mutability runs and are never part of the
	// default adjacent-.out sweep, including under `-d noskip`.
	excluded << diagnostic_fixture_specialized_paths
	return excluded
}

// run compares every `.vv` compiler invocation with its adjacent `.out` file.
pub fn run(vexe string, dir string, invocation_args []string) int {
	fixture_dir := os.real_path(dir)
	repo_root := fixture_repo_root(vexe, fixture_dir)
	compiler_options := forwarded_compiler_options(invocation_args)
	mut names := os.ls(dir) or {
		eprintln('failed to list fixture directory `${dir}`: ${err}')
		return 1
	}
	names = names.filter(is_comparable_fixture(dir, it))
	names.sort()
	filter := os.getenv('VTEST_ONLY')
	if filter.len > 0 {
		filters := filter.split(',')
		names = names.filter(name_matches_filters(it, os.join_path(dir, it), filters))
	}
	start_at := os.getenv('VTEST_START_AT').int()
	if start_at >= names.len && names.len > 0 {
		eprintln('VTEST_START_AT ${start_at} is outside the ${names.len} matching fixtures')
		return 1
	}
	if start_at > 0 {
		names = names[start_at..].clone()
	}
	if names.len == 0 {
		eprintln('no comparable `.vv`/`.out` fixtures found in `${dir}`')
		return 1
	}
	os.setenv('VCOLORS', 'never', true)
	os.setenv('VTEST_RUNNER', 'normal', true)
	mut paths := []string{cap: names.len}
	mut expected_paths := []string{cap: names.len}
	mut expected_outputs := []string{cap: names.len}
	mut expected_exit_codes := []int{cap: names.len}
	for name in names {
		absolute_path := os.join_path(fixture_dir, name)
		path := repo_relative_path(repo_root, absolute_path)
		expected_path := absolute_path.all_before_last('.vv') + '.out'
		if autofix_enabled() && !os.exists(expected_path) {
			os.write_file(expected_path, '') or {
				eprintln('failed to create `${expected_path}`: ${err}')
				return 1
			}
		}
		expected := os.read_file(expected_path) or {
			eprintln('failed to read `${expected_path}`: ${err}')
			return 1
		}
		paths << path
		expected_paths << expected_path
		cleaned_expected := clean_output(expected)
		expected_outputs << cleaned_expected
		expected_exit_codes << expected_fixture_exit_code(cleaned_expected)
	}
	max_failures := os.getenv('VTEST_MAX_FAILURES').int()
	mut failures := 0
	mut abnormal_exits := 0
	mut abnormal_paths := []string{}
	mut completed := 0
	mut batch_start := 0
	for batch_start < paths.len {
		batch_end := int_min(batch_start + max_parallel_fixtures, paths.len)
		mut threads := []thread FixtureResult{cap: batch_end - batch_start}
		for index in batch_start .. batch_end {
			threads << spawn run_fixture(vexe, repo_root, paths[index], index, compiler_options)
		}
		for result in threads.wait() {
			index := result.index
			path := paths[index]
			result_path := fixture_output_path(index)
			found_raw := os.read_file(result_path) or { '' }
			os.rm(result_path) or {}
			found := clean_output(found_raw)
			exit_code := result.exit_code
			abnormal := exit_code !in [0, 1]
			completed++
			if fixture_result_matches(expected_outputs[index], found, expected_exit_codes[index], exit_code)
				&& !abnormal {
				continue
			}
			// Like compiler_errors_test, rewrite now but keep the first pass failed so the
			// changed expectations can be reviewed before the confirming second pass.
			if autofix_enabled() && expected_outputs[index] != found {
				os.write_file(expected_paths[index], found_raw) or {
					eprintln('failed to update `${expected_paths[index]}`: ${err}')
				}
			}
			failures++
			if abnormal {
				abnormal_exits++
				abnormal_paths << '${path} (${exit_code})'
			}
			eprintln('FAIL ${path} (exit ${exit_code}, expected ${expected_exit_codes[index]})')
			if failures <= max_reported_mismatches {
				eprintln('--- expected')
				eprintln(expected_outputs[index])
				eprintln('--- found')
				eprintln(found)
				eprintln('---')
			}
		}
		batch_start = batch_end
		if max_failures > 0 && failures >= max_failures {
			break
		}
	}
	passed := completed - failures
	not_run := paths.len - completed
	not_run_suffix := if not_run > 0 { ', ${not_run} not run' } else { '' }
	println('checker fixtures: ${passed}/${completed} passed, ${failures} failed, ${abnormal_exits} abnormal exits${not_run_suffix}')
	if abnormal_paths.len > 0 {
		eprintln('abnormal fixture exits: ${abnormal_paths.join(', ')}')
	}
	if failures > max_reported_mismatches {
		eprintln('${failures - max_reported_mismatches} additional mismatch details were not shown')
	}
	return if failures == 0 { 0 } else { 1 }
}

fn fixture_repo_root(vexe string, fixture_dir string) string {
	return find_repo_root(os.dir(os.real_path(vexe))) or {
		find_repo_root(fixture_dir) or { os.getwd() }
	}
}

fn name_matches_filters(name string, path string, filters []string) bool {
	return filters.any(name.contains(it) || path.contains(it))
}

fn forwarded_compiler_options(args []string) []string {
	value_options := ['-o', '-b', '-os', '-arch', '-compile-backend', '--compile-backend', '-d',
		'-gc', '-cc', '-cflags', '-thread-stack-size']
	runner_options := ['-silent', '-no-parallel', '--no-parallel', '-nocache', '--no-cache',
		'-checker-fixture']
	mut options := []string{cap: args.len}
	mut i := 0
	for i < args.len {
		arg := args[i]
		if arg in value_options {
			if i + 1 >= args.len {
				break
			}
			if arg != '-o' {
				options << arg
				options << args[i + 1]
			}
			i += 2
			continue
		}
		if arg == 'test' || arg in runner_options || !arg.starts_with('-') {
			i++
			continue
		}
		options << arg
		i++
	}
	return options
}

fn run_fixture(vexe string, repo_root string, path string, index int, compiler_options []string) FixtureResult {
	output_base := fixture_binary_path(index)
	mut args := compiler_options.clone()
	args << ['-silent', '-no-parallel', '-nocache', '-checker-fixture', '-o', output_base, path]
	result := cmdexec.run_in_merged(vexe, args, repo_root)
	os.rm(output_base) or {}
	os.rm(output_base + '.c') or {}
	os.write_file(fixture_output_path(index), result.output) or {
		return FixtureResult{
			index:     index
			exit_code: -1
		}
	}
	return FixtureResult{
		index:     index
		exit_code: result.exit_code
	}
}

fn find_repo_root(start string) ?string {
	mut current := start
	for _ in 0 .. 8 {
		if os.is_dir(os.join_path(current, 'vlib', 'v3'))
			&& os.is_dir(os.join_path(current, 'vlib', 'v', 'checker', 'tests')) {
			return current
		}
		parent := os.dir(current)
		if parent == current {
			break
		}
		current = parent
	}
	return none
}

fn repo_relative_path(repo_root string, path string) string {
	root_prefix := repo_root.replace('\\', '/').trim_right('/') + '/'
	normalized := path.replace('\\', '/')
	if normalized.starts_with(root_prefix) {
		return normalized[root_prefix.len..]
	}
	return path
}

fn fixture_binary_path(index int) string {
	return os.join_path(os.vtmp_dir(), 'v3_checker_fixture_${os.getpid()}_${index}')
}

fn fixture_output_path(index int) string {
	return fixture_binary_path(index) + '.fixture.out'
}

fn clean_output(input string) string {
	mut output := input.trim_space()
	output = output.replace(' \r\n', '\n')
	output = output.replace(' \n', '\n')
	output = output.replace('\r\n', '\n')
	return output.trim('\n')
}

fn expected_fixture_exit_code(output string) int {
	for line in output.split_into_lines() {
		trimmed := line.trim_space()
		if is_fixture_source_gutter(trimmed) {
			continue
		}
		for severity in ['error:', 'builder error:', 'cgen error:'] {
			if trimmed.starts_with(severity) || trimmed.contains(': ${severity}') {
				return 1
			}
		}
	}
	return 0
}

fn is_fixture_source_gutter(line string) bool {
	pipe_index := line.index('|') or { return false }
	mut prefix := line[..pipe_index].trim_space()
	if prefix.starts_with('>') {
		prefix = prefix[1..].trim_space()
	}
	return prefix.len == 0 || prefix.bytes().all(it.is_digit())
}

fn fixture_result_matches(expected_output string, found_output string, expected_exit_code int, actual_exit_code int) bool {
	return expected_output == found_output && expected_exit_code == actual_exit_code
}
