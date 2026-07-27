module fixturetest

import os
import v3.cmdexec

const max_reported_mismatches = 20
const max_parallel_fixtures = 1

struct FixtureResult {
	index     int
	exit_code int
}

// is_fixture_dir reports whether path contains v1-style `.vv`/`.out` fixtures.
pub fn is_fixture_dir(path string) bool {
	if !os.is_dir(path) {
		return false
	}
	files := os.ls(path) or { return false }
	return files.any(it.ends_with('.vv')
		&& os.is_file(os.join_path(path, it.all_before_last('.vv') + '.out')))
}

// run compares every `.vv` compiler invocation with its adjacent `.out` file.
pub fn run(vexe string, dir string) int {
	repo_root := find_repo_root(os.dir(os.real_path(vexe))) or { os.getwd() }
	fixture_dir := os.real_path(dir)
	mut names := os.ls(dir) or {
		eprintln('failed to list fixture directory `${dir}`: ${err}')
		return 1
	}
	names = names.filter(it.ends_with('.vv')
		&& os.is_file(os.join_path(dir, it.all_before_last('.vv') + '.out')))
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
	mut expected_outputs := []string{cap: names.len}
	for name in names {
		absolute_path := os.join_path(fixture_dir, name)
		path := repo_relative_path(repo_root, absolute_path)
		expected_path := absolute_path.all_before_last('.vv') + '.out'
		expected := os.read_file(expected_path) or {
			eprintln('failed to read `${expected_path}`: ${err}')
			return 1
		}
		paths << path
		expected_outputs << clean_output(expected)
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
			threads << spawn run_fixture(vexe, repo_root, paths[index], index)
		}
		for result in threads.wait() {
			index := result.index
			path := paths[index]
			result_path := fixture_output_path(index)
			found_raw := os.read_file(result_path) or { '' }
			os.rm(result_path) or {}
			found := clean_output(found_raw)
			exit_code := result.exit_code
			mismatch := expected_outputs[index] != found
			abnormal := exit_code !in [0, 1]
			completed++
			if !mismatch && !abnormal {
				continue
			}
			failures++
			if abnormal {
				abnormal_exits++
				abnormal_paths << '${path} (${exit_code})'
			}
			eprintln('FAIL ${path} (exit ${exit_code})')
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

fn name_matches_filters(name string, path string, filters []string) bool {
	return filters.any(name.contains(it) || path.contains(it))
}

fn run_fixture(vexe string, repo_root string, path string, index int) FixtureResult {
	output_base := fixture_binary_path(index)
	result := cmdexec.run_in(vexe, ['-silent', '-no-parallel', '-nocache', '-checker-fixture',
		'-o', output_base, path], repo_root)
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
