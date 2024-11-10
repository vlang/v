import v.util.diff
import os
import term

const tdir = os.join_path(os.vtmp_dir(), 'diff_test')

fn testsuite_begin() {
	if diff.DiffTool.diff !in diff.available_tools() {
		// On GitHub runners, diff should be available on all platforms.
		// Prevent regressions by failing instead of skipping when it's not detected.
		if os.getenv('CI') == 'true' {
			exit(1)
		}
		eprintln('> skipping test `${@FILE}`, since this test requires `diff` to be installed')
		exit(0)
	}
	os.mkdir_all(tdir)!
	// Disable environmental overwrites that can result in different compare outputs.
	os.setenv('VDIFF_CMD', '', true)
}

fn testsuite_end() {
	os.rmdir_all(tdir) or {}
}

fn test_compare_files() {
	f1 := "Module{
	name: 'Foo'
	description: 'Awesome V module.'
	version: '0.0.0'
	dependencies: []
}
"
	f2 := "Module{
	name: 'foo'
	description: 'Awesome V module.'
	version: '0.1.0'
	license: 'MIT'
	dependencies: []
}
"
	p1 := os.join_path(tdir, '${@FN}_f1.txt')
	p2 := os.join_path(tdir, '${@FN}_f2.txt')
	os.write_file(p1, f1)!
	os.write_file(p2, f2)!

	// Test comparison without specifying a cmd only loosely, since an automatically detected tool
	// or can result in a different compare output.
	mut res := term.strip_ansi(diff.compare_files(p1, p2)!)
	assert res.contains("name: 'Foo'"), res
	assert res.contains("name: 'foo'"), res

	// From here on, pass `.diff` via the arg or environment variable to enforce consistent behavior in regular tests.
	res = diff.compare_files(p1, p2, tool: .diff)!
	assert res.contains("-\tname: 'Foo'"), res
	assert res.contains("+\tname: 'foo'"), res
	assert res.contains("-\tversion: '0.0.0'"), res
	assert res.contains("+\tversion: '0.1.0'"), res
	assert res.contains("+\tlicense: 'MIT'"), res
	assert res == diff.color_compare_files('diff', p1, p2)
	// Test again using `find_working_diff_command()`.
	zzz := diff.color_compare_files(diff.find_working_diff_command()!, p1, p2)
	assert term.strip_ansi(res) == term.strip_ansi(zzz)

	// Test custom options.
	res = diff.compare_files(p1, p2, tool: .diff, args: '-U 2 -i')!
	assert !res.contains("+\tname: 'foo'"), res
	assert res.contains("-\tversion: '0.0.0'"), res
	assert res.contains("+\tversion: '0.1.0'"), res
	assert res.contains("+\tlicense: 'MIT'"), res
	assert res == term.strip_ansi(diff.color_compare_files('diff --ignore-case', p1, p2))

	// Test options via env variable.
	os.setenv('VDIFF_CMD', 'diff --ignore-case -U 2', true)
	defer {
		os.setenv('VDIFF_CMD', '', true)
	}
	res = diff.compare_files(p1, p2)!
	assert !res.contains("+\tname: 'foo'"), res
	assert res.contains("-\tversion: '0.0.0'"), res
	assert res.contains("+\tversion: '0.1.0'"), res
	assert res.contains("+\tlicense: 'MIT'"), res
	os.setenv('VDIFF_TOOL', 'diff', true)
	os.setenv('VDIFF_OPTIONS', '--ignore-case', true)
	assert res == term.strip_ansi(diff.color_compare_files(diff.find_working_diff_command()!,
		p1, p2))

	// Test custom option that interferes with default options.
	res = diff.compare_files(p1, p2, tool: .diff, args: '--side-by-side', env_overwrite_var: none)!
	assert res.match_glob("*version: '0.0.0'*|*version: '0.1.0'*"), res

	// Test custom diff command.
	// Test windows default `fc`.
	/* $if windows { // TODO: enable when its `os.execute` output can be read.
		res = diff.compare_files(p1, p1, tool: .fc)!
		assert res.contains('FC: no differences encountered')
		res = diff.compare_files(p1, p2, tool: .fc, args: '/b')!
		assert res.contains('FC: ABCD longer than abc')
	} */
}

fn test_compare_string() {
	mut res := diff.compare_text('abc', 'abcd', tool: .diff)!
	assert res.contains('-abc'), res
	assert res.contains('+abcd'), res
	assert !res.contains('No newline at end of file'), res
	// Default base and target name.
	assert res.match_glob('*---*base*'), res
	assert res.match_glob('*+++*target*'), res
	// Custom base and target name.
	res = diff.compare_text('abc', 'abcd', tool: .diff, base_name: 'old.v', target_name: 'new.v')!
	assert res.match_glob('*---*old.v*'), res
	assert res.match_glob('*+++*new.v*'), res
}

fn test_coloring() {
	if os.execute('diff --color=always').output.starts_with('diff: unrecognized option') {
		eprintln('> skipping test `${@FN}`, since `diff` does not support --color=always')
		return
	}
	f1 := 'abc\n'
	f2 := 'abcd\n'
	p1 := os.join_path(tdir, '${@FN}_f1.txt')
	p2 := os.join_path(tdir, '${@FN}_f2.txt')
	os.write_file(p1, f1)!
	os.write_file(p2, f2)!
	esc := rune(27)
	res := diff.compare_files(p1, p2, tool: .diff)!
	assert res.contains('${esc}[31m-abc${esc}['), res
	assert res.contains('${esc}[32m+abcd${esc}['), res
	assert res == diff.color_compare_files('diff', p1, p2)
}
