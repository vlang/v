import v.util.diff
import os

const tdir = os.join_path(os.vtmp_dir(), 'diff_test')

fn testsuite_begin() {
	os.find_abs_path_of_executable('diff') or {
		eprintln('> skipping test, since this test requires `diff` to be installed')
		exit(0)
	}
	os.mkdir_all(tdir)!
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

	mut res := diff.color_compare_files('diff', p1, p2)
	assert res.contains("-\tname: 'Foo'"), res
	assert res.contains("+\tname: 'foo'"), res
	assert res.contains("-\tversion: '0.0.0'"), res
	assert res.contains("+\tversion: '0.1.0'"), res
	assert res.contains("+\tlicense: 'MIT'"), res

	// Test adding a flag to the command.
	res = diff.color_compare_files('diff --ignore-case', p1, p2)
	assert !res.contains("+\tname: 'foo'"), res
	assert res.contains("-\tversion: '0.0.0'"), res
	assert res.contains("+\tversion: '0.1.0'"), res
	assert res.contains("+\tlicense: 'MIT'"), res

	// Test again using `find_working_diff_command()`.
	res = diff.color_compare_files(diff.find_working_diff_command()!, p1, p2)
	assert res.contains("-\tversion: '0.0.0'"), res
	assert res.contains("+\tversion: '0.1.0'"), res
	assert res.contains("+\tlicense: 'MIT'"), res

	// Test adding a flag via env flag.
	os.setenv('VDIFF_OPTIONS', '--ignore-case', true)
	res = diff.color_compare_files(diff.find_working_diff_command()!, p1, p2)
	assert !res.contains("+\tname: 'foo'"), res
	assert res.contains("-\tversion: '0.0.0'"), res
	assert res.contains("+\tversion: '0.1.0'"), res
	assert res.contains("+\tlicense: 'MIT'"), res

	// Test coloring
	if !os.execute('diff --color=always').output.contains('--color=always') {
		// If the flag is unknown, it will be reprinted in the output.
		assert res.contains("[31m-\tversion: '0.0.0'[m"), res
		assert res.contains("[32m+\tversion: '0.1.0'[m"), res
	}
}
