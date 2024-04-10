import v.util.diff
import os

fn test_compare_files() {
	os.find_abs_path_of_executable('diff') or {
		eprintln('> skipping test, since this test requires `diff` to be installed.')
		return
	}

	f1 := "Module{
	name: 'Foo'
	description: 'Awesome V module.'
	version: '0.0.0'
	dependencies: []
}
"
	f2 := "Module{
	name: 'Foo'
	description: 'Awesome V module.'
	version: '0.1.0'
	license: 'MIT'
	dependencies: []
}
"
	tdir := os.join_path(os.vtmp_dir(), 'diff_test')
	os.mkdir_all(tdir)!
	defer {
		os.rmdir_all(tdir) or {}
	}
	p1 := os.join_path(tdir, 'f1.txt')
	p2 := os.join_path(tdir, 'f2.txt')
	os.write_file(p1, f1)!
	os.write_file(p2, f2)!

	mut res := diff.color_compare_files('diff', p1, p2)
	assert res.contains("-\tversion: '0.0.0'"), res
	assert res.contains("+\tversion: '0.1.0'"), res
	assert res.contains("+\tlicense: 'MIT'"), res

	// Test adding a flag to the command.
	res = diff.color_compare_files('diff --expand-tabs', p1, p2)
	assert res.contains("- version: '0.0.0'"), res
	assert res.contains("+ version: '0.1.0'"), res
	assert res.contains("+ license: 'MIT'"), res

	// Test again using `find_working_diff_command()`.
	res = diff.color_compare_files(diff.find_working_diff_command()!, p1, p2)
	assert res.contains("-\tversion: '0.0.0'"), res
	assert res.contains("+\tversion: '0.1.0'"), res
	assert res.contains("+\tlicense: 'MIT'"), res

	// Test adding a flag via env flag.
	os.setenv('VDIFF_OPTIONS', '--expand-tabs', true)
	res = diff.color_compare_files(diff.find_working_diff_command()!, p1, p2)
	assert res.contains("- version: '0.0.0'"), res
	assert res.contains("+ version: '0.1.0'"), res
	assert res.contains("+ license: 'MIT'"), res
}
