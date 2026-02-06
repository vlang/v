import v.util.diff
import os
import term

const tdir = os.join_path(os.vtmp_dir(), 'diff_test')

fn testsuite_begin() {
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

	mut res := term.strip_ansi(diff.compare_files(p1, p2)!)
	assert res.contains("name: 'Foo'"), res
	assert res.contains("name: 'foo'"), res

	res = diff.compare_files(p1, p2)!
	assert res.contains("+\tname: 'foo'"), res
	assert res.contains("-\tversion: '0.0.0'"), res
	assert res.contains("+\tversion: '0.1.0'"), res
	assert res.contains("+\tlicense: 'MIT'"), res
}

fn test_compare_string() {
	mut res := diff.compare_text('abc', 'abcd')!
	println(res)
	assert res.contains('-abc'), res
	assert res.contains('+abcd'), res
	assert !res.contains('No newline at end of file'), res
}
