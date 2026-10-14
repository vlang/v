import os

fn test_file_ext() {
	assert os.file_ext('file.v') == '.v'
	assert os.file_ext('a/b/c.tar.gz') == '.gz'
	assert os.file_ext('dir.d/name') == ''
	assert os.file_ext('x/.hidden') == ''
	assert os.file_ext('.ignore_me') == ''
	assert os.file_ext('trailing.') == ''
	assert os.file_ext('dir/') == ''
	// Anything shorter than 3 characters can not carry a name and an extension.
	assert os.file_ext('') == ''
	assert os.file_ext('a') == ''
	assert os.file_ext('ab') == ''
	assert os.file_ext('a.b') == '.b'
}

fn test_file_name() {
	assert os.file_name('') == ''
	assert os.file_name('plain') == 'plain'
	assert os.file_name('a/b/c.tar.gz') == 'c.tar.gz'
	assert os.file_name('a\\b\\c.tar.gz') == 'c.tar.gz'
	assert os.file_name('a/b/') == ''
}

fn test_split_path_splits_directory_name_and_extension() {
	d1, n1, e1 := os.split_path('/usr/lib/test.so')
	assert d1 == '/usr/lib'
	assert n1 == 'test'
	assert e1 == '.so'

	d2, n2, e2 := os.split_path('C:\\dir\\file.txt')
	assert d2 == 'C:\\dir'
	assert n2 == 'file'
	assert e2 == '.txt'

	d3, n3, e3 := os.split_path('a/b/c')
	assert [d3, n3, e3] == ['a/b', 'c', '']

	d4, n4, e4 := os.split_path('a/b/')
	assert [d4, n4, e4] == ['a/b', '', '']

	d5, n5, e5 := os.split_path('name')
	assert [d5, n5, e5] == ['.', 'name', '']

	d6, n6, e6 := os.split_path('.hidden')
	assert [d6, n6, e6] == ['.', '.hidden', '']

	d7, n7, e7 := os.split_path('trailing.')
	assert [d7, n7, e7] == ['.', 'trailing.', '']

	for dot in ['', '.', '..', '/'] {
		d, n, e := os.split_path(dot)
		assert n == ''
		assert e == ''
		assert d != ''
	}
}

fn test_resource_abs_path_builds_an_absolute_nested_path() {
	// The base directory is the folder of the running executable, unless
	// V_RESOURCE_PATH is set, so only assert the parts that are stable.
	res := os.resource_abs_path(os.join_path('one', 'two'))
	assert os.is_abs_path(res)
	assert os.file_name(res) == 'two'
	assert os.dir(res).ends_with('one')
}

fn test_page_size_and_is_main_thread() {
	ps := os.page_size()
	assert ps >= 1024
	assert ps <= 1024 * 1024
	assert os.is_main_thread()
}

fn test_path_predicates_on_a_blank_directory() {
	root := os.join_path(os.vtmp_dir(), 'os_path_helpers_tests_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	empty := os.join_path(root, 'empty')
	os.mkdir_all(empty) or { panic(err) }
	missing := os.join_path(root, 'missing')
	file := os.join_path(root, 'file.txt')
	os.write_file(file, 'x')!

	assert os.is_dir_empty(empty)
	// Documented as returning true for a path that does not exist.
	assert os.is_dir_empty(missing)
	assert !os.is_dir_empty(root)

	assert os.is_file(file)
	assert !os.is_file(empty)
	assert !os.is_file(missing)
	assert os.is_dir(empty)
	assert !os.is_dir(file)
	assert !os.is_link(file)
	assert os.exists(file)
	assert !os.exists(missing)

	assert os.is_readable(file)
	assert os.is_writable(file)
	assert os.file_size(file) == 1
	assert os.read_lines(file)! == ['x']
}
