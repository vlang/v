import os

fn test_join_path() {
	assert os.join_path('', '', '') == ''
	assert os.join_path('', '') == ''
	assert os.join_path('') == ''
	assert os.join_path('b', '', '') == 'b'
	assert os.join_path('b', '') == 'b'
	assert os.join_path('b') == 'b'
	assert os.join_path('', '', './b') == 'b'
	assert os.join_path('', '', '/b') == os.path_separator + 'b'
	assert os.join_path('', '', 'b') == 'b'
	assert os.join_path('', './b') == 'b'
	assert os.join_path('', '/b') == os.path_separator + 'b'
	assert os.join_path('', 'b') == 'b'
	assert os.join_path('b', '') == 'b'
	$if windows {
		assert os.join_path('./b', '') == r'.\b'
		assert os.join_path('/b', '') == r'\b'
		assert os.join_path('./a', './b') == r'.\a\b'
		assert os.join_path('/', 'test') == r'\test'
		assert os.join_path('v', 'vlib', 'os') == r'v\vlib\os'
		assert os.join_path('', 'f1', 'f2') == r'f1\f2'
		assert os.join_path('v', '', 'dir') == r'v\dir'
		assert os.join_path(r'foo\bar', r'.\file.txt') == r'foo\bar\file.txt'
		assert os.join_path('foo/bar', './file.txt') == r'foo\bar\file.txt'
		assert os.join_path('/opt/v', './x') == r'\opt\v\x'
		assert os.join_path('v', 'foo/bar', 'dir') == r'v\foo\bar\dir'
		assert os.join_path('v', 'foo/bar\\baz', '/dir') == r'v\foo\bar\baz\dir'
		assert os.join_path('C:', 'f1\\..', 'f2') == r'C:\f1\..\f2'
	} $else {
		assert os.join_path('./b', '') == './b'
		assert os.join_path('/b', '') == '/b'
		assert os.join_path('./a', './b') == './a/b'
		assert os.join_path('/', 'test') == '/test'
		assert os.join_path('v', 'vlib', 'os') == 'v/vlib/os'
		assert os.join_path('', 'f1', 'f2') == 'f1/f2'
		assert os.join_path('v', '', 'dir') == 'v/dir'
		assert os.join_path('/foo/bar', './file.txt') == '/foo/bar/file.txt'
		assert os.join_path('foo/bar', './file.txt') == 'foo/bar/file.txt'
		assert os.join_path('/opt/v', './x') == '/opt/v/x'
		assert os.join_path('/foo/bar', './.././file.txt') == '/foo/bar/../file.txt'
		assert os.join_path('v', 'foo/bar\\baz', '/dir') == r'v/foo/bar/baz/dir'
	}
}

fn test_join_path_single() {
	assert os.join_path_single('', '') == ''
	assert os.join_path_single('', './b') == 'b'
	assert os.join_path_single('', '/b') == os.path_separator + 'b'
	assert os.join_path_single('', 'b') == 'b'
	assert os.join_path_single('b', '') == 'b'
	$if windows {
		assert os.join_path_single('./b', '') == r'.\b'
		assert os.join_path_single('/b', '') == r'\b'
		assert os.join_path_single('/foo/bar', './file.txt') == r'\foo\bar\file.txt'
		assert os.join_path_single('/', 'test') == r'\test'
		assert os.join_path_single('foo\\bar', '.\\file.txt') == r'foo\bar\file.txt'
		assert os.join_path_single('/opt/v', './x') == r'\opt\v\x'
	} $else {
		assert os.join_path_single('./b', '') == r'./b'
		assert os.join_path_single('/b', '') == r'/b'
		assert os.join_path_single('/foo/bar', './file.txt') == '/foo/bar/file.txt'
		assert os.join_path_single('/', 'test') == '/test'
		assert os.join_path_single('foo/bar', './file.txt') == 'foo/bar/file.txt'
		assert os.join_path_single('/opt/v', './x') == '/opt/v/x'
		assert os.join_path_single('./a', './b') == './a/b'
		assert os.join_path_single('a', './b') == 'a/b'
	}
}

fn test_join_path_preserves_current_directory() {
	for component in ['./', '././', './/', '.\\', '.\\.\\'] {
		assert os.join_path('', component) == '.'
		assert os.join_path('', '', component) == '.'
		dirs := ['', component]
		assert os.join_path('', ...dirs) == '.'
		assert os.join_path_single('', component) == '.'
		assert os.is_dir(os.join_path('', component))
		assert os.is_dir(os.join_path_single('', component))
	}
}

fn test_join_path_separators_and_roots() {
	sep := os.path_separator
	empty_dirs := []string{}
	for base in ['a', 'a/', 'a\\'] {
		for elem in ['/b', '//b', '///b', '\\b', '\\\\b', './b', './/b'] {
			dirs := [elem]
			assert os.join_path(base, elem) == 'a${sep}b', '${base} ${elem}'
			assert os.join_path(base, ...dirs) == 'a${sep}b', '${base} ${elem}'
			assert os.join_path_single(base, elem) == 'a${sep}b', '${base} ${elem}'
		}
	}
	for root in ['/', '\\'] {
		assert os.join_path(root, ...empty_dirs) == sep
		assert os.join_path(root, '') == sep
		assert os.join_path('', root) == sep
		assert os.join_path('', '', root, 'b') == '${sep}b'
		assert os.join_path_single(root, '') == sep
		assert os.join_path_single('', root) == sep
		for elem in ['/b', '\\b', '//b', '\\\\b'] {
			assert os.join_path(root, elem) == '${sep}b'
			assert os.join_path('', root, elem) == '${sep}b'
			assert os.join_path_single(root, elem) == '${sep}b'
		}
	}
	assert os.join_path('a', '/./b') == 'a${sep}b'
	assert os.join_path('a', 'b///') == 'a${sep}b${sep}'
	assert os.join_path('', '././b') == 'b'
	assert os.join_path('', 'a', '/b', '//c') == 'a${sep}b${sep}c'
	dirs := ['', '/b']
	assert os.join_path('', ...dirs) == '${sep}b'
	$if windows {
		for root in [r'\\', '//'] {
			assert os.join_path(root, ...empty_dirs) == r'\\'
			assert os.join_path('', root) == r'\\'
			assert os.join_path(root, 'server', 'share') == r'\\server\share'
			assert os.join_path('', root, 'server', 'share') == r'\\server\share'
			assert os.join_path_single(root, 'server') == r'\\server'
			assert os.join_path_single('', root) == r'\\'
		}
		for base in [r'C:\', r'C:/'] {
			assert os.join_path(base, ...empty_dirs) == r'C:\'
			assert os.join_path_single(base, '') == r'C:\'
			assert os.join_path(base, 'x') == r'C:\x'
			assert os.join_path_single(base, 'x') == r'C:\x'
		}
		assert os.join_path(r'\\?\C:\', ...empty_dirs) == r'\\?\C:\'
		assert os.join_path_single(r'\\?\C:\', '') == r'\\?\C:\'
		for root in [r'\\server\share', r'\\?\C:', r'\\.\device'] {
			assert os.join_path(root, 'x') == root + r'\x'
			assert os.join_path('', root, 'x') == root + r'\x'
			assert os.join_path_single(root, 'x') == root + r'\x'
			assert os.join_path_single('', root) == root
		}
	}
}
