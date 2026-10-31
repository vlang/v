module path

fn test_clean() {
	assert clean('') == '.'
	assert clean('.') == '.'
	assert clean('/') == '/'
	assert clean('a//b') == 'a/b'
	assert clean('a/./b') == 'a/b'
	assert clean('a/../b') == 'b'
	assert clean('/a/../b') == '/b'
	assert clean('a/b/..') == 'a'
}

fn test_join() {
	assert join('a', 'b') == 'a/b'
	assert join('/a', 'b', 'c') == '/a/b/c'
	assert join('a', '', 'b') == 'a/b'
	assert join() == ''
}

fn test_split() {
	d, f := split('a/b/c')
	assert d == 'a/b/'
	assert f == 'c'
	d2, f2 := split('c')
	assert d2 == ''
	assert f2 == 'c'
}

fn test_base_dir_ext() {
	assert base('a/b/c.txt') == 'c.txt'
	assert base('a/b/') == 'b'
	assert base('') == '.'
	assert dir('a/b/c') == 'a/b'
	assert dir('a') == '.'
	assert ext('a/b/c.txt') == '.txt'
	assert ext('a/b/c') == ''
}

fn test_is_abs() {
	assert is_abs('/a/b')
	assert !is_abs('a/b')
}

fn test_match() {
	assert match('*.go', 'foo.go')! == true
	assert match('*.go', 'foo.txt')! == false
	assert match('?', 'a')! == true
	assert match('?', '')! == false
	assert match('a[b]c', 'abc')! == true
	assert match('a[b]c', 'axc')! == false
	assert match('a[!b]c', 'axc')! == true
}
