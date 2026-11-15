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

fn test_root_and_extension_edges() {
	assert base('/') == '/'
	assert base('////') == '/'
	assert ext('.profile') == '.profile'
	assert ext('.') == '.'
	assert ext('..') == '.'
	assert ext('a.txt/') == ''
	assert ext('a.b/c') == ''
	assert ext('a.b/c.') == '.'
	assert ext('') == ''
	assert dir('////') == '/'
	assert dir('a/../') == '.'
}

fn test_clean_relative_parents_and_slash_only_paths() {
	for input, expected in {
		'../../a/../b': '../../b'
		'a/../../b':    '../b'
		'/../../a':     '/a'
		'//a///b//':    '/a/b'
		'a/../..':      '..'
		'C:\\a\\b':     'C:\\a\\b'
	} {
		assert clean(input) == expected
		assert clean(clean(input)) == expected
	}
	assert join('a', '/b') == 'a/b'
	assert join('', '') == ''
	assert !is_abs('C:\\a')
	for input in ['', '/', 'a//b/', 'a\\b', 'a/b'] {
		d, f := split(input)
		assert d + f == input
	}
}

fn test_match_respects_slashes() {
	assert !match('*', 'a/b')!
	assert !match('?', '/')!
	assert !match('a*/b', 'a/c/b')!
	assert match('a/*/b', 'a/c/b')!
	assert match('a/*', 'a/')!
	assert match('[/]', '/')!
	assert !match('**', '/')!
}

fn test_match_unicode_and_escapes() {
	assert match('?', 'é')!
	assert match('[α-ω]', 'β')!
	assert match('a?c', 'a界c')!
	assert !match('??', 'é')!
	assert !match('??', '世')!
	assert match('*??', '世')!
	assert match('*界?', 'hello界é')!
	assert match(r'a\*b', 'a*b')!
	assert match(r'\?', '?')!
	assert match(r'\\', '\\')!
	assert match(r'[\-\]]', '-')!
	assert match(r'[\-\]]', ']')!
	assert match('[^a]', 'b')!
	assert match('[!a]', 'b')!
	assert !match('[^a]', 'a')!
}

fn test_match_rejects_malformed_patterns_before_matching() {
	for pattern in ['[', '[]', '[^]', '[!]', '[a-]', '[-a]', '[a', r'\', '*[', 'x[', r'*\', 'x[]',
		'[a-b-c]'] {
		for name in ['', 'a', 'xyz', '/'] {
			if result := path.match(pattern, name) {
				assert false, 'accepted malformed pattern ${pattern}: ${result}'
			} else {
				assert err.msg() == 'path: invalid pattern'
			}
		}
	}
}

fn test_match_backtracking_and_whole_name() {
	assert match('*ab*bc', 'ababc')!
	assert !match('*ab*bc', 'ababcx')!
	assert match('*a*b*c', 'aaabbbccc')!
	assert match('***', '')!
	assert !match('*a*a*a*a*a*a*a*a*a*a*b', 'aaaaaaaaaaaa')!
	assert match('*[/a]*b', 'aa/b')!
}

fn test_match_long_patterns_do_not_recurse_or_expand_exponentially() {
	assert !match('*a'.repeat(32) + 'b', 'a'.repeat(64))!
	assert match('a'.repeat(10000), 'a'.repeat(10000))!
}

fn test_match_invalid_utf8_and_nul() {
	invalid := [u8(0xff)].bytestr()
	assert match('?', invalid)!
	assert match('[\ufffd]', invalid)!
	assert match(invalid, invalid)!
	assert !match(invalid, [u8(0xfe)].bytestr())!
	assert match('?', '\x00')!
	assert match('\x00', '\x00')!
	if result := path.match('[' + invalid + ']', 'a') {
		assert false, 'accepted invalid UTF-8 class: ${result}'
	} else {
		assert err.msg() == 'path: invalid pattern'
	}
}
