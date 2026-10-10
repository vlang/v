module os

// `/` separates path elements on every system. `\` separates them on Windows too, so a
// path may mix both there. On other systems `\` is a valid file name character, and it
// separates only in a path that has no `/` at all.

struct LastSeparatorCase {
	path    string
	windows int // the index of the last separator, when `\` always separates
	other   int // the index of the last separator, when `\` separates only without a `/`
}

const last_separator_cases = [
	LastSeparatorCase{
		path:    ''
		windows: -1
		other:   -1
	},
	LastSeparatorCase{
		path:    'a'
		windows: -1
		other:   -1
	},
	LastSeparatorCase{
		path:    'a/b'
		windows: 1
		other:   1
	},
	LastSeparatorCase{
		path:    'a\\b'
		windows: 1
		other:   1
	},
	LastSeparatorCase{
		path:    'a/b\\c'
		windows: 3
		other:   1
	},
	LastSeparatorCase{
		path:    'a\\b/c'
		windows: 3
		other:   3
	},
	LastSeparatorCase{
		path:    'go/out\\osf_v_fixture'
		windows: 6
		other:   2
	},
	LastSeparatorCase{
		path:    'a/b\\'
		windows: 3
		other:   1
	},
	LastSeparatorCase{
		path:    'a\\b/'
		windows: 3
		other:   3
	},
	LastSeparatorCase{
		path:    '/'
		windows: 0
		other:   0
	},
	LastSeparatorCase{
		path:    '\\'
		windows: 0
		other:   0
	},
	LastSeparatorCase{
		path:    '/\\'
		windows: 1
		other:   0
	},
	LastSeparatorCase{
		path:    '\\/'
		windows: 1
		other:   1
	},
	LastSeparatorCase{
		path:    'C:\\a/b\\c.v'
		windows: 6
		other:   4
	},
]

// Both sets of rules are checked on every system, since the helper takes the rule.
fn test_last_separator_index() {
	for c in last_separator_cases {
		assert last_separator_index(c.path, c.path.len, true) == c.windows, c.path
		assert last_separator_index(c.path, c.path.len, !c.path.contains('/')) == c.other, c.path
		expected := $if windows { c.windows } $else { c.other }
		assert last_separator_index(c.path, c.path.len, backslash_separates(c.path)) == expected, c.path
	}
	// `end` limits the search: that is how `base` skips a trailing separator.
	assert last_separator_index('a/b\\', 3, true) == 1
	assert last_separator_index('a\\b/', 3, true) == 1
	assert last_separator_index('a\\b/', 3, false) == -1
	assert last_separator_index('a/b', 0, true) == -1
}

fn test_backslash_separates() {
	assert backslash_separates('')
	assert backslash_separates('a')
	assert backslash_separates('a\\b')
	$if windows {
		assert backslash_separates('a/b')
		assert backslash_separates('a/b\\c')
	} $else {
		assert !backslash_separates('a/b')
		assert !backslash_separates('a/b\\c')
	}
}

// A path with one kind of separator is split in the same way on every system.
fn test_one_kind_of_separator() {
	assert dir('') == '.'
	assert base('') == '.'
	assert file_name('') == ''
	assert dir('a') == '.'
	assert base('a') == 'a'
	assert file_name('a') == 'a'

	assert dir('a/b/c.v') == 'a/b'
	assert base('a/b/c.v') == 'c.v'
	assert file_name('a/b/c.v') == 'c.v'
	assert dir('a\\b\\c.v') == 'a\\b'
	assert base('a\\b\\c.v') == 'c.v'
	assert file_name('a\\b\\c.v') == 'c.v'

	// a trailing separator
	assert dir('a/b/') == 'a/b'
	assert base('a/b/') == 'b'
	assert file_name('a/b/') == ''
	assert dir('a\\b\\') == 'a\\b'
	assert base('a\\b\\') == 'b'
	assert file_name('a\\b\\') == ''

	// only separators
	assert dir('/') == '/'
	assert base('/') == '/'
	assert file_name('/') == ''
	assert dir('\\') == '\\'
	assert base('\\') == '\\'
	assert file_name('\\') == ''
	assert dir('//') == '/'
	assert dir('\\\\') == '\\'

	// the root and a drive letter
	assert dir('/a') == '/'
	assert dir('\\a') == '\\'
	assert dir('C:/a/b.v') == 'C:/a'
	assert dir('C:\\a\\b.v') == 'C:\\a'
	assert base('C:\\a\\b.v') == 'b.v'
}

fn test_mixed_separators() {
	$if windows {
		// The last separator of either kind is the one that counts.
		assert file_name('go/out\\osf_v_fixture') == 'osf_v_fixture'
		assert file_name('a/b\\c') == 'c'
		assert base('a/b\\c') == 'c'
		assert dir('a/b\\c') == 'a/b'
		assert file_name('a\\b/c') == 'c'
		assert base('a\\b/c') == 'c'
		assert dir('a\\b/c') == 'a\\b'
		// a trailing separator of the other kind
		assert dir('a/b\\') == 'a/b'
		assert base('a/b\\') == 'b'
		assert file_name('a/b\\') == ''
		assert dir('a\\b/') == 'a\\b'
		assert base('a\\b/') == 'b'
		assert file_name('a\\b/') == ''
		// only separators
		assert dir('/\\') == '/'
		assert dir('\\/') == '\\'
		assert file_name('/\\') == ''
		assert file_name('\\/') == ''
		// the root and a drive letter
		assert dir('/a\\b') == '/a'
		assert dir('C:\\a/b\\c.v') == 'C:\\a/b'
		assert base('C:\\a/b\\c.v') == 'c.v'
		// a `.` in a directory name is not an extension
		assert file_ext('a/b.c\\d') == ''
		d, name, ext := split_path('C:/a\\b.c.v')
		assert [d, name, ext] == ['C:/a', 'b.c', '.v']
		d2, name2, ext2 := split_path('a/b\\')
		assert [d2, name2, ext2] == ['a/b', '', '']
	} $else {
		// `\` is a valid file name character here, so a path that has a `/` is split on `/` only.
		assert file_name('go/out\\osf_v_fixture') == 'out\\osf_v_fixture'
		assert file_name('a/b\\c') == 'b\\c'
		assert base('a/b\\c') == 'b\\c'
		assert dir('a/b\\c') == 'a'
		assert file_name('a\\b/c') == 'c'
		assert base('a\\b/c') == 'c'
		assert dir('a\\b/c') == 'a\\b'
		// such as the escaped name of a systemd unit
		assert file_name('/run/systemd/dev-by\\x2dlabel.swap') == 'dev-by\\x2dlabel.swap'
		// a trailing `\` belongs to the name, a trailing `/` ends a directory named `a\b`
		assert dir('a/b\\') == 'a'
		assert base('a/b\\') == 'b\\'
		assert file_name('a/b\\') == 'b\\'
		assert dir('a\\b/') == 'a\\b'
		assert base('a\\b/') == 'a\\b'
		assert file_name('a\\b/') == ''
		// only separators
		assert dir('/\\') == '/'
		assert dir('\\/') == '\\'
		assert file_name('/\\') == '\\'
		assert file_name('\\/') == ''
		// the root and a drive letter
		assert dir('/a\\b') == '/'
		assert dir('C:\\a/b\\c.v') == 'C:\\a'
		assert base('C:\\a/b\\c.v') == 'b\\c.v'
		assert file_ext('a/b.c\\d') == '.c\\d'
		d, name, ext := split_path('C:/a\\b.c.v')
		assert [d, name, ext] == ['C:', 'a\\b.c', '.v']
		d2, name2, ext2 := split_path('a/b\\')
		assert [d2, name2, ext2] == ['a', 'b\\', '']
	}
}

// Without a trailing separator, `dir` and `file_name` are the two sides of the last
// separator and `base` is `file_name`, whichever separators the current system accepts.
fn test_dir_base_and_file_name_agree() {
	for path in ['a/b', 'a\\b', 'a/b\\c', 'a\\b/c', 'go/out\\osf_v_fixture', 'C:\\a/b\\c.v',
		'C:/a\\b/c.v'] {
		d := dir(path)
		name := file_name(path)
		assert base(path) == name, path
		assert path.starts_with(d), path
		assert path.ends_with(name), path
		assert d.len + 1 + name.len == path.len, path
		split_dir, split_name, split_ext := split_path(path)
		assert split_dir == d, path
		assert split_name + split_ext == name, path
	}
}
