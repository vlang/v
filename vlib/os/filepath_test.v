module os

fn test_is_abs_path() {
	$if windows {
		assert is_abs_path('/')
		assert is_abs_path('\\')
		assert !is_abs_path('\\\\')
		assert !is_abs_path('//')
		assert !is_abs_path(r'/\')
		assert !is_abs_path(r'\/')
		assert is_abs_path('/x')
		assert is_abs_path(r'\x')
		assert is_abs_path('C:/x')
		assert is_abs_path('//Host/share')
		assert is_abs_path(r'//Host\share')
		assert is_abs_path(r'\\Host/share')
		assert !is_abs_path('//Host')
		assert !is_abs_path('//Host/')
		assert is_abs_path(r'C:\path\to\files\file.v')
		assert is_abs_path(r'\\Host\share')
		assert is_abs_path(r'//Host\share\files\file.v')
		assert is_abs_path(r'\\.\BootPartition\Windows')
		assert !is_abs_path(r'\\.\')
		assert !is_abs_path(r'\\?\\')
		assert !is_abs_path(r'C:path\to\dir')
		assert !is_abs_path(r'dir')
		assert !is_abs_path(r'.\')
		assert !is_abs_path(r'.')
		assert !is_abs_path(r'\\Host')
		assert !is_abs_path(r'\\Host\')
		return
	}
	assert is_abs_path('/')
	assert is_abs_path('/path/to/files/file.v')
	assert !is_abs_path('\\')
	assert !is_abs_path('path/to/files/file.v')
	assert !is_abs_path('dir')
	assert !is_abs_path('./')
	assert !is_abs_path('.')
}

fn test_clean_path() {
	$if windows {
		assert clean_path(r'\\path\to\files/file.v') == r'\path\to\files\file.v'
		assert clean_path(r'\/\//\/') == '\\'
		assert clean_path(r'./path\\dir/\\./\/\\/file.v\.\\\.') == r'path\dir\file.v'
		assert clean_path(r'\./path/dir\\file.exe') == r'\path\dir\file.exe'
		assert clean_path(r'.') == ''
		assert clean_path(r'./') == ''
		assert clean_path('') == ''
		assert clean_path(r'\./') == '\\'
		assert clean_path(r'//\/\/////') == '\\'
		return
	}
	assert clean_path('./../.././././//') == '../..'
	assert clean_path('') == ''
	assert clean_path('.') == ''
	assert clean_path('./path/to/file.v//./') == 'path/to/file.v'
	assert clean_path('./') == ''
	assert clean_path('/.') == '/'
	assert clean_path('//path/./to/.///files/file.v///') == '/path/to/files/file.v'
	assert clean_path('path/./to/.///files/.././file.v///') == 'path/to/files/../file.v'
	assert clean_path('\\') == '\\'
	assert clean_path('//////////') == '/'
}

fn test_to_slash() {
	sep := path_separator
	assert to_slash('') == ''
	assert to_slash(sep) == ('/')
	assert to_slash([sep, 'a', sep, 'b'].join('')) == '/a/b'
	assert to_slash(['a', sep, sep, 'b'].join('')) == 'a//b'
}

fn test_from_slash() {
	sep := path_separator
	assert from_slash('') == ''
	assert from_slash('/') == sep
	assert from_slash('/a/b') == [sep, 'a', sep, 'b'].join('')
	assert from_slash('a//b') == ['a', sep, sep, 'b'].join('')
}

fn test_norm_path() {
	$if windows {
		assert norm_path(r'C:/path/to//file.v\\') == r'C:\path\to\file.v'
		assert norm_path(r'C:path\.\..\\\.\to//file.v') == r'C:to\file.v'
		assert norm_path(r'D:path\.\..\..\\\\.\to//dir/..\') == r'D:..\to'
		assert norm_path(r'D:/path\.\..\/..\file.v') == r'D:\file.v'
		assert norm_path(r'') == '.'
		assert norm_path(r'/') == '\\'
		assert norm_path(r'\/') == '\\'
		assert norm_path(r'path\../dir\..') == '.'
		assert norm_path(r'.\.\') == '.'
		assert norm_path(r'G:.\.\dir\././\.\.\\\\///to/././\file.v/./\\') == r'G:dir\to\file.v'
		assert norm_path(r'G:\..\..\.\.\file.v\\\.') == r'G:\file.v'
		assert norm_path(r'\\Server\share\\\dir/..\file.v\./.') == r'\\Server\share\file.v'
		assert norm_path(r'\\.\device\\\dir/to/./file.v\.') == r'\\.\device\dir\to\file.v'
		assert norm_path(r'C:dir/../dir2/../../../file.v') == r'C:..\..\file.v'
		assert norm_path(r'\\.\C:\\\Users/\Documents//..') == r'\\.\C:\Users'
		assert norm_path(r'\\.\C:\Users') == r'\\.\C:\Users'
		assert norm_path(r'\\') == '\\'
		assert norm_path(r'//') == '\\'
		assert norm_path(r'\\\') == '\\'
		assert norm_path(r'.') == '.'
		assert norm_path(r'\\Server') == '\\Server'
		assert norm_path(r'\\Server\') == '\\Server'
		return
	}
	assert norm_path('/path/././../to/file//file.v/.') == '/to/file/file.v'
	assert norm_path('path/././to/files/../../file.v/.') == 'path/file.v'
	assert norm_path('path/././/../../to/file.v/.') == '../to/file.v'
	assert norm_path('/path/././/../..///.././file.v/././') == '/file.v'
	assert norm_path('path/././//../../../to/dir//.././file.v/././') == '../../to/file.v'
	assert norm_path('path/../dir/..') == '.'
	assert norm_path('../dir/..') == '..'
	assert norm_path('/../dir/..') == '/'
	assert norm_path('//././dir/../files/././/file.v') == '/files/file.v'
	assert norm_path('/\\../dir/////////.') == '/\\../dir'
	assert norm_path('/home/') == '/home'
	assert norm_path('/home/////./.') == '/home'
	assert norm_path('...') == '...'
}

fn test_abs_path() {
	wd := getwd()
	wd_w_sep := wd + path_separator
	$if windows {
		assert abs_path('path/to/file.v') == '${wd_w_sep}path\\to\\file.v'
		assert abs_path('path/to/file.v') == '${wd_w_sep}path\\to\\file.v'
		assert abs_path('/') == r'\'
		assert abs_path(r'C:\path\to\files\file.v') == r'C:\path\to\files\file.v'
		assert abs_path(r'C:\/\path\.\to\../files\file.v\.\\\.\') == r'C:\path\files\file.v'
		assert abs_path(r'\\Host\share\files\..\..\.') == r'\\Host\share\'
		assert abs_path(r'\\.\HardDiskvolume2\files\..\..\.') == r'\\.\HardDiskvolume2\'
		assert abs_path(r'\\?\share') == r'\\?\share'
		assert abs_path(r'\\.\') == r'\'
		assert abs_path(r'G:/\..\\..\.\.\file.v\\.\.\\\\') == r'G:\file.v'
		assert abs_path('files') == '${wd_w_sep}files'
		assert abs_path('') == wd
		assert abs_path('.') == wd
		assert abs_path('files/../file.v') == '${wd_w_sep}file.v'
		assert abs_path('///') == r'\'
		assert abs_path('/path/to/file.v') == r'\path\to\file.v'
		assert abs_path('D:/') == r'D:\'
		assert abs_path(r'\\.\HardiskVolume6') == r'\\.\HardiskVolume6'
		return
	}
	assert abs_path('/') == '/'
	assert abs_path('.') == wd
	assert abs_path('files') == '${wd_w_sep}files'
	assert abs_path('') == wd
	assert abs_path('files/../file.v') == '${wd_w_sep}file.v'
	assert abs_path('///') == '/'
	assert abs_path('/path/to/file.v') == '/path/to/file.v'
	assert abs_path('/path/to/file.v/../..') == '/path'
	assert abs_path('path/../file.v/..') == wd
	assert abs_path('///') == '/'
}

fn test_existing_path() {
	wd := getwd()
	$if windows {
		assert existing_path('') or { '' } == ''
		assert existing_path('..') or { '' } == '..'
		assert existing_path('.') or { '' } == '.'
		assert existing_path(wd) or { '' } == wd
		assert existing_path('\\') or { '' } == '\\'
		assert existing_path('${wd}\\.\\\\does/not/exist\\.\\') or { '' } == '${wd}\\.\\\\'
		assert existing_path('${wd}\\\\/\\.\\.\\/.') or { '' } == '${wd}\\\\/\\.\\.\\/.'
		assert existing_path('${wd}\\././/\\/oh') or { '' } == '${wd}\\././/\\/'
		return
	}
	assert existing_path('') or { '' } == ''
	assert existing_path('..') or { '' } == '..'
	assert existing_path('.') or { '' } == '.'
	assert existing_path(wd) or { '' } == wd
	assert existing_path('/') or { '' } == '/'
	assert existing_path('${wd}/does/.///not/exist///.//') or { '' } == '${wd}/'
	assert existing_path('${wd}//././/.//') or { '' } == '${wd}//././/.//'
	assert existing_path('${wd}//././/.//oh') or { '' } == '${wd}//././/.//'
}

fn test_windows_volume() {
	$if windows {
		assert windows_volume('C:/path\\to/file.v') == 'C:'
		assert windows_volume('D:\\.\\') == 'D:'
		assert windows_volume('G:') == 'G:'
		assert windows_volume('G') == ''
		assert windows_volume(r'\\Host\share\files\file.v') == r'\\Host\share'
		assert windows_volume('\\\\Host\\') == ''
		assert windows_volume(r'\\.\BootPartition2\\files\.\\') == r'\\.\BootPartition2'
		assert windows_volume(r'\/.\BootPartition2\\files\.\\') == r'\/.\BootPartition2'
		assert windows_volume(r'\\\.\BootPartition2\\files\.\\') == ''
		assert windows_volume('') == ''
		assert windows_volume('\\') == ''
		assert windows_volume('/') == ''
	}
}

fn test_trim_extended_length_path_prefix() {
	$if windows {
		assert trim_extended_length_path_prefix(r'\\?\C:\path\to\file.v') == r'C:\path\to\file.v'
		assert trim_extended_length_path_prefix(r'\\?\UNC\host\share\file.v') == r'\\host\share\file.v'
		assert trim_extended_length_path_prefix(r'\\?\Volume{01234567-89ab-cdef-0123-456789abcdef}\file.v') == r'\\?\Volume{01234567-89ab-cdef-0123-456789abcdef}\file.v'
		assert trim_extended_length_path_prefix(r'C:\path\to\file.v') == r'C:\path\to\file.v'
		assert trim_extended_length_path_prefix('') == ''
	}
}

fn test_parent_dir() {
	$if windows {
		assert parent_dir(r'S:\repo\vlang') == r'S:\repo'
		// The parent of a top level directory is the absolute drive root, not the
		// bare volume `S:`, which names the current directory *on that drive*.
		assert parent_dir(r'S:\repo') == 'S:\\'
		assert parent_dir('S:/repo') == 'S:/'
		// `dir('S:')` is `.`, which would restart a parent walk at the current
		// directory; `parent_dir` reports "no parent" instead.
		assert parent_dir('S:') == ''
		assert parent_dir('S:\\') == ''
		assert parent_dir('S:/') == ''
		// A drive relative path has no parent that can be named without the
		// current directory on that drive, however many components it has: the
		// ancestors resolve against that same hidden per-drive state.
		assert parent_dir('S:outside') == ''
		assert parent_dir(r'S:foo\bar') == ''
		assert parent_dir('S:foo/bar') == ''
		assert parent_dir(r'S:foo\bar\baz') == ''
		assert parent_dir(r'S:foo\bar' + '\\') == ''
		assert !is_drive_relative_path(r'S:\foo')
		assert is_drive_relative_path(r'S:foo\bar')
		assert parent_dir(r'\\Host\share') == ''
		assert parent_dir(r'\\Host\share\') == ''
		assert parent_dir(r'\\Host\share\files') == r'\\Host\share' + '\\'
		assert parent_dir(r'\\Host\share\files\file.v') == r'\\Host\share\files'
		// Windows accepts both separators in one path, so the parent is decided by
		// the last separator of either kind, not by whichever kind appears first.
		assert parent_dir(r'C:/one\two') == 'C:/one'
		assert parent_dir(r'C:\one/two') == r'C:\one'
		assert parent_dir(r'C:/one\two/three') == r'C:/one\two'
		assert parent_dir(r'\\Host\share/files\file.v') == r'\\Host\share/files'
		// A trailing separator names the same directory, so it cannot select the
		// parent; trimming it must still stop at a root.
		assert parent_dir(r'C:\a\b' + '\\') == r'C:\a'
		assert parent_dir(r'C:\a\b\\') == r'C:\a'
		assert parent_dir(r'C:\a' + '\\') == 'C:\\'
		assert parent_dir('C:/a/') == 'C:/'
		assert parent_dir('C:\\\\') == ''
		assert parent_dir(r'\\Host\share\files' + '\\') == r'\\Host\share' + '\\'
		assert parent_dir(r'\\Host\share\\') == ''
		// `\\?\UNC\server\share` is a share root spelled with the extended length
		// prefix. `win_volume_len` stops at the `\\?\UNC` tag, which names nothing,
		// so the server and the share have to count as part of the root too.
		assert parent_dir(r'\\?\UNC\server\share') == ''
		assert parent_dir(r'\\?\UNC\server\share' + '\\') == ''
		assert parent_dir(r'\\?\UNC\server\share\files') == r'\\?\UNC\server\share' + '\\'
		assert parent_dir(r'\\?\UNC\server\share\files\file.v') == r'\\?\UNC\server\share\files'
		assert parent_dir(r'\\?\unc\server\share\files') == r'\\?\unc\server\share' + '\\'
		// Incomplete extended UNC paths do not name a location either.
		assert parent_dir(r'\\?\UNC\server') == ''
		assert parent_dir(r'\\?\UNC') == ''
		// `UNCx` is an ordinary extended device name, not the UNC tag.
		assert parent_dir(r'\\?\UNCx\a\b') == r'\\?\UNCx\a'
		// The extended length drive form already worked; keep it that way.
		assert parent_dir(r'\\?\C:\dir') == r'\\?\C:' + '\\'
		assert parent_dir(r'\\?\C:\dir\file.v') == r'\\?\C:\dir'
		assert parent_dir(r'\\?\C:' + '\\') == ''
		assert parent_dir('\\') == ''
		assert parent_dir('.') == ''
		assert parent_dir('') == ''
		assert parent_dir('file.v') == ''
		return
	}
	assert parent_dir('/path/to/files/file.v') == '/path/to/files'
	assert parent_dir('/path') == '/'
	assert parent_dir('/') == ''
	assert parent_dir('.') == ''
	assert parent_dir('') == ''
	assert parent_dir('file.v') == ''
	assert parent_dir('path/to/file.v') == 'path/to'
	// A backslash is an ordinary file name byte outside Windows, so it does not
	// split a path here. This is deliberately unlike `dir`, which treats it as a
	// separator whenever the path holds no forward slash.
	assert parent_dir(r'one\two') == ''
	assert parent_dir(r'one/two\three') == 'one'
	// A trailing separator names the same directory, so it cannot select the
	// parent; trimming it must still stop at the root.
	assert parent_dir('/a/b/') == '/a'
	assert parent_dir('/a/b//') == '/a'
	assert parent_dir('/path/') == '/'
	assert parent_dir('///') == ''
	assert parent_dir('./') == ''
	assert parent_dir('a/') == ''
	assert parent_dir('path/to/') == 'path'
}

// test_parent_dir_walk_terminates guards the parent walks in the compiler
// (vroot and v.mod root detection): every one of them must reach a root in a
// bounded number of steps, and must never hand the walk a drive relative value
// to probe on the way up, from any starting path, on any platform.
fn test_parent_dir_walk_terminates() {
	for start in ['/a/b/c', 'a/b/c', '/a/b/', '.', '/', '', r'S:\a\b', 'S:', 'S:outside',
		r'S:foo\bar\baz', r'\\Host\share\a', r'\\?\UNC\srv\shr\a'] {
		mut dir := start
		mut steps := 0
		for dir.len > 0 {
			parent := parent_dir(dir)
			assert parent != '.'
			assert !is_drive_relative_path(parent)
			// Every step has to make progress. A parent that is merely `dir`
			// without its trailing separator would probe the same directory
			// twice and cost the bounded vroot walks one ancestor.
			assert parent.len < dir.len
			dir = parent
			steps++
			assert steps < 16
		}
	}
}

// path_rel_parity_cases are the shapes whose results were taken from Go's
// path/filepath.Rel and checked value by value, not from reading its source.
// Expected values are built from path_separator so the same table holds on both
// platforms.
fn path_rel_parity_cases() [][]string {
	sep := path_separator
	return [
		['a', 'a', '.']
		['a/b', 'a/b', '.']
		['/a/b', '/a/c', '..${sep}c']
		['a/b', 'a/c', '..${sep}c']
		['a/b/c', 'a', '..${sep}..']
		['a', 'a/b/c', 'b${sep}c']
		['/', '/a', 'a']
		['/a', '/', '..']
		['a/b', 'a', '..']
		['a', 'a/b', 'b']
		['a/b/', 'a/b', '.']
		['a/b', 'a/b/', '.']
		['a/b', 'a/b/.', '.']
		['./a/b', 'a/c', '..${sep}c']
		['a/./b', 'a/c', '..${sep}c']
		['a/../a/b', 'a/c', '..${sep}c']
		['a/b/../c', 'c', '..${sep}..${sep}c']
		['', '', '.']
		['', 'a', 'a']
		['a', '', '..']
		['.', 'a', 'a']
		['a', '.', '..']
		['a/b', './c', '..${sep}..${sep}c']
		['/a/b/c', '/a', '..${sep}..']
		['/a/b', '/a/b/c', 'c']
		['a/b/c/d', 'a/e', '..${sep}..${sep}..${sep}e']
		['a/b/c', 'a/b/c', '.']
		// these normalize rather than fail
		['a/b/../..', 'c', 'c']
		['a/..', 'b', 'b']
		['..', '..', '.']
		['../a', '../b', '..${sep}b']
		// ".." in the target is fine; it is only the base that cannot start with it
		['a', '../b', '..${sep}..${sep}b'],
	]
}

fn test_path_rel() {
	for c in path_rel_parity_cases() {
		got := path_rel(c[0], c[1]) or {
			assert false, 'path_rel(${c[0]}, ${c[1]}) failed with ${err.msg()}'
			return
		}
		assert got == c[2], 'path_rel(${c[0]}, ${c[1]}) = "${got}", want "${c[2]}"'
	}
}

fn test_path_rel_errors() {
	// One path absolute and the other not, or the base itself below the working
	// directory, so no relative path can express the target.
	failing := [
		['/a', 'b']
		['a', '/b']
		['..', 'a']
		['../a', 'b']
		['../..', 'a']
		['../a', 'a'],
	]
	for c in failing {
		mut failed := false
		path_rel(c[0], c[1]) or {
			failed = true
		}
		assert failed, 'path_rel(${c[0]}, ${c[1]}) should have failed'
	}
}

fn test_path_rel_rejects_different_windows_volumes() {
	$if windows {
		mut failed := false
		path_rel(r'C:\a\b', r'D:\a\c') or {
			failed = true
		}
		assert failed, 'path_rel across two volumes should have failed'
	}
}

fn test_path_rel_roots_and_case() ! {
	$if windows {
		// Volumes and components compare case-insensitively, and either
		// separator may be used.
		assert path_rel(r'C:\a\b', r'c:\A\c')! == r'..\c'
		assert path_rel('C:/a/b', r'C:\a\c')! == r'..\c'
		assert path_rel(r'\\server\share\a\b', r'\\SERVER\share\a\c')! == r'..\c'
		assert path_rel(r'\\server\share', r'\\server\share\a')! == 'a'
		// The drive of a drive relative path is its root, not a component.
		assert path_rel('C:a', 'C:b')! == r'..\b'
		assert path_rel('C:', 'C:a')! == 'a'
		failing := [
			[r'C:..\a', 'C:b']
			['C:a', r'C:\a']
			[r'\\server\s1\a', r'\\server\s2\a']
			[r'\\?\UNC\srv\s1\a', r'\\?\UNC\srv\s2\a'],
		]
		for c in failing {
			mut failed := false
			path_rel(c[0], c[1]) or {
				failed = true
			}
			assert failed, 'path_rel(${c[0]}, ${c[1]}) should have failed'
		}
	} $else {
		// Elsewhere components that differ only in case name different paths.
		assert path_rel('/a/B', '/a/b')! == '../b'
	}
}

// A relative path that resolves back to the target is the whole point of the
// function, so check that property rather than only the string it returns.
fn test_path_rel_result_resolves_to_target() {
	sep := path_separator
	pairs := [
		['/a/b/c', '/a/d/e']
		['/a/b', '/a/b/c/d']
		['/x', '/y/z']
		['p/q', 'p/r/s'],
	]
	for p in pairs {
		rel := path_rel(p[0], p[1]) or { panic(err) }
		joined := if is_abs_path(rel) {
			rel
		} else {
			norm_path(p[0] + sep + rel)
		}
		assert joined == norm_path(p[1]), 'from ${p[0]} via "${rel}" got "${joined}", want "${norm_path(p[1])}"'
	}
}
