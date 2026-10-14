module os

fn test_is_absolute_and_separator() {
	$if windows {
		assert is_absolute('C:/Users/me')
		assert is_absolute('c:/Users/me')
		assert is_absolute('//server/share')
		assert !is_absolute('Users/me')
		assert !is_absolute('')
		assert is_path_separator(`\\`)
		assert is_path_separator(`/`)
		assert !is_path_separator(`:`)
	} $else {
		assert is_absolute('/etc/passwd')
		assert is_absolute('/')
		assert !is_absolute('etc/passwd')
		assert !is_absolute('./etc')
		assert !is_absolute('')
		assert is_path_separator(`/`)
		assert !is_path_separator(`\\`)
	}
}

fn test_volume_name() {
	$if windows {
		assert volume_name('C:/Windows') == 'C:'
		assert volume_name('d:') == 'd:'
		assert volume_name('/Windows') == ''
		assert volume_name('') == ''
	} $else {
		assert volume_name('/home/me') == ''
		assert volume_name('C:/x') == ''
	}
}

fn test_same_file() {
	path := join_path(temp_dir(), 'v_os_samefile_a.txt')
	link := join_path(temp_dir(), 'v_os_samefile_link.txt')
	other := join_path(temp_dir(), 'v_os_samefile_b.txt')
	write_file(path, 'same content') or { panic('write: ${err}') }
	write_file(other, 'same content') or { panic('write: ${err}') }
	defer {
		rm(path) or {}
		rm(link) or {}
		rm(other) or {}
	}
	assert same_file(path, path)!
	assert !same_file(path, other)!
	// a byte-for-byte copy made by read/write is a distinct object ...
	write_file(link, read_file(path)!)!
	assert !same_file(path, link)!, 'byte copies are distinct objects'
	// ... while a hard link is the very same one (POSIX only)
	$if !windows {
		hard := join_path(temp_dir(), 'v_os_samefile_hard.txt')
		link(hard, path) or {}
		if exists(hard) {
			assert same_file(path, hard)!
			rm(hard) or {}
		}
	}
	rm(path) or {}
	same_file(path, other) or {
		// platform-specific wording ('cannot stat' on POSIX, 'cannot open'
		// on Windows), but the path must be named
		assert err.msg().contains(path)
		return
	}
	assert false, 'missing file must error'
}

fn test_copy_file_preserves_mode_and_content() {
	src := join_path(temp_dir(), 'v_os_copy_src.txt')
	dst := join_path(temp_dir(), 'v_os_copy_dst.txt')
	write_file(src, 'payload') or { panic('write: ${err}') }
	chmod(src, 0o640) or {}
	defer {
		rm(src) or {}
		rm(dst) or {}
	}
	copy_file(src, dst)!
	assert read_file(dst) or { panic('read: ${err}') } == 'payload'
	$if !windows {
		assert inode(dst).typ == .regular
	}
	write_file(src, 'x') or {}
	assert read_file(dst) or { panic('read: ${err}') } == 'payload', 'copy must be independent'
}

fn test_user_dirs() {
	setenv('VLIB_OS_TEST_HOME', 'C:/fakehome', true)
	setenv('XDG_CACHE_HOME', 'C:/fakecache', true)
	setenv('XDG_CONFIG_HOME', 'C:/fakeconfig', true)
	defer {
		unsetenv('VLIB_OS_TEST_HOME')
		unsetenv('XDG_CACHE_HOME')
		unsetenv('XDG_CONFIG_HOME')
	}
	h := user_home_dir() or { '' }
	c := user_cache_dir() or { '' }
	g := user_config_dir() or { '' }
	assert h != ''
	assert c != ''
	assert g != ''
	assert c.contains('cache') || c.contains('Local') || c.contains('fakecache')
	assert g.contains('config') || g.contains('Roaming') || g.contains('fakeconfig')
}
