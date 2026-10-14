module os

fn test_is_absolute_and_separator() {
	$if windows {
		assert is_absolute('C:/Users/me')
		assert is_absolute('c:/Users/me')
		assert is_absolute('//server/share')
		assert !is_absolute('C:relative')
		assert !is_absolute('C:')
		assert !is_absolute('/current-drive')
		assert !is_absolute(r'\current-drive')
		assert !is_absolute('//server')
		assert !is_absolute('//server/')
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
		assert volume_name('//server/share/file') == '//server/share'
		assert volume_name(r'\\?\UNC\server\share\file') == r'\\?\UNC\server\share'
		assert volume_name('/Windows') == ''
		assert volume_name('') == ''
	} $else {
		assert volume_name('/home/me') == ''
		assert volume_name('C:/x') == ''
	}
}

fn test_same_file() {
	path := join_path(temp_dir(), 'v_os_samefile_a.txt')
	copy_path := join_path(temp_dir(), 'v_os_samefile_link.txt')
	other := join_path(temp_dir(), 'v_os_samefile_b.txt')
	write_file(path, 'same content') or { panic('write: ${err}') }
	write_file(other, 'same content') or { panic('write: ${err}') }
	defer {
		rm(path) or {}
		rm(copy_path) or {}
		rm(other) or {}
	}
	assert same_file(path, path)!
	assert !same_file(path, other)!
	// a byte-for-byte copy made by read/write is a distinct object ...
	write_file(copy_path, read_file(path)!)!
	assert !same_file(path, copy_path)!, 'byte copies are distinct objects'
	// ... while a hard link is the very same one (POSIX only)
	$if !windows {
		hard := join_path(temp_dir(), 'v_os_samefile_hard.txt')
		link(path, hard)!
		assert same_file(path, hard)!
		rm(hard)!
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
		assert stat(dst)!.mode & 0o777 == 0o640
	}
	write_file(src, 'x') or {}
	assert read_file(dst) or { panic('read: ${err}') } == 'payload', 'copy must be independent'
}

fn test_user_dirs() {
	names := ['USERPROFILE', 'LOCALAPPDATA', 'APPDATA', 'XDG_CACHE_HOME', 'XDG_CONFIG_HOME']
	saved := environ()
	defer {
		for name in names {
			if value := saved[name] {
				setenv(name, value, true)
			} else {
				unsetenv(name)
			}
		}
	}
	$if windows {
		setenv('USERPROFILE', 'C:/fakehome', true)
		setenv('LOCALAPPDATA', 'C:/fakecache', true)
		setenv('APPDATA', 'C:/fakeconfig', true)
		setenv('XDG_CACHE_HOME', '/ignored-cache', true)
		setenv('XDG_CONFIG_HOME', '/ignored-config', true)
		assert user_home_dir()! == 'C:/fakehome'
		assert user_cache_dir()! == 'C:/fakecache'
		assert user_config_dir()! == 'C:/fakeconfig'
		setenv('LOCALAPPDATA', '', true)
		setenv('APPDATA', '', true)
		assert user_cache_dir()! == join_path('C:/fakehome', 'AppData', 'Local')
		assert user_config_dir()! == join_path('C:/fakehome', 'AppData', 'Roaming')
	} $else {
		setenv('LOCALAPPDATA', '/ignored-cache', true)
		setenv('APPDATA', '/ignored-config', true)
		setenv('XDG_CACHE_HOME', '/fakecache', true)
		setenv('XDG_CONFIG_HOME', '/fakeconfig', true)
		assert user_cache_dir()! == '/fakecache'
		assert user_config_dir()! == '/fakeconfig'
		setenv('XDG_CACHE_HOME', '', true)
		setenv('XDG_CONFIG_HOME', '', true)
		home := user_home_dir()!
		assert user_cache_dir()! == join_path(home, '.cache')
		assert user_config_dir()! == join_path(home, '.config')
		setenv('XDG_CACHE_HOME', 'relative-cache', true)
		setenv('XDG_CONFIG_HOME', 'relative-config', true)
		if cache := user_cache_dir() {
			assert false, 'accepted ${cache}'
		} else {
			assert err.msg().contains('absolute')
		}
		if config := user_config_dir() {
			assert false, 'accepted ${config}'
		} else {
			assert err.msg().contains('absolute')
		}
	}
}

fn test_same_file_unicode_and_directories() {
	root := join_path(temp_dir(), 'v_same_file_é_目录')
	mkdir_all(root)!
	defer { rmdir_all(root) or {} }
	path := join_path(root, 'café.txt')
	write_file(path, 'payload')!
	assert same_file(root, root)!
	assert same_file(path, path)!
	$if !windows {
		symbolic := join_path(root, 'symbolic')
		symlink(path, symbolic)!
		assert same_file(path, symbolic)!
	}
}

fn test_copy_file_uses_symbolic_link_target_mode() {
	$if windows {
		return
	}
	root := join_path(temp_dir(), 'v_copy_file_symlink_mode')
	mkdir_all(root)!
	defer { rmdir_all(root) or {} }
	source := join_path(root, 'source')
	symbolic := join_path(root, 'symbolic')
	destination := join_path(root, 'destination')
	write_file(source, 'content')!
	chmod(source, 0o640)!
	symlink(source, symbolic)!
	copy_file(symbolic, destination)!
	assert read_file(destination)! == 'content'
	assert stat(destination)!.mode & 0o777 == 0o640
}

fn test_copy_file_rejects_source_aliases() {
	root := join_path(temp_dir(), 'v_copy_file_source_alias')
	mkdir_all(root)!
	defer { rmdir_all(root) or {} }
	source := join_path(root, 'source')
	alias := join_path(root, 'alias')
	write_file(source, 'preserved')!
	link(source, alias)!
	for destination in [source, alias] {
		copy_file(source, destination) or {
			assert err.msg().contains('same file')
			continue
		}
		assert false, 'copy to a source alias must fail'
	}
	assert read_file(source)! == 'preserved'
}
