module os

// is_absolute reports whether path is rooted at '/' on POSIX, or names a
// drive root or UNC volume on Windows. Windows drive-relative paths such as
// 'C:file' and paths rooted on the current drive such as '/file' are relative.
pub fn is_absolute(path string) bool {
	if path == '' { return false }
	$if windows {
		return is_drive_rooted(path) || is_unc_path(path)
	} $else {
		return path[0] == `/`
	}
}

// is_path_separator reports whether c separates path elements on this
// platform (both '/' and '\\' on Windows, '/' elsewhere).
pub fn is_path_separator(c u8) bool {
	$if windows {
		return c == `/` || c == `\\`
	} $else {
		return c == `/`
	}
}

// same_file reports whether a and b refer to the same filesystem object,
// following symbolic links. Hard links compare equal; distinct copies do not.
// Missing or inaccessible paths return an error.
pub fn same_file(a string, b string) !bool {
	$if windows {
		return same_file_windows(a, b)
	} $else {
		sa := stat(a) or { return error('os.same_file: cannot stat `${a}`') }
		sb := stat(b) or { return error('os.same_file: cannot stat `${b}`') }
		return sa.dev == sb.dev && sa.inode == sb.inode
	}
}

// copy_file copies src's contents and basic permission bits (0o777) to dst,
// following links. It rejects destinations that refer to the source object.
// Read, write and permission-setting failures return an error.
pub fn copy_file(src string, dst string) ! {
	mode := int(stat(src)!.mode & 0o777)
	if exists(dst) && same_file(src, dst)! {
		return error('os.copy_file: source and destination are the same file')
	}
	write_file(dst, read_file(src)!)!
	chmod(dst, mode)!
}

// user_home_dir returns the nonempty HOME directory on POSIX or USERPROFILE
// on Windows. Windows can also use HOMEDRIVE together with HOMEPATH.
// It returns an error when the platform's home variables are unavailable.
pub fn user_home_dir() !string {
	$if windows {
		profile := getenv('USERPROFILE')
		if profile != '' { return profile }
		drive := getenv('HOMEDRIVE')
		path := getenv('HOMEPATH')
		if drive != '' && path != '' { return drive + path }
		return error('os: USERPROFILE or HOMEDRIVE and HOMEPATH are not set')
	} $else {
		home := getenv('HOME')
		if home != '' { return home }
		return error('os: HOME is not set')
	}
}

// user_cache_dir returns the user cache directory: LOCALAPPDATA on Windows,
// or XDG_CACHE_HOME on POSIX. Empty values use AppData/Local or ~/.cache.
// A nonempty XDG_CACHE_HOME must be absolute.
pub fn user_cache_dir() !string {
	$if windows {
		cache := getenv('LOCALAPPDATA')
		if cache != '' { return cache }
		return join_path(user_home_dir()!, 'AppData', 'Local')
	} $else {
		cache := getenv('XDG_CACHE_HOME')
		if cache != '' {
			if !is_absolute(cache) { return error('os: XDG_CACHE_HOME must be absolute') }
			return cache
		}
		return join_path(user_home_dir()!, '.cache')
	}
}

// user_config_dir returns the configuration directory: APPDATA on Windows,
// or XDG_CONFIG_HOME on POSIX. Empty values use AppData/Roaming or ~/.config.
// A nonempty XDG_CONFIG_HOME must be absolute.
pub fn user_config_dir() !string {
	$if windows {
		config := getenv('APPDATA')
		if config != '' { return config }
		return join_path(user_home_dir()!, 'AppData', 'Roaming')
	} $else {
		config := getenv('XDG_CONFIG_HOME')
		if config != '' {
			if !is_absolute(config) { return error('os: XDG_CONFIG_HOME must be absolute') }
			return config
		}
		return join_path(user_home_dir()!, '.config')
	}
}

// volume_name returns the Windows drive or UNC volume prefix of path, or an
// empty string on other platforms.
pub fn volume_name(path string) string {
	$if windows {
		return path[..win_root_len(path)]
	} $else {
		return ''
	}
}
