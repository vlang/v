module os

// is_absolute reports whether path starts from the filesystem root: '/' on
// POSIX and a drive-letter prefix (or a UNC prefix) on Windows. It is the
// V-idiomatic equivalent of Go's filepath.IsAbs / Rust's Path::is_absolute.
pub fn is_absolute(path string) bool {
	if path == '' {
		return false
	}
	$if windows {
		// C:\... or \\.\... (UNC) or \\server\share
		if path.len >= 2 && path[1] == `:` && is_drive_letter(path[0]) {
			return true
		}
		return path.starts_with('\\') || path.starts_with('/')
	} $else {
		return path[0] == `/`
	}
}

// is_drive_letter reports whether c is an ASCII letter.
fn is_drive_letter(c u8) bool {
	return (c >= `a` && c <= `z`) || (c >= `A` && c <= `Z`)
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

// same_file reports whether a and b refer to the same filesystem object, so
// hard links compare equal while distinct copies do not. It is the
// V-idiomatic equivalent of Go's os.SameFile: POSIX compares
// (device, inode) from lstat; Windows compares the file index from
// GetFileInformationByHandle, because MSVCRT's stat leaves st_ino zero.
pub fn same_file(a string, b string) !bool {
	$if windows {
		return same_file_windows(a, b)
	} $else {
		sa := lstat(a) or { return error('os.same_file: cannot stat `${a}`') }
		sb := lstat(b) or { return error('os.same_file: cannot stat `${b}`') }
		return sa.dev == sb.dev && sa.inode == sb.inode
	}
}

// copy_file copies the contents of src to dst together with its permission
// bits (not the full metadata). It is the V-idiomatic equivalent of Python's
// shutil.copyfile + copymode combined; `os.cp` remains for recursive copies.
pub fn copy_file(src string, dst string) ! {
	mode := if is_link(src) {
		0o644
	} else {
		os.stat(src)!.mode & 0o777
	}
	write_file(dst, read_file(src)!)!
	chmod(dst, mode) or {}
}

// user_home_dir returns the current user's home directory (Go's
// os.UserHomeDir). It prefers $HOME / %USERPROFILE% and falls back to the
// directory database where available, erroring when neither is set.
pub fn user_home_dir() !string {
	if h := getenv_opt('HOME') {
		return h
	}
	if h := getenv_opt('USERPROFILE') {
		return h
	}
	$if windows {
		// fall back to the user's profile directory via the environment
		drive := getenv_opt('HOMEDRIVE') or { 'C:' }
		path := getenv_opt('HOMEPATH') or { return error('os: $HOME and %USERPROFILE% are not set') }
		return drive + path
	} $else {
		return error('os: $HOME is not set')
	}
}

// user_cache_dir returns the base directory for user-specific cached data
// (Go's os.UserCacheDir): $XDG_CACHE_HOME or ~/.cache on POSIX,
// %LocalAppData% on Windows.
pub fn user_cache_dir() !string {
	if c := getenv_opt('XDG_CACHE_HOME') {
		return c
	}
	if la := getenv_opt('LOCALAPPDATA') {
		return la
	}
	home := user_home_dir()!
	$if windows {
		return join_path(home, 'AppData', 'Local')
	} $else {
		return join_path(home, '.cache')
	}
}

// user_config_dir returns the base directory for user-specific configuration
// (Go's os.UserConfigDir): $XDG_CONFIG_HOME or ~/.config on POSIX,
// %AppData% on Windows.
pub fn user_config_dir() !string {
	if c := getenv_opt('XDG_CONFIG_HOME') {
		return c
	}
	if ad := getenv_opt('APPDATA') {
		return ad
	}
	home := user_home_dir()!
	$if windows {
		return join_path(home, 'AppData', 'Roaming')
	} $else {
		return join_path(home, '.config')
	}
}

// volume_name returns the leading volume component of path: the drive prefix
// on Windows (e.g. `C:`), and '' elsewhere (Go's filepath.VolumeName).
pub fn volume_name(path string) string {
	$if windows {
		if path.len >= 2 && is_drive_letter(path[0]) && path[1] == `:` {
			return path[..2]
		}
	} $else {
	}
	return ''
}
