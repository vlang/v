module os

// Collection of useful functions for manipulation, validation and analysis of system paths.
// The following functions handle paths depending on the operating system,
// therefore results may be different for certain operating systems.

const (
	fslash = `/`
	bslash = `\\`
	dot    = `.`
	is_win = user_os() == 'windows'
)

// is_abs_path returns `true` if the given `path` is absolute.
pub fn is_abs_path(path string) bool {
	if path.len == 0 {
		return false
	}
	if os.is_win {
		return is_device_path(path) || is_drive_rooted(path) || is_normal_path(path)
	}
	return path[0] == os.fslash
}

// win_volume_len returns the length of the
// Windows volume/drive from the given `path`.
fn win_volume_len(path string) int {
	plen := path.len
	if plen < 2 {
		return 0
	}
	if has_drive_letter(path) {
		return 2
	}
	// its UNC path / DOS device path?
	if path.len >= 5 && starts_w_slash_slash(path) && !is_slash(path[2]) {
		for i := 3; i < plen; i++ {
			if is_slash(path[i]) {
				if i + 1 >= plen || is_slash(path[i + 1]) {
					break
				}
				i++
				for ; i < plen; i++ {
					if is_slash(path[i]) {
						return i
					}
				}
				return i
			}
		}
	}
	return 0
}

fn is_slash(b u8) bool {
	return b == os.fslash || (os.is_win && b == os.bslash)
}

fn is_device_path(path string) bool {
	return win_volume_len(path) >= 5 && starts_w_slash_slash(path)
}

fn has_drive_letter(path string) bool {
	return path.len >= 2 && path[0].is_letter() && path[1] == `:`
}

fn starts_w_slash_slash(path string) bool {
	return path.len >= 2 && is_slash(path[0]) && is_slash(path[1])
}

fn is_drive_rooted(path string) bool {
	return path.len >= 3 && has_drive_letter(path) && is_slash(path[2])
}

// is_normal_path returns `true` if the given
// `path` is NOT a network or Windows device path.
fn is_normal_path(path string) bool {
	plen := path.len
	if plen == 0 {
		return false
	}
	return (plen == 1 && is_slash(path[0])) || (plen >= 2 && is_slash(path[0])
		&& !is_slash(path[1]))
}
