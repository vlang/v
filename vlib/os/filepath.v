module os

import strings
import strings.textscanner

// Collection of useful functions for manipulation, validation and analysis of system paths.
// The following functions handle paths depending on the operating system,
// therefore results may be different for certain operating systems.

const (
	fslash     = `/`
	bslash     = `\\`
	dot        = `.`
	qmark      = `?`
	fslash_str = '/'
	dot_dot    = '..'
	empty_str  = ''
	dot_str    = '.'
)

// is_abs_path returns `true` if the given `path` is absolute.
pub fn is_abs_path(path string) bool {
	if path.len == 0 {
		return false
	}
	$if windows {
		return is_unc_path(path) || is_drive_rooted(path) || is_normal_path(path)
	}
	return path[0] == os.fslash
}

// abs_path joins the current working directory
// with the given `path` (if the `path` is relative)
// and returns the absolute path representation.
pub fn abs_path(path string) string {
	wd := getwd()
	if path.len == 0 {
		return wd
	}
	npath := norm_path(path)
	if npath == os.dot_str {
		return wd
	}
	if !is_abs_path(npath) {
		mut sb := strings.new_builder(npath.len)
		sb.write_string(wd)
		sb.write_string(path_separator)
		sb.write_string(npath)
		return norm_path(sb.str())
	}
	return npath
}

// norm_path returns the normalized version of the given `path`
// by resolving backlinks (..), turning forward slashes into
// back slashes on a Windows system and eliminating:
// - references to current directories (.)
// - redundant path separators
// - the last path separator
[direct_array_access]
pub fn norm_path(path string) string {
	if path.len == 0 {
		return os.dot_str
	}
	rooted := is_abs_path(path)
	// get the volume name from the path
	// if the current operating system is Windows
	volume_len := win_volume_len(path)
	mut volume := path[..volume_len]
	if volume_len != 0 && volume.contains(os.fslash_str) {
		volume = volume.replace(os.fslash_str, path_separator)
	}
	cpath := clean_path(path[volume_len..])
	if cpath.len == 0 && volume_len == 0 {
		return os.dot_str
	}
	spath := cpath.split(path_separator)
	if os.dot_dot !in spath {
		return if volume_len != 0 { volume + cpath } else { cpath }
	}
	// resolve backlinks (..)
	spath_len := spath.len
	mut sb := strings.new_builder(cpath.len)
	if rooted {
		sb.write_string(path_separator)
	}
	mut new_path := []string{cap: spath_len}
	mut backlink_count := 0
	for i := spath_len - 1; i >= 0; i-- {
		part := spath[i]
		if part == os.empty_str {
			continue
		}
		if part == os.dot_dot {
			backlink_count++
			continue
		}
		if backlink_count != 0 {
			backlink_count--
			continue
		}
		new_path.prepend(part)
	}
	// append backlink(s) to the path if backtracking
	// is not possible and the given path is not rooted
	if backlink_count != 0 && !rooted {
		for i in 0 .. backlink_count {
			sb.write_string(os.dot_dot)
			if new_path.len == 0 && i == backlink_count - 1 {
				break
			}
			sb.write_string(path_separator)
		}
	}
	sb.write_string(new_path.join(path_separator))
	res := sb.str()
	if res.len == 0 {
		if volume_len != 0 {
			return volume
		}
		if !rooted {
			return os.dot_str
		}
		return path_separator
	}
	if volume_len != 0 {
		return volume + res
	}
	return res
}

// existing_path returns the existing part of the given `path`.
// An error is returned if there is no existing part of the given `path`.
pub fn existing_path(path string) ?string {
	err := error('path does not exist')
	if path.len == 0 {
		return err
	}
	if exists(path) {
		return path
	}
	mut volume_len := 0
	$if windows {
		volume_len = win_volume_len(path)
	}
	if volume_len > 0 && is_slash(path[volume_len - 1]) {
		volume_len++
	}
	mut sc := textscanner.new(path[volume_len..])
	mut recent_path := path[..volume_len]
	for sc.next() != -1 {
		curr := u8(sc.current())
		peek := sc.peek()
		back := sc.peek_back()
		if is_curr_dir_ref(back, curr, peek) {
			continue
		}
		range := sc.ilen - sc.remaining() + volume_len
		if is_slash(curr) && !is_slash(u8(peek)) {
			recent_path = path[..range]
			continue
		}
		if !is_slash(curr) && (peek == -1 || is_slash(u8(peek))) {
			curr_path := path[..range]
			if exists(curr_path) {
				recent_path = curr_path
				continue
			}
			if recent_path.len == 0 {
				break
			}
			return recent_path
		}
	}
	return err
}

// clean_path returns the "cleaned" version of the given `path`
// by turning forward slashes into back slashes
// on a Windows system and eliminating:
// - references to current directories (.)
// - redundant separators
// - the last path separator
fn clean_path(path string) string {
	if path.len == 0 {
		return os.empty_str
	}
	mut sb := strings.new_builder(path.len)
	mut sc := textscanner.new(path)
	for sc.next() != -1 {
		curr := u8(sc.current())
		back := sc.peek_back()
		peek := sc.peek()
		// skip current path separator if last byte was a path separator
		if back != -1 && is_slash(u8(back)) && is_slash(curr) {
			continue
		}
		// skip reference to current dir (.)
		if is_curr_dir_ref(back, curr, peek) {
			// skip if the next byte is a path separator
			if peek != -1 && is_slash(u8(peek)) {
				sc.skip_n(1)
			}
			continue
		}
		// turn foward slash into a back slash on a Windows system
		$if windows {
			if curr == os.fslash {
				sb.write_u8(os.bslash)
				continue
			}
		}
		sb.write_u8(u8(sc.current()))
	}
	res := sb.str()
	// eliminate the last path separator
	if res.len > 1 && is_slash(res[res.len - 1]) {
		return res[..res.len - 1]
	}
	return res
}

// to_slash returns the result of replacing each separator character
// in path with a slash (`/`).
pub fn to_slash(path string) string {
	if path_separator == '/' {
		return path
	}
	return path.replace(path_separator, '/')
}

// from_slash returns the result of replacing each slash (`/`) character
// is path with a separator character.
pub fn from_slash(path string) string {
	if path_separator == '/' {
		return path
	}
	return path.replace('/', path_separator)
}

// win_volume_len returns the length of the
// Windows volume/drive from the given `path`.
fn win_volume_len(path string) int {
	$if !windows {
		return 0
	}
	plen := path.len
	if plen < 2 {
		return 0
	}
	if has_drive_letter(path) {
		return 2
	}
	// its UNC path / DOS device path?
	if plen >= 5 && starts_w_slash_slash(path) && !is_slash(path[2]) {
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
	$if windows {
		return b == os.bslash || b == os.fslash
	}
	return b == os.fslash
}

fn is_unc_path(path string) bool {
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

// is_curr_dir_ref returns `true` if the 3 given integer construct
// a reference to a current directory (.).
// NOTE: a negative integer means that no byte is present
fn is_curr_dir_ref(byte_one int, byte_two int, byte_three int) bool {
	if u8(byte_two) != os.dot {
		return false
	}
	return (byte_one < 0 || is_slash(u8(byte_one))) && (byte_three < 0 || is_slash(u8(byte_three)))
}
