module os

import strings
import strings.textscanner

// Collection of useful functions for manipulation, validation and analysis of system paths.
// The following functions handle paths depending on the operating system,
// therefore results may be different for certain operating systems.

const (
	fslash    = `/`
	bslash    = `\\`
	dot       = `.`
	qmark     = `?`
	dot_dot   = '..'
	empty_str = ''
	dot_str   = '.'
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
	volume := get_volume(path)
	volume_len := volume.len
	cpath := clean_path(path[volume_len..])
	if cpath == os.dot_str && volume_len == 0 {
		return cpath
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
[direct_array_access]
pub fn existing_path(path string) string {
	if path.len == 0 {
		return os.empty_str
	}
	if exists(path) {
		return path
	}
	parts := logical_path_parts(path)
	for i, part in parts {
		if !exists(part) {
			if i - 1 < 0 {
				break
			}
			return parts[i - 1]
		}
	}
	return os.empty_str
}

// logical_path_parts returns the logical parts of the given `path`.
// Examples:
// on a unix-based system:
// ```v
// assert os.logical_path_parts('/path/to/file.v') == ['/', '/path', '/path/to', '/path/to/file.v']
// assert os.logical_path_parts('path/to/file.v') == ['path', 'path/to', 'path/to/file.v']
// ```
// on a Windows system:
// ```v
// assert os.logical_path_parts(r'C:\path\to\file.v') == ['C:\\', 'C:\\path', r'C:\path\to', r'C:\path\to\file.v']
// assert os.logical_path_parts(r'C:path\to\file.v') == ['C:', 'C:path', 'C:path\\to', r'C:path\to\file.v']
// ```
[direct_array_access]
fn logical_path_parts(path string) []string {
	if path.len == 0 {
		return []
	}
	volume := get_volume(path)
	volume_len := volume.len
	cpath := clean_path(path[volume_len..])
	if cpath == os.dot_str && volume_len == 0 {
		return [cpath]
	}
	full_path := if volume_len != 0 { volume + cpath } else { cpath }
	rooted := is_abs_path(full_path)
	mut parts := []string{}
	if volume_len != 0 {
		if rooted {
			parts << volume + path_separator
		} else {
			parts << volume
		}
	}
	if rooted && volume_len == 0 {
		parts << path_separator
	}
	spath := cpath.split(path_separator)
	for i, part in spath {
		if part == os.empty_str {
			continue
		}
		if parts.len == 0 {
			parts << part
			continue
		}
		if rooted && i == 1 {
			parts << parts[i - 1] + part
			continue
		}
		parts << parts[i - 1] + path_separator + part
	}
	return parts
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
		if (back == -1 || is_slash(u8(back))) && curr == os.dot
			&& (peek == -1 || is_slash(u8(peek))) {
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
	if res == os.empty_str {
		return os.dot_str
	}
	// eliminate the last path separator
	if res.len > 1 && is_slash(res[res.len - 1]) {
		return res[..res.len - 1]
	}
	return res
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

fn get_volume(path string) string {
	$if !windows {
		return os.empty_str
	}
	volume := path[..win_volume_len(path)]
	if volume.len == 0 {
		return os.empty_str
	}
	if volume[0] == os.fslash {
		return volume.replace('/', '\\')
	}
	return volume
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
