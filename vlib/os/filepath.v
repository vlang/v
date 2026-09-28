module os

import strings
import strings.textscanner

// Collection of useful functions for manipulation, validation and analysis of system paths.
// The following functions handle paths depending on the operating system,
// therefore results may be different for certain operating systems.

const fslash = `/`
const bslash = `\\`
const dot = `.`
const qmark = `?`
const fslash_str = '/'
const dot_dot = '..'
const empty_str = ''
const dot_str = '.'

// is_abs_path returns `true` if the given `path` is absolute.
pub fn is_abs_path(path string) bool {
	if path == '' {
		return false
	}
	$if windows {
		return is_unc_path(path) || is_drive_rooted(path) || is_normal_path(path)
	}
	return path[0] == fslash
}

// abs_path joins the current working directory with the given `path` (if the `path` is relative), and returns the absolute path representation.
pub fn abs_path(path string) string {
	wd := getwd()
	if path == '' {
		return wd
	}
	npath := norm_path(path)
	if npath == dot_str {
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
@[direct_array_access]
pub fn norm_path(path string) string {
	if path == '' {
		return dot_str
	}
	rooted := is_abs_path(path)
	// get the volume name from the path
	// if the current operating system is Windows
	volume_len := win_volume_len(path)
	mut volume := path[..volume_len]
	if volume_len != 0 && volume.contains(fslash_str) {
		volume = volume.replace(fslash_str, path_separator)
	}
	cpath := clean_path(path[volume_len..])
	if cpath == '' && volume_len == 0 {
		return dot_str
	}
	spath := cpath.split(path_separator)
	if dot_dot !in spath {
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
		if part == empty_str {
			continue
		}
		if part == dot_dot {
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
			sb.write_string(dot_dot)
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
			return dot_str
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
pub fn existing_path(path string) !string {
	err := error('path does not exist')
	if path == '' {
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
			if recent_path == '' {
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
	if path == '' {
		return empty_str
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
		// turn forward slash into a back slash on a Windows system
		$if windows {
			if curr == fslash {
				sb.write_u8(bslash)
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

// to_slash returns the result of replacing each separator character in path with a slash (`/`).
pub fn to_slash(path string) string {
	return $if windows {
		path.replace(path_separator, '/')
	} $else {
		path
	}
}

// parent_dir returns the parent directory of the given `path`, or an empty
// string when `path` has no parent. A path has no parent when it is a
// filesystem root (`/`, `C:\`, `\\server\share`, `\\?\UNC\server\share`), the
// current directory reference `.`, a single element with no directory in it
// (`file.v`), or a Windows drive relative path (`C:`, `C:file.v`,
// `C:dir\file.v`), which resolves against the current directory *of that
// drive* - state the caller cannot see, and which every one of its ancestors
// shares, so none of them is safe to hand back.
// A separator is any byte the platform accepts as one, so a Windows path may
// mix `/` and `\` freely, and trailing separators are ignored: they name the
// same directory, so `parent_dir('/a/b/')` is `/a`, exactly like `/a/b`.
//
// Every value parent_dir returns is safe to probe directly, and that is what
// separates it from `dir`, which has two Windows answers that resolve against
// a current directory rather than against the path they came from:
// `dir('C:')` is the relative `.`, and `dir(r'C:\outside')` is the bare volume
// `C:`, which names the current directory *on drive C*, not its root.
// parent_dir reports "no parent" for the first and the absolute root `C:\` for
// the second, so a parent directory walk can neither escape the drive nor
// probe a drive relative path on the way up.
pub fn parent_dir(path string) string {
	if path == '' {
		return empty_str
	}
	if is_drive_relative_path(path) {
		// `C:`, `C:file.v` and `C:dir\file.v` all resolve against the current
		// directory of drive C, and so does every ancestor of them, `C:dir`
		// included. There is no parent here that a caller could safely probe.
		return empty_str
	}
	root_len := win_root_len(path)
	// Trailing separators name the same directory, so they cannot select the
	// parent: without this, `/a/b/` would answer `/a/b` and a walk would probe
	// that directory twice, losing one ancestor to its iteration bound. Never
	// trim into a root though, which is nothing but a volume and a separator.
	mut end := path.len
	for end > root_len + 1 && is_slash(path[end - 1]) {
		end--
	}
	// Scan for the last separator instead of delegating to `dir`, which commits
	// to one separator kind for the whole path (`/` whenever the path holds any)
	// and so answers `C:` for the mixed `C:/one\two` that Windows accepts,
	// skipping the real parent `C:/one`.
	mut pos := -1
	for i := end - 1; i >= root_len; i-- {
		if is_slash(path[i]) {
			pos = i
			break
		}
	}
	if pos < 0 {
		// A single element with no directory in it, such as `file.v`.
		return empty_str
	}
	if pos == end - 1 {
		// Nothing but separators after the root: `path` is a root itself.
		return empty_str
	}
	if pos == root_len {
		// The parent is the root. Keep its separator, so the result is the
		// absolute `/` or `C:\`, never the drive relative `C:`.
		return path[..pos + 1]
	}
	return path[..pos]
}

// from_slash returns the result of replacing each slash (`/`) character is path with a separator character.
pub fn from_slash(path string) string {
	return $if windows {
		path.replace('/', path_separator)
	} $else {
		path
	}
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

// win_root_len returns the length of the leading part of `path` that has no
// parent directory. That is the Windows volume, except for an extended length
// UNC path (`\\?\UNC\server\share`): `win_volume_len` stops at the `\\?\UNC`
// tag, which does not name a location, so the server and the share belong to
// the root as well. Like `win_volume_len`, it is 0 outside Windows.
fn win_root_len(path string) int {
	volume_len := win_volume_len(path)
	if volume_len == 0 || !is_extended_unc_tag(path, volume_len) {
		return volume_len
	}
	// Consume `\server` and then `\share`. A path that ends before both are
	// present does not name a location either, so all of it is the root.
	mut i := volume_len
	for _ in 0 .. 2 {
		if i >= path.len || !is_slash(path[i]) {
			return path.len
		}
		i++
		for i < path.len && !is_slash(path[i]) {
			i++
		}
	}
	return i
}

// is_drive_relative_path reports whether `path` names a location relative to
// the current directory of a Windows drive: `C:`, `C:file.v`, `C:dir\file.v`.
// Windows keeps one current directory per drive, so such a path resolves
// against state a caller cannot see. Elsewhere `C:dir` is an ordinary file
// name, so this is Windows only.
fn is_drive_relative_path(path string) bool {
	$if !windows {
		return false
	}
	return has_drive_letter(path) && (path.len == 2 || !is_slash(path[2]))
}

// is_extended_unc_tag reports whether `win_volume_len` stopped at the `\\?\UNC`
// tag that introduces the server and share of an extended length UNC path.
fn is_extended_unc_tag(path string, volume_len int) bool {
	return volume_len == 7 && starts_w_slash_slash(path) && path[2] == qmark
		&& is_slash(path[3]) && (path[4] == `U` || path[4] == `u`)
		&& (path[5] == `N` || path[5] == `n`) && (path[6] == `C` || path[6] == `c`)
}

fn is_slash(b u8) bool {
	$if windows {
		return b == bslash || b == fslash
	}
	return b == fslash
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
	// vfmt off
	return (plen == 1 && is_slash(path[0])) || (plen >= 2 && is_slash(path[0]) && !is_slash(path[1]))
	// vfmt on
}

// is_curr_dir_ref returns `true` if the 3 given integer construct
// a reference to a current directory (.).
// NOTE: a negative integer means that no byte is present
fn is_curr_dir_ref(byte_one int, byte_two int, byte_three int) bool {
	if u8(byte_two) != dot {
		return false
	}
	return (byte_one < 0 || is_slash(u8(byte_one))) && (byte_three < 0 || is_slash(u8(byte_three)))
}
