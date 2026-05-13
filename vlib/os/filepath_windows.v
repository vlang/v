module os

// windows_volume returns the volume name from the given `path` on a Windows system.
// An empty string is returned if no Windows volume is present.
// Examples (on a Windows system):
// ```v
// assert os.windows_volume(r'C:\path\to\file.v') == 'C:'
// assert os.windows_volume('D:') == 'D:'
// assert os.windows_volume(r'\\Host\share\files\file.v') == r'\\Host\share'
// ```
pub fn windows_volume(path string) string {
	volume_len := win_volume_len(path)
	if volume_len == 0 {
		return empty_str
	}
	return path[..volume_len]
}

// trim_extended_length_path_prefix converts Win32 extended-length DOS/UNC
// paths returned by APIs like GetFinalPathNameByHandleW into regular paths.
// Paths that are not standard DOS or UNC paths are left unchanged.
fn trim_extended_length_path_prefix(path string) string {
	$if !windows {
		return path
	}
	if path.len < 4 || !starts_w_slash_slash(path) || path[2] != qmark || !is_slash(path[3]) {
		return path
	}
	if path.len >= 8 && (path[4] == `U` || path[4] == `u`) && (path[5] == `N` || path[5] == `n`)
		&& (path[6] == `C` || path[6] == `c`) && is_slash(path[7]) {
		return '\\\\' + path[8..]
	}
	if path.len >= 7 && path[4].is_letter() && path[5] == `:` && is_slash(path[6]) {
		return path[4..]
	}
	return path
}
