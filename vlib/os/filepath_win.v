module os

// windows_volume returns the volume name from the given `path` on a Windows system.
// An empty string is returned if no Windows volume is present.
// NOTE: An error is returned if the current operating system is not Windows.
// Examples (on a Windows system):
// ```v
// assert os.windows_volume(r'C:\path\to\file.v') == 'C:'
// assert os.windows_volume('D:') == 'D:'
// assert os.windows_volume(r'\\Host\share\files\file.v') == '\\Host\share'
// ```
pub fn windows_volume(path string) ?string {
	$if !windows {
		return error(@FN + '() is only supported on a Windows system')
	}
	volume_len := win_volume_len(path)
	if volume_len == 0 {
		return empty_str
	}
	return path[..volume_len]
}
