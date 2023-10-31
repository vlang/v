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
