module cmdexec

const long_tail = 'd\\'.repeat(140) + 'x.exe'

fn test_extended_length_path_prefixes_a_long_drive_path() {
	long := 'S:\\' + long_tail
	assert windows_extended_length_path_for(long) == '\\\\?\\' + long
	assert windows_extended_length_path_for(long.replace('\\', '/')) == '\\\\?\\' + long
	assert windows_extended_length_path_for('S:\\short\\x.exe') == 'S:\\short\\x.exe'
}

fn test_extended_length_path_uses_the_unc_form_for_a_share() {
	unc := '\\\\server\\share\\' + long_tail
	assert windows_extended_length_path_for(unc) == '\\\\?\\UNC\\server\\share\\' + long_tail
}

fn test_extended_length_path_keeps_paths_that_are_not_fully_qualified() {
	// `\\?\` needs a fully qualified path: relative, root-relative and drive-relative
	// paths keep their meaning only without it.
	for path in ['r\\'.repeat(140) + 'x.exe', '\\' + long_tail, 'S:' + long_tail] {
		assert windows_extended_length_path_for(path) == path
	}
	// Paths that already use a prefix are left alone.
	for path in ['\\\\?\\S:\\' + long_tail, '\\\\.\\S:\\' + long_tail] {
		assert windows_extended_length_path_for(path) == path
	}
}

fn test_extended_length_path_normalizes_dot_parts() {
	// Windows takes a `\\?\` path literally, so `.` and `..` must be resolved first.
	dotted := 'S:\\a\\.\\b\\..\\' + long_tail
	assert windows_extended_length_path_for(dotted) == '\\\\?\\S:\\a\\' + long_tail
	unc := '\\\\server\\share\\..\\..\\' + long_tail
	assert windows_extended_length_path_for(unc) == '\\\\?\\UNC\\server\\share\\' + long_tail
}

fn test_extended_length_path_counts_utf16_units() {
	// 130 characters above U+FFFF are 260 UTF-16 units, 263 with `S:\`: past the limit,
	// although the path has only 133 runes.
	wide := 'S:\\' + '\U0001F600'.repeat(130)
	assert windows_extended_length_path_for(wide) == '\\\\?\\' + wide
	// 200 two-byte characters are 403 bytes but only 203 units: within the limit.
	accented := 'S:\\' + 'é'.repeat(200)
	assert accented.len >= 260
	assert windows_extended_length_path_for(accented) == accented
}
