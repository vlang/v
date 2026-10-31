module cmdexec

import os

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

// test_run_starts_an_executable_past_max_path launches a copy of a system tool from a
// directory deep enough that its path only starts through the `\\?\` prefix.
fn test_run_starts_an_executable_past_max_path() ! {
	$if windows {
		// Creating the directory needs this test binary to be longPathAware, which builds
		// without the manifest (MSVC, some cross builds) are not.
		if !os.read_file(os.executable())!.contains('longPathAware') {
			eprintln('skipped: this test binary is not longPathAware')
			return
		}
		// hostname.exe ships with every Windows version; fail rather than skip without it.
		tool := os.join_path(os.getenv('SystemRoot'), 'System32', 'hostname.exe')
		assert os.is_file(tool), 'missing ${tool}'
		root := os.join_path(os.vtmp_dir(), 'cmdexec_long_path_${os.getpid()}')
		dir := os.join_path(root, 'd'.repeat(120), 'e'.repeat(120))
		os.mkdir_all(dir) or {
			eprintln('skipped: cannot create ${dir} (is LongPathsEnabled off?): ${err}')
			return
		}
		defer {
			os.rmdir_all(root) or {}
		}
		exe := os.join_path(dir, 'hostname.exe')
		os.cp(tool, exe)!
		assert exe.len >= 260
		res := run(exe, [])
		assert res.exit_code == 0, res.output
		assert res.output.trim_space().len > 0
	}
}
