module os

fn test_avoid_long_temp_env_rewrites_the_process_environment() {
	old_tmp := windows_env_lookup('TMP') or { '' }
	defer {
		setenv('TMP', old_tmp, true)
	}
	long_tmp := 'C:\\' + 'd'.repeat(windows_temp_env_max_len)
	setenv('TMP', long_tmp, true)
	assert windows_env_lookup('TMP') or { '' } == long_tmp
	windows_avoid_long_temp_env()
	fixed := windows_env_lookup('TMP') or { '' }
	assert fixed != '' && windows_utf16_len(fixed) <= windows_temp_env_max_len
	// The C runtime's copy, which `getenv` reads, agrees with the environment block.
	assert getenv('TMP') == fixed
}

fn test_short_path_name_strips_the_extended_length_prefix() {
	root := join_path(vtmp_dir(), 'temp_env_short_${getpid()}')
	defer {
		rmdir_all(root) or {}
	}
	deep := join_path(root, 'b'.repeat(120), 'f'.repeat(150))
	mkdir_all(deep) or {
		eprintln('skipped: cannot create a path past MAX_PATH here: ${err}')
		return
	}
	short := windows_short_path_name(deep)
	assert short != ''
	assert !short.starts_with('\\\\?\\')
	assert is_dir(short)
}
