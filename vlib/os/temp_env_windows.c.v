module os

@[unsafe]
fn C.GetEnvironmentVariableW(name &u16, buffer &u16, size u32) u32

// windows_env_lookup reads `name` from the process environment block, which is what
// GetTempPathW reads (the C runtime keeps its own copy for `getenv`).
fn windows_env_lookup(name string) ?string {
	wname := name.to_wide()
	defer {
		unsafe { free(wname) }
	}
	size := C.GetEnvironmentVariableW(wname, unsafe { nil }, 0)
	if size == 0 {
		return none
	}
	buf := []u16{len: int(size)}
	n := C.GetEnvironmentVariableW(wname, &u16(buf.data), size)
	if n == 0 || n >= size {
		return none
	}
	return unsafe { string_from_wide2(&u16(buf.data), int(n)) }
}

// windows_short_path_name returns the 8.3 short form of the existing `path`, or '' when
// there is none. It is the same path again when 8.3 names are turned off on the volume.
fn windows_short_path_name(path string) string {
	// GetShortPathNameW needs the `\\?\` form for a path past MAX_PATH.
	in_path := if windows_utf16_len(path) >= windows_temp_env_max_len && is_abs_path(path)
		&& !path.starts_with('\\\\') {
		'\\\\?\\' + path.replace('/', '\\')
	} else {
		path
	}
	winput := in_path.to_wide()
	defer {
		unsafe { free(winput) }
	}
	buf := []u16{len: 32768}
	n := C.GetShortPathNameW(winput, &u16(buf.data), u32(buf.len))
	if n == 0 || n >= u32(buf.len) {
		return ''
	}
	short := unsafe { string_from_wide2(&u16(buf.data), int(n)) }
	if short.starts_with('\\\\?\\') {
		// A `\\?\UNC\` result cannot become a plain path that is short enough.
		return if short.starts_with('\\\\?\\UNC\\') { '' } else { short[4..] }
	}
	return short
}

// windows_temp_env_candidates lists replacements for the too long `value` of `name`, best
// first: the same folder by its short name, then the user's own temporary folder, then the
// Windows one, which always fits.
fn windows_temp_env_candidates(name string, value string) []string {
	mut candidates := []string{}
	if name != 'USERPROFILE' {
		candidates << windows_short_path_name(value)
	}
	if local_app_data := windows_env_lookup('LOCALAPPDATA') {
		local_temp := local_app_data.trim_right('\\/') + '\\Temp'
		if is_dir(local_temp) {
			candidates << local_temp
			candidates << windows_short_path_name(local_temp)
		}
	}
	system_root := windows_env_lookup('SystemRoot') or { 'C:\\Windows' }
	candidates << system_root.trim_right('\\/') + '\\Temp'
	return candidates
}

// windows_avoid_long_temp_env shortens this process's TMP or TEMP when GetTempPathW would
// read a value past MAX_PATH from it. CreateProcessW calls GetTempPathW on the caller's own
// environment, so this must run before every program that this process starts; the
// children then inherit the short value.
fn windows_avoid_long_temp_env() {
	mut env := map[string]string{}
	for name in windows_temp_env_names {
		if value := windows_env_lookup(name) {
			env[name] = value
		}
	}
	change := windows_temp_env_change(env, windows_temp_env_candidates) or { return }
	setenv(change.name, change.value, true)
}
