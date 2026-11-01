module os

fn C.SetEnvironmentVariableW(name &u16, value &u16) i32

// clearenv removes all environment variables from the current process.
// It is safe to call repeatedly, including when the environment is already empty.
pub fn clearenv() {
	$if windows {
		block := C.GetEnvironmentStringsW()
		if block == unsafe { nil } {
			return
		}
		environment := clearenv_windows_environment(block)
		C.FreeEnvironmentStringsW(block)
		// Initialize and snapshot the CRT table before unsetenv can reallocate it.
		getenv('PATH')
		crt_environment := unsafe { &&u16(voidptr(C._wenviron)) }
		for name in clearenv_windows_crt_names(crt_environment) {
			unsetenv(name)
		}
		for name, _ in environment {
			// Win32 also contains hidden names and names absent from the CRT cache.
			wide_name := name.to_wide()
			C.SetEnvironmentVariableW(wide_name, unsafe { nil })
			unsafe { free(voidptr(wide_name)) }
		}
	} $else {
		if unsafe { voidptr(C.environ) } == unsafe { nil } {
			return
		}
		for key, _ in environ() {
			unsetenv(key)
		}
	}
}

// Read the complete Windows block, including hidden per-drive entries omitted by environ().
fn clearenv_windows_environment(block &u16) map[string]string {
	mut environment := map[string]string{}
	if block == unsafe { nil } {
		return environment
	}
	mut p := unsafe { block }
	for unsafe { *p } != 0 {
		mut end := unsafe { p }
		for unsafe { *end } != 0 {
			unsafe { end = end + 1 }
		}
		entry := unsafe { string_from_wide(p) }
		// A leading '=' belongs to a hidden name, not to its separator.
		if separator := entry.index_after('=', 1) {
			environment[entry[..separator]] = entry[separator + 1..]
		}
		unsafe { p = end + 1 }
	}
	return environment
}

fn clearenv_windows_crt_names(environment &&u16) []string {
	mut names := []string{}
	if environment == unsafe { nil } {
		return names
	}
	mut i := 0
	for unsafe { environment[i] } != unsafe { nil } {
		entry := unsafe { string_from_wide(environment[i]) }
		if separator := entry.index('=') {
			if separator > 0 {
				names << entry[..separator]
			}
		}
		i++
	}
	return names
}
