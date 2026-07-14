import os

$if tinyc {
	#flag -ladvapi32
	#flag -luser32
}

fn setup_symlink(custom_link_dir string) {
	// Create a symlink in a local folder (by default .\.bin\v.exe).
	// Puts `v` in %PATH% without polluting it with anything else (like makev.bat).
	// This will make `v` available on cmd.exe, PowerShell, and MinGW(MSYS)/WSL/Cygwin
	vsymlinkdir := normalized_link_dir(custom_link_dir)
	mut vsymlink := symlink_path(vsymlinkdir)
	// Remove old symlink first (v could have been moved, symlink rerun)
	if !os.exists(vsymlinkdir) {
		os.mkdir_all(vsymlinkdir) or { panic(err) }
	} else {
		if os.exists(vsymlink) {
			os.rm(vsymlink) or { panic(err) }
		} else {
			vsymlink = os.join_path(vsymlinkdir, 'v.bat')
			if os.exists(vsymlink) {
				os.rm(vsymlink) or { panic(err) }
			}
			vsymlink = os.join_path(vsymlinkdir, 'v.exe')
		}
	}
	// First, try to create a native symlink in the configured directory.
	os.symlink(vexe, vsymlink) or {
		// typically only fails if you're on a network drive (VirtualBox)
		// do batch file creation instead
		eprintln('Could not create a native symlink: ${err}')
		eprintln('Creating a batch file instead...')
		vsymlink = os.join_path(vsymlinkdir, 'v.bat')
		if os.exists(vsymlink) {
			os.rm(vsymlink) or { panic(err) }
		}
		os.write_file(vsymlink, '@echo off\n"${vexe}" %*') or { panic(err) }
		eprintln('${vsymlink} file written.')
	}
	if !os.exists(vsymlink) {
		warn_and_exit('Could not create ${vsymlink}')
	}
	println('Symlink ${vsymlink} to ${vexe} created.')
	println('Checking system %PATH%...')
	reg_sys_env_handle := get_reg_sys_env_handle() or {
		warn_and_exit(err.msg())
		return
	}
	// TODO: Fix defers inside ifs
	// defer {
	// C.RegCloseKey(reg_sys_env_handle)
	// }
	sys_env_path := get_reg_value(reg_sys_env_handle, 'Path') or {
		C.RegCloseKey(reg_sys_env_handle)
		warn_and_exit(err.msg())
		return
	}
	new_sys_env_path := append_path_entry(sys_env_path, vsymlinkdir, os.path_delimiter, true)
	if new_sys_env_path == sys_env_path {
		println('System %PATH% was already configured.')
	} else {
		println('System %PATH% was not configured.')
		println('Adding symlink directory to system %PATH%...')
		set_reg_value(reg_sys_env_handle, 'Path', new_sys_env_path) or {
			C.RegCloseKey(reg_sys_env_handle)
			warn_and_exit(err.msg())
		}
		println('Done.')
	}
	println('Notifying running processes to update their Environment...')
	send_setting_change_msg('Environment') or {
		eprintln(err)
		C.RegCloseKey(reg_sys_env_handle)
		warn_and_exit('You might need to run this again to have the `v` command in your %PATH%')
	}
	C.RegCloseKey(reg_sys_env_handle)
	if os.getenv('GITHUB_JOB') != '' {
		// Append V's install location to GITHUBs PATH environment variable.
		// Resources:
		// 1. https://docs.github.com/en/actions/using-workflows/workflow-commands-for-github-actions#environment-files
		// 2. https://docs.github.com/en/actions/using-workflows/workflow-commands-for-github-actions#setting-an-environment-variable
		mut content := os.read_file(os.getenv('GITHUB_PATH')) or {
			eprintln('The `GITHUB_PATH` env variable is not defined.')
			exit(1)
		}
		content += '\n${new_sys_env_path}\n'
		os.write_file(os.getenv('GITHUB_PATH'), content) or {
			panic('Failed to write to GITHUB_PATH.')
		}
	}
	println('Done.')
	println('Note: Restart your shell/IDE to load the new %PATH%.')
	println('After restarting your shell/IDE, give `v version` a try in another directory!')
}

fn default_link_dir() string {
	vdir := os.real_path(os.dir(vexe))
	return os.join_path(vdir, '.bin')
}

fn symlink_path(link_dir string) string {
	return os.join_path(link_dir, 'v.exe')
}

fn append_path_entry(path_value string, entry string, delimiter string, case_insensitive bool) string {
	target := normalized_path_entry(entry, case_insensitive)
	for current_entry in path_value.split(delimiter) {
		if normalized_path_entry(current_entry, case_insensitive) == target {
			return path_value
		}
	}
	if path_value == '' || path_value.ends_with(delimiter) {
		return path_value + entry
	}
	return path_value + delimiter + entry
}

fn normalized_path_entry(entry string, case_insensitive bool) string {
	normalized := entry.trim_space().trim_right('/\\')
	if case_insensitive {
		return normalized.to_lower_ascii()
	}
	return normalized
}

fn warn_and_exit(err string) {
	eprintln(err)
	exit(1)
}

// get the system environment registry handle
fn get_reg_sys_env_handle() !voidptr {
	// open the registry key
	reg_key_path := 'Environment'
	mut reg_env_key := os.hkey_current_user // placeholder, overwritten by RegOpenKeyEx
	if C.RegOpenKeyEx(os.hkey_current_user, reg_key_path.to_wide(), 0, 1 | 2, voidptr(&reg_env_key)) != 0 {
		return error('Could not open "${reg_key_path}" in the registry')
	}
	return reg_env_key
}

// get a value from a given $key
fn get_reg_value(reg_env_key voidptr, key string) !string {
	mut reg_value_size := u32(0)
	query_result := C.RegQueryValueExW(reg_env_key, key.to_wide(), 0, 0, 0,
		voidptr(&reg_value_size))
	if query_result == C.ERROR_FILE_NOT_FOUND {
		return ''
	}
	if query_result != 0 {
		return error('Unable to get registry value for "${key}" (error ${query_result}).')
	}
	if reg_value_size == 0 {
		return ''
	}
	reg_value_cap := int((reg_value_size + u32(sizeof(u16)) - 1) / u32(sizeof(u16)))
	mut reg_value := []u16{len: reg_value_cap + 1}
	read_result := C.RegQueryValueExW(reg_env_key, key.to_wide(), 0, 0, &u8(reg_value.data),
		voidptr(&reg_value_size))
	if read_result != 0 {
		return error('Unable to get registry value for "${key}" (error ${read_result}).')
	}
	return unsafe { string_from_wide(reg_value.data) }
}

// sets the value for the given $key to the given  $value
fn set_reg_value(reg_key voidptr, key string, value string) !bool {
	wide_value := value.to_wide()
	wide_value_size := u32((C.wcslen(wide_value) + 1) * sizeof(u16))
	if C.RegSetValueExW(reg_key, key.to_wide(), 0, C.REG_EXPAND_SZ, voidptr(wide_value),
		wide_value_size) != 0 {
		return error('Unable to set registry value for "${key}". %PATH% may be too long.')
	}
	return true
}

// Broadcasts a message to all listening windows (explorer.exe in particular)
// letting them know that the system environment has changed and should be reloaded
fn send_setting_change_msg(message_data string) !bool {
	message_data_wide := message_data.to_wide()
	if C.SendMessageTimeoutW(os.hwnd_broadcast, os.wm_settingchange, 0,
		unsafe { &u32(message_data_wide) }, os.smto_abortifhung, 5000, 0) == 0 {
		return error('Could not broadcast WM_SETTINGCHANGE')
	}
	return true
}
