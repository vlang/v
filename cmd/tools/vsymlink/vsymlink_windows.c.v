import os

$if tinyc {
	#flag -ladvapi32
	#flag -luser32
}

fn setup_symlink() {
	// Create a symlink in a new local folder (.\.bin\.v.exe)
	// Puts `v` in %PATH% without polluting it with anything else (like make.bat).
	// This will make `v` available on cmd.exe, PowerShell, and MinGW(MSYS)/WSL/Cygwin
	vdir := os.real_path(os.dir(vexe))
	vsymlinkdir := os.join_path(vdir, '.bin')
	mut vsymlink := os.join_path(vsymlinkdir, 'v.exe')
	// Remove old symlink first (v could have been moved, symlink rerun)
	if !os.exists(vsymlinkdir) {
		os.mkdir(vsymlinkdir) or { panic(err) }
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
	// First, try to create a native symlink at .\.bin\v.exe
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
	// if the above succeeded, and we cannot get the value, it may simply be empty
	sys_env_path := get_reg_value(reg_sys_env_handle, 'Path') or { '' }
	current_sys_paths := sys_env_path.split(os.path_delimiter).map(it.trim('/${os.path_separator}'))
	mut new_paths := [vsymlinkdir]
	for p in current_sys_paths {
		if p == '' {
			continue
		}
		if p !in new_paths {
			new_paths << p
		}
	}
	new_sys_env_path := new_paths.join(os.path_delimiter)
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

fn warn_and_exit(err string) {
	eprintln(err)
	exit(1)
}

// get the system environment registry handle
fn get_reg_sys_env_handle() !voidptr {
	// open the registry key
	reg_key_path := 'Environment'
	reg_env_key := unsafe { nil } // or HKEY (HANDLE)
	if C.RegOpenKeyEx(os.hkey_current_user, reg_key_path.to_wide(), 0, 1 | 2, &reg_env_key) != 0 {
		return error('Could not open "${reg_key_path}" in the registry')
	}
	return reg_env_key
}

// get a value from a given $key
fn get_reg_value(reg_env_key voidptr, key string) !string {
	// query the value (shortcut the sizing step)
	reg_value_size := u32(4095) // this is the max length (not for the registry, but for the system %PATH%)
	mut reg_value := unsafe { &u16(malloc(int(reg_value_size))) }
	if C.RegQueryValueExW(reg_env_key, key.to_wide(), 0, 0, reg_value, &reg_value_size) != 0 {
		return error('Unable to get registry value for "${key}".')
	}
	return unsafe { string_from_wide(reg_value) }
}

// sets the value for the given $key to the given  $value
fn set_reg_value(reg_key voidptr, key string, value string) !bool {
	if C.RegSetValueExW(reg_key, key.to_wide(), 0, C.REG_EXPAND_SZ, value.to_wide(), value.len * 2) != 0 {
		return error('Unable to set registry value for "${key}". %PATH% may be too long.')
	}
	return true
}

// Broadcasts a message to all listening windows (explorer.exe in particular)
// letting them know that the system environment has changed and should be reloaded
fn send_setting_change_msg(message_data string) !bool {
	if C.SendMessageTimeoutW(os.hwnd_broadcast, os.wm_settingchange, 0, unsafe { &u32(message_data.to_wide()) },
		os.smto_abortifhung, 5000, 0) == 0 {
		return error('Could not broadcast WM_SETTINGCHANGE')
	}
	return true
}
