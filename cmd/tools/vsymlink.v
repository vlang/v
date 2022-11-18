import os
import v.pref

$if windows {
	$if tinyc {
		#flag -ladvapi32
		#flag -luser32
	}
}

fn main() {
	C.atexit(cleanup_vtmp_folder)

	if os.args.len > 3 {
		print('usage: v symlink [OPTIONS]')
		exit(1)
	}

	ci_mode := '-githubci' in os.args

	vexe := os.real_path(pref.vexe_path())
	if ci_mode {
		setup_symlink_github()
	} else {
		$if windows {
			setup_symlink_windows(vexe)
		} $else {
			setup_symlink_unix(vexe)
		}
	}
}

fn cleanup_vtmp_folder() {
	os.rmdir_all(os.vtmp_dir()) or {}
}

fn setup_symlink_github() {
	// We append V's install location (which should
	// be the current directory) to the PATH environment variable.

	// Resources:
	// 1. https://docs.github.com/en/actions/using-workflows/workflow-commands-for-github-actions#environment-files
	// 2. https://docs.github.com/en/actions/using-workflows/workflow-commands-for-github-actions#setting-an-environment-variable
	mut content := os.read_file(os.getenv('GITHUB_PATH')) or {
		panic('Failed to read GITHUB_PATH.')
	}
	content += '\n${os.getwd()}\n'
	os.write_file(os.getenv('GITHUB_PATH'), content) or { panic('Failed to write to GITHUB_PATH.') }
}

fn setup_symlink_unix(vexe string) {
	mut link_path := '/data/data/com.termux/files/usr/bin/v'
	if !os.is_dir('/data/data/com.termux/files') {
		link_dir := '/usr/local/bin'
		if !os.exists(link_dir) {
			os.mkdir_all(link_dir) or { panic(err) }
		}
		link_path = link_dir + '/v'
	}
	os.rm(link_path) or {}
	os.symlink(vexe, link_path) or {
		eprintln('Failed to create symlink "${link_path}". Try again with sudo.')
		exit(1)
	}
}

fn setup_symlink_windows(vexe string) {
	$if windows {
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
		os.symlink(vsymlink, vexe) or {
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
		new_sys_env_path := new_paths.join(';')
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
		println('Done.')
		println('Note: Restart your shell/IDE to load the new %PATH%.')
		println('After restarting your shell/IDE, give `v version` a try in another directory!')
	}
}

fn warn_and_exit(err string) {
	eprintln(err)
	exit(1)
}

// get the system environment registry handle
fn get_reg_sys_env_handle() ?voidptr {
	$if windows { // wrap for cross-compile compat
		// open the registry key
		reg_key_path := 'Environment'
		reg_env_key := unsafe { nil } // or HKEY (HANDLE)
		if C.RegOpenKeyEx(os.hkey_current_user, reg_key_path.to_wide(), 0, 1 | 2, &reg_env_key) != 0 {
			return error('Could not open "${reg_key_path}" in the registry')
		}
		return reg_env_key
	}
	return error('not on windows')
}

// get a value from a given $key
fn get_reg_value(reg_env_key voidptr, key string) ?string {
	$if windows {
		// query the value (shortcut the sizing step)
		reg_value_size := u32(4095) // this is the max length (not for the registry, but for the system %PATH%)
		mut reg_value := unsafe { &u16(malloc(int(reg_value_size))) }
		if C.RegQueryValueExW(reg_env_key, key.to_wide(), 0, 0, reg_value, &reg_value_size) != 0 {
			return error('Unable to get registry value for "${key}".')
		}
		return unsafe { string_from_wide(reg_value) }
	}
	return error('not on windows')
}

// sets the value for the given $key to the given  $value
fn set_reg_value(reg_key voidptr, key string, value string) ?bool {
	$if windows {
		if C.RegSetValueExW(reg_key, key.to_wide(), 0, C.REG_EXPAND_SZ, value.to_wide(),
			value.len * 2) != 0 {
			return error('Unable to set registry value for "${key}". %PATH% may be too long.')
		}
		return true
	}
	return error('not on windows')
}

// Broadcasts a message to all listening windows (explorer.exe in particular)
// letting them know that the system environment has changed and should be reloaded
fn send_setting_change_msg(message_data string) ?bool {
	$if windows {
		if C.SendMessageTimeoutW(os.hwnd_broadcast, os.wm_settingchange, 0, unsafe { &u32(message_data.to_wide()) },
			os.smto_abortifhung, 5000, 0) == 0 {
			return error('Could not broadcast WM_SETTINGCHANGE')
		}
		return true
	}
	return error('not on windows')
}
