import os
import v.pref

const (
	hkey_current_user = voidptr(0x80000001)
	hwnd_broadcast = voidptr(0xffff)
)

$if windows {
	$if tinyc {
		#flag -lAdvapi32
		#flag -lUser32
	}
}

fn main(){
	$if windows {
		setup_symlink_on_windows()
	} $else {
		setup_symlink_on_unix()
	}
}

fn setup_symlink_on_unix(){
	vexe := pref.vexe_path()
	mut link_path := '/usr/local/bin/v'
	mut ret := os.exec('ln -sf $vexe $link_path') or {
		panic(err)
	}
	if ret.exit_code == 0 {
		println('Symlink "$link_path" has been created')
	} else if os.system("uname -o | grep -q \'[A/a]ndroid\'") == 0 {
		println('Failed to create symlink "$link_path". Trying again with Termux path for Android.')
		link_path = '/data/data/com.termux/files/usr/bin/v'
		ret = os.exec('ln -sf $vexe $link_path') or {
			panic(err)
		}
		if ret.exit_code == 0 {
			println('Symlink "$link_path" has been created')
		} else {
			println('Failed to create symlink "$link_path". Try again with sudo.')
		}
	} else {
		println('Failed to create symlink "$link_path". Try again with sudo.')
	}
}

fn setup_symlink_on_windows(){
	$if windows {
		vexe := pref.vexe_path()
		// NB: Putting $vdir directly into PATH will also result in
		// make.bat being global, which is NOT what we want.
		//
		// Instead, we create a small launcher v.bat, in a new local
		// folder .bin/ . That .bin/ folder can then be put in PATH
		// without poluting it with anything else - just a `v`
		// command will be available, similar to unix.
		//
		// Creating a real NTFS symlink to the real executable was also
		// tried, but then os.real_path( os.executable() ) returns the
		// path to the symlink, unfortunately, unlike on posix systems
		// ¯\_(ツ)_/¯
		vdir := os.real_path(os.dir(vexe))
		vsymlinkdir := os.join_path(vdir, '.bin')
		vsymlinkbat := os.join_path(vsymlinkdir, 'v.bat')
		if os.exists(vsymlinkbat) {
			print('Batch script $vsymlinkbat already exists, checking system %PATH%...')
		}
		else {
			os.rmdir_all(vsymlinkdir)
			os.mkdir_all(vsymlinkdir)
			os.write_file(vsymlinkbat, '@echo off\n${vexe} %*')
			if !os.exists(vsymlinkbat) {
				eprintln('Could not create $vsymlinkbat')
				exit(1)
			}
			else {
				print('Created $vsymlinkbat, checking system %PATH%...')
			}
		}

		reg_sys_env_handle  := get_reg_sys_env_handle() or {
			warn_and_exit(err)
			return
		}
		defer {
			C.RegCloseKey(reg_sys_env_handle)
		}

		// if the above succeeded, and we cannot get the value, it may simply be empty
		sys_env_path := get_reg_value(reg_sys_env_handle, 'Path') or { '' }

		current_sys_paths := sys_env_path.split(os.path_delimiter).map(it.trim('/$os.path_separator'))
		mut new_paths := [ vsymlinkdir ]
		for p in current_sys_paths {
			if p !in new_paths {
				new_paths << p
			}
		}

		new_sys_env_path := new_paths.join(';')

		if new_sys_env_path == sys_env_path {
			println('configured.')
		}
		else {
			print('not configured.\nSetting system %PATH%...')
			set_reg_value(reg_sys_env_handle, 'Path', new_sys_env_path) or {
				warn_and_exit(err)
				return
			}
			println('done.')
		}

		print('Letting running process know to update their Environment...')
		send_setting_change_msg('Environment') or {
			eprintln('\n' + err)
			warn_and_exit('You might need to run this again to have `v` in your %PATH%')
			return
		}

		println('finished.\n\nNote: restart your shell/IDE to load the new %PATH%.')
		println('\nAfter restarting your shell/IDE, give `v version` a try in another dir!')
	}
}

fn warn_and_exit(err string) {
	eprintln(err)
	exit(1)
}

// get the system environment registry handle
fn get_reg_sys_env_handle() ?voidptr {
	$if windows {
		// open the registry key
		reg_key_path   := 'Environment'
		reg_env_key    := voidptr(0) // or HKEY (HANDLE)
		if C.RegOpenKeyEx(hkey_current_user, reg_key_path.to_wide(), 0, 1 | 2, &reg_env_key) != 0 {
			return error('Could not open "$reg_key_path" in the registry')
		}

		return reg_env_key
	}
	return error('not on windows')
}

// get a value from a given $key
fn get_reg_value(reg_env_key voidptr, key string) ?string {
	$if windows {
		// query the value (shortcut the sizing step)
		reg_value_size := 4095 // this is the max length (not for the registry, but for the system %PATH%)
		mut reg_value  := &u16(malloc(reg_value_size))
		if C.RegQueryValueEx(reg_env_key, key.to_wide(), 0, 0, reg_value, &reg_value_size) != 0 {
			return error('Unable to get registry value for "$key", try rerunning as an Administrator')
		}

		return string_from_wide(reg_value)
	}
	return error('not on windows')
}

// sets the value for the given $key to the given  $value
fn set_reg_value(reg_key voidptr, key string, value string) ?bool {
	$if windows {
		if C.RegSetValueEx(reg_key, key.to_wide(), 0, 1, value.to_wide(), 4095) != 0 {
			return error('Unable to set registry value for "$key", are you running as an Administrator?')
		}

		return true
	}
	return error('not on windows')
}

// broadcasts a message to all listening windows (explorer.exe in particular)
// letting them know that the system environment has changed and should be reloaded
fn send_setting_change_msg(message_data string) ?bool {
	$if windows {
		if C.SendMessageTimeout(hwnd_broadcast, 0x001A, 0, message_data.to_wide(), 2, 5000, 0) == 0 {
			return error('Could not broadcast WM_SETTINGCHANGE')
		}
		return true
	}
	return error('not on windows')
}
