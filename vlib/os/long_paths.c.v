module os

// Windows long-path policy (`LongPathsEnabled`).
//
// The Win32 and CRT file APIs refuse paths reaching past MAX_PATH (260
// UTF-16 code units) unless the path uses the extended-length (`\\?\`) form,
// the process is long-path aware, *and* the machine policy allows it. V
// executables are long-path aware (see v.exe.manifest), and `os` retries long
// deletions and listings through the extended form, but creating and opening
// such paths still needs the policy. It is machine-wide, needs administrator
// rights to change, and is what the Python installer toggles with its
// "Disable path length limit" option.

// long_paths_registry_subkey is the registry key holding the machine
// Win32 long-path policy.
const long_paths_registry_subkey = r'SYSTEM\CurrentControlSet\Control\FileSystem'

// long_paths_registry_value is the policy value: a REG_DWORD, nonzero when
// Win32 long paths are enabled.
const long_paths_registry_value = 'LongPathsEnabled'

// LongPathsStatus is what `long_paths_status` reports about the machine
// Win32 long-path policy.
pub enum LongPathsStatus {
	enabled
	disabled
	unknown
}

// long_paths_status reads the machine Win32 long-path policy. It never fails:
// a missing key or value means disabled, anything unreadable means unknown.
pub fn long_paths_status() LongPathsStatus {
	$if windows {
		return windows_long_paths_status()
	} $else {
		return .unknown
	}
}

// enable_long_paths turns the machine Win32 long-path policy on. It needs
// administrator rights: without them it fails with an access-denied error
// naming the manual command. New processes pick the policy up; when in doubt,
// restart the terminal (or sign out and back in).
pub fn enable_long_paths() ! {
	$if windows {
		windows_enable_long_paths()!
		return
	} $else {
		return error('`v enable-long-paths` is only supported on Windows')
	}
}

// long_paths_manual_command returns the elevated command that enables the
// machine Win32 long-path policy without this API, or '' where it does not
// apply. The quoting suits both cmd.exe and PowerShell.
pub fn long_paths_manual_command() string {
	$if windows {
		return 'reg add "HKLM\\${long_paths_registry_subkey}" /v ${long_paths_registry_value} /t REG_DWORD /d 1 /f'
	} $else {
		return ''
	}
}

$if windows {
	// long_paths_access_mask is the registry access used below, forced into
	// the 64-bit view: a 32-bit process would otherwise read and write the
	// redirected 32-bit copy of the policy.
	const long_paths_access_mask = key_query_value | 0x0100
	const long_paths_access_mask_set = key_set_value | 0x0100
	const long_paths_reg_dword = u32(4)
	const long_paths_error_access_denied = 5
	const long_paths_error_file_not_found = 2

	// windows_long_paths_status reads the policy value, mapping every failure
	// to disabled (absent key or value: the default is off) or unknown.
	fn windows_long_paths_status() LongPathsStatus {
		wsub := long_paths_registry_subkey.to_wide()
		defer {
			unsafe { free(wsub) }
		}
		wvalue := long_paths_registry_value.to_wide()
		defer {
			unsafe { free(wvalue) }
		}
		mut hkey := voidptr(nil)
		if C.RegOpenKeyExW(hkey_local_machine, wsub, 0, long_paths_access_mask, voidptr(&hkey)) != success {
			return .unknown
		}
		defer {
			C.RegCloseKey(hkey)
		}
		mut reserved := u32(0)
		mut typ := u32(0)
		mut raw := [4]u8{}
		mut size := u32(4)
		if C.RegQueryValueExW(hkey, wvalue, &reserved, &typ, &raw[0], &size) != success {
			// Absent value or key: long paths default to off.
			return .disabled
		}
		if typ != long_paths_reg_dword || size != 4 {
			return .unknown
		}
		if u32(raw[0]) | (u32(raw[1]) << 8) | (u32(raw[2]) << 16) | (u32(raw[3]) << 24) == 0 {
			return .disabled
		}
		return .enabled
	}

	// windows_enable_long_paths sets the policy value to 1.
	fn windows_enable_long_paths() ! {
		wsub := long_paths_registry_subkey.to_wide()
		defer {
			unsafe { free(wsub) }
		}
		wvalue := long_paths_registry_value.to_wide()
		defer {
			unsafe { free(wvalue) }
		}
		mut hkey := voidptr(nil)
		open_res := C.RegOpenKeyExW(hkey_local_machine, wsub, 0, long_paths_access_mask_set,
			voidptr(&hkey))
		if open_res == long_paths_error_access_denied {
			return error_with_code('could not enable Win32 long paths: access is denied',
				open_res)
		}
		if open_res == long_paths_error_file_not_found {
			return error_with_code('could not enable Win32 long paths: the policy key is missing (Windows 10 version 1607 or later is required)',
				open_res)
		}
		if open_res != success {
			return error_with_code('could not enable Win32 long paths: ' +
				get_error_msg(int(open_res)), int(open_res))
		}
		defer {
			C.RegCloseKey(hkey)
		}
		mut one := [4]u8{}
		one[0] = 1
		set_res := C.RegSetValueExW(hkey, wvalue, 0, long_paths_reg_dword, &one[0], 4)
		if set_res == long_paths_error_access_denied {
			return error_with_code('could not enable Win32 long paths: access is denied',
				set_res)
		}
		if set_res != success {
			return error_with_code('could not enable Win32 long paths: ' +
				get_error_msg(int(set_res)), int(set_res))
		}
	}
}
