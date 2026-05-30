module pref

pub fn normalize_target_os_name(target_os string) string {
	return match target_os.to_lower() {
		'darwin', 'mac' { 'macos' }
		else { target_os.to_lower() }
	}
}

fn normalize_current_os_name(current_os string) string {
	return normalize_target_os_name(current_os)
}

fn file_has_termux_os_suffix(file string) bool {
	return file.contains('_termux.') && !file.contains('_android_outside_termux.')
}

fn file_has_known_os_suffix(file string) bool {
	return file.contains('_nix.') || file.contains('_windows.') || file.contains('_linux.')
		|| file.contains('_macos.') || file.contains('_darwin.') || file.contains('_bsd.')
		|| file.contains('_android') || file_has_termux_os_suffix(file) || file.contains('_ios.')
		|| file.contains('_freebsd.') || file.contains('_openbsd.') || file.contains('_netbsd.')
		|| file.contains('_dragonfly.') || file.contains('_solaris.') || file.contains('_qnx.')
		|| file.contains('_serenity.') || file.contains('_plan9.') || file.contains('_vinix.')
}

// file_has_incompatible_os_suffix reports whether file is specialized for a different OS.
pub fn file_has_incompatible_os_suffix(file string, current_os string) bool {
	os_name := normalize_current_os_name(current_os)
	if os_name == 'none' {
		return file_has_known_os_suffix(file)
	}
	if os_name == 'windows' && file.contains('_nix.') {
		return true
	}
	if os_name != 'windows' && file.contains('_windows.') {
		return true
	}
	if os_name != 'linux' && file.contains('_linux.') {
		return true
	}
	if os_name != 'macos' && (file.contains('_macos.') || file.contains('_darwin.')) {
		return true
	}
	if os_name !in ['macos', 'freebsd', 'openbsd', 'netbsd', 'dragonfly'] && file.contains('_bsd.') {
		return true
	}
	if os_name != 'android' && file.contains('_android') {
		return true
	}
	if os_name != 'termux' && file_has_termux_os_suffix(file) {
		return true
	}
	if os_name != 'ios' && file.contains('_ios.') {
		return true
	}
	if os_name != 'freebsd' && file.contains('_freebsd.') {
		return true
	}
	if os_name != 'openbsd' && file.contains('_openbsd.') {
		return true
	}
	if os_name != 'netbsd' && file.contains('_netbsd.') {
		return true
	}
	if os_name != 'dragonfly' && file.contains('_dragonfly.') {
		return true
	}
	if os_name != 'solaris' && file.contains('_solaris.') {
		return true
	}
	if os_name != 'qnx' && file.contains('_qnx.') {
		return true
	}
	if os_name != 'serenity' && file.contains('_serenity.') {
		return true
	}
	if os_name != 'plan9' && file.contains('_plan9.') {
		return true
	}
	if os_name != 'vinix' && file.contains('_vinix.') {
		return true
	}
	return false
}
