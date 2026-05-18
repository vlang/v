module pref

fn normalize_current_os_name(current_os string) string {
	return match current_os.to_lower() {
		'darwin', 'mac' { 'macos' }
		else { current_os.to_lower() }
	}
}

// file_has_incompatible_os_suffix reports whether file is specialized for a different OS.
pub fn file_has_incompatible_os_suffix(file string, current_os string) bool {
	os_name := normalize_current_os_name(current_os)
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
	if os_name != 'android' && file.contains('_android') {
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
