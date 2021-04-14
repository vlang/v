module pref

import os

pub fn (prefs &Preferences) should_compile_filtered_files(dir string, files_ []string) []string {
	mut res := []string{}
	mut files := files_.clone()
	files.sort()
	mut all_v_files := []string{}
	for file in files {
		if !file.ends_with('.v') && !file.ends_with('.vh') {
			continue
		}
		if file.ends_with('_test.v')
			|| file.all_before_last('.v').all_before_last('.').ends_with('_test') {
			continue
		}
		if prefs.backend == .c && !prefs.should_compile_c(file) {
			continue
		}
		if prefs.backend == .js && !prefs.should_compile_js(file) {
			continue
		}
		if prefs.backend != .js && !prefs.should_compile_asm(file) {
			continue
		}
		if file.contains('_d_') {
			if prefs.compile_defines_all.len == 0 {
				continue
			}
			mut allowed := false
			for cdefine in prefs.compile_defines {
				file_postfix := '_d_${cdefine}.v'
				if file.ends_with(file_postfix) {
					allowed = true
					break
				}
			}
			if !allowed {
				continue
			}
		}
		if file.contains('_notd_') {
			mut allowed := true
			for cdefine in prefs.compile_defines {
				file_postfix := '_notd_${cdefine}.v'
				if file.ends_with(file_postfix) {
					allowed = false
					break
				}
			}
			if !allowed {
				continue
			}
		}
		all_v_files << os.join_path(dir, file)
	}
	//
	mut defaults := []string{}
	mut fnames_no_postfixes := map[string][]string{}
	for file in all_v_files {
		if file.contains('default.c.v') {
			defaults << file
		} else {
			res << file
			no_postfix_key := fname_without_platform_postfix(file)
			mut candidates := fnames_no_postfixes[no_postfix_key]
			candidates << file
			fnames_no_postfixes[no_postfix_key] = candidates
		}
	}
	for file in defaults {
		no_postfix_key := fname_without_platform_postfix(file)
		if no_postfix_key in fnames_no_postfixes {
			if prefs.is_verbose {
				println('>>> should_compile_filtered_files: skipping _default.c.v file $file ; the specialized versions are: ${fnames_no_postfixes[no_postfix_key]}')
			}
			continue
		}
		res << file
	}
	if prefs.is_verbose {
		println('>>> should_compile_filtered_files: res: $res')
	}
	return res
}

fn fname_without_platform_postfix(file string) string {
	res := file.replace_each([
		'default.c.v',
		'_',
		'nix.c.v',
		'_',
		'windows.c.v',
		'_',
		'linux.c.v',
		'_',
		'darwin.c.v',
		'_',
		'macos.c.v',
		'_',
		'android.c.v',
		'_',
		'freebsd.c.v',
		'_',
		'netbsd.c.v',
		'_',
		'dragonfly.c.v',
		'_',
		'solaris.c.v',
		'_',
		'x64.v',
		'_',
	])
	return res
}

pub fn (prefs &Preferences) should_compile_c(file string) bool {
	if file.ends_with('.js.v') {
		// Probably something like `a.js.v`.
		return false
	}
	if prefs.is_bare && file.ends_with('.freestanding.v') {
		return true
	}
	if prefs.os == .all {
		return true
	}
	if prefs.backend != .x64 && file.ends_with('_x64.v') {
		return false
	}
	if prefs.os != .windows && (file.ends_with('_windows.c.v') || file.ends_with('_windows.v')) {
		return false
	}
	if prefs.os != .linux && (file.ends_with('_linux.c.v') || file.ends_with('_linux.v')) {
		return false
	}
	if prefs.os != .macos && (file.ends_with('_darwin.c.v') || file.ends_with('_darwin.v')) {
		return false
	}
	if (file.ends_with('_ios.c.v') || file.ends_with('_ios.v')) && prefs.os != .ios {
		return false
	}
	if file.ends_with('_nix.c.v') && prefs.os == .windows {
		return false
	}
	if prefs.os != .macos && (file.ends_with('_macos.c.v') || file.ends_with('_macos.v')) {
		return false
	}
	if prefs.os == .windows && file.ends_with('_nix.c.v') {
		return false
	}
	if prefs.os != .android && file.ends_with('_android.c.v') {
		return false
	}
	if prefs.os != .freebsd && file.ends_with('_freebsd.c.v') {
		return false
	}
	if prefs.os != .openbsd && file.ends_with('_openbsd.c.v') {
		return false
	}
	if prefs.os != .netbsd && file.ends_with('_netbsd.c.v') {
		return false
	}
	if prefs.os != .dragonfly && file.ends_with('_dragonfly.c.v') {
		return false
	}
	if prefs.os != .solaris && file.ends_with('_solaris.c.v') {
		return false
	}
	return true
}

pub fn (prefs &Preferences) should_compile_asm(path string) bool {
	if path.count('.') != 2 || path.ends_with('c.v') || path.ends_with('js.v') {
		return true
	}
	file := path.all_before_last('.v')
	arch := arch_from_string(file.all_after_last('.')) or { Arch._auto }

	if arch != prefs.arch && prefs.arch != ._auto && arch != ._auto {
		return false
	}
	os := os_from_string(file.all_after_last('_').all_before('.')) or { OS._auto }

	if os != prefs.os && prefs.os != ._auto && os != ._auto {
		return false
	}
	return true
}

pub fn (prefs &Preferences) should_compile_js(file string) bool {
	if !file.ends_with('.js.v') && file.split('.').len > 2 {
		// Probably something like `a.c.v`.
		return false
	}
	return true
}
