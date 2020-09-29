module pref

import os

pub fn (prefs &Preferences) should_compile_filtered_files(dir string, files_ []string) []string {
	mut res := []string{}
	mut files := files_.clone()
	files.sort()
	for file in files {
		if !file.ends_with('.v') && !file.ends_with('.vh') {
			continue
		}
		if file.ends_with('_test.v') {
			continue
		}
		if prefs.backend == .c && !prefs.should_compile_c(file) {
			continue
		}
		if prefs.backend == .js && !prefs.should_compile_js(file) {
			continue
		}
		if prefs.compile_defines_all.len > 0 && file.contains('_d_') {
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
		res << os.join_path(dir, file)
	}
	return res
}

pub fn (prefs &Preferences) should_compile_c(file string) bool {
	if !file.ends_with('.c.v') && file.split('.').len > 2 {
		// Probably something like `a.js.v`.
		return false
	}
	if (file.ends_with('_windows.c.v') || file.ends_with('_windows.v')) && prefs.os != .windows {
		return false
	}
	if (file.ends_with('_linux.c.v') || file.ends_with('_linux.v')) && prefs.os != .linux {
		return false
	}
	if (file.ends_with('_darwin.c.v') || file.ends_with('_darwin.v')) && prefs.os != .macos {
		return false
	}
	if (file.ends_with('_macos.c.v') || file.ends_with('_macos.v')) && prefs.os != .macos {
		return false
	}
	if file.ends_with('_nix.c.v') && prefs.os == .windows {
		return false
	}
	if file.ends_with('_android.c.v') && prefs.os != .android {
		return false
	}
	if file.ends_with('_freebsd.c.v') && prefs.os != .freebsd {
		return false
	}
	if file.ends_with('_openbsd.c.v') && prefs.os != .openbsd {
		return false
	}
	if file.ends_with('_netbsd.c.v') && prefs.os != .netbsd {
		return false
	}
	if file.ends_with('_dragonfly.c.v') && prefs.os != .dragonfly {
		return false
	}
	if file.ends_with('_solaris.c.v') && prefs.os != .solaris {
		return false
	}
	if file.ends_with('_x64.v') && prefs.backend != .x64 {
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
