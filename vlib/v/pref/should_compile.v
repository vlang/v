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
		if prefs.backend in [.c, .interpret] && !prefs.should_compile_c(file) {
			continue
		}
		if prefs.backend.is_js() && !prefs.should_compile_js(file) {
			continue
		}
		if prefs.backend == .native && !prefs.should_compile_native(file) {
			continue
		}
		if !prefs.backend.is_js() && !prefs.should_compile_asm(file) {
			continue
		}
		if file.starts_with('.#') {
			continue
		}
		if !prefs.prealloc && !prefs.output_cross_c && file.ends_with('prealloc.c.v') {
			continue
		}
		if prefs.nofloat && file.ends_with('float.c.v') {
			continue
		}
		if file.contains('_d_') {
			if prefs.compile_defines_all.len == 0 {
				continue
			}
			mut allowed := false
			for cdefine in prefs.compile_defines {
				file_postfixes := ['_d_${cdefine}.v', '_d_${cdefine}.c.v']
				for file_postfix in file_postfixes {
					if file.ends_with(file_postfix) {
						allowed = true
						break
					}
				}
				if allowed {
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
				file_postfixes := ['_notd_${cdefine}.v', '_notd_${cdefine}.c.v']
				for file_postfix in file_postfixes {
					if file.ends_with(file_postfix) {
						allowed = false
						break
					}
				}
				if !allowed {
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
		// println('>>> prefs: $prefs')
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
		'termux.c.v',
		'_',
		'android_outside_termux.c.v',
		'_',
		'freebsd.c.v',
		'_',
		'openbsd.c.v',
		'_',
		'netbsd.c.v',
		'_',
		'dragonfly.c.v',
		'_',
		'solaris.c.v',
		'_',
		'native.v',
		'_',
	])
	return res
}

pub fn (prefs &Preferences) should_compile_native(file string) bool {
	// allow custom filtering for native backends,
	// but if there are no other rules, default to the c backend rules
	return prefs.should_compile_c(file)
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
	if prefs.backend != .native && file.ends_with('_native.v') {
		return false
	}
	if prefs.building_v && prefs.output_cross_c && file.ends_with('_windows.v') {
		// TODO temp hack to make msvc_windows.v work with -os cross
		return true
	}
	if prefs.os == .windows && (file.ends_with('_nix.c.v') || file.ends_with('_nix.v')) {
		return false
	}
	if prefs.os != .windows && (file.ends_with('_windows.c.v') || file.ends_with('_windows.v')) {
		return false
	}
	//
	if prefs.os != .linux && (file.ends_with('_linux.c.v') || file.ends_with('_linux.v')) {
		return false
	}
	//
	if prefs.os != .macos && (file.ends_with('_darwin.c.v') || file.ends_with('_darwin.v')) {
		return false
	}
	if prefs.os != .macos && (file.ends_with('_macos.c.v') || file.ends_with('_macos.v')) {
		return false
	}
	//
	if prefs.os != .ios && (file.ends_with('_ios.c.v') || file.ends_with('_ios.v')) {
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
	if prefs.os != .serenity && file.ends_with('_serenity.c.v') {
		return false
	}
	if prefs.os != .vinix && file.ends_with('_vinix.c.v') {
		return false
	}
	if prefs.os in [.android, .termux] {
		// Note: Termux is running natively on Android devices, but the compilers there (clang) usually do not have access
		// to the Android SDK. The code here ensures that you can have `_termux.c.v` and `_android_outside_termux.c.v` postfixes,
		// to target both the cross compilation case (where the SDK headers are used and available), and the Termux case,
		// where the Android SDK is not used.
		if file.ends_with('_android.c.v') {
			// common case, should compile for both cross android and termux
			// eprintln('prefs.os: $prefs.os | file: $file | common')
			return true
		}
		if file.ends_with('_android_outside_termux.c.v') {
			// compile code that targets Android, but NOT Termux (i.e. the SDK is available)
			// eprintln('prefs.os: $prefs.os | file: $file | android_outside_termux')
			return prefs.os == .android
		}
		if file.ends_with('_termux.c.v') {
			// compile Termux specific code
			// eprintln('prefs.os: $prefs.os | file: $file | termux specific')
			return prefs.os == .termux
		}
	} else if file.ends_with('_android.c.v') || file.ends_with('_termux.c.v')
		|| file.ends_with('_android_outside_termux.c.v') {
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
