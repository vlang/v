// Copyright (c) 2020-2024 Joe Conigliaro. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module builder

import os
import v2.pref

fn list_dir_entries(path string) []string {
	return os.ls(path) or { []string{} }
}

pub fn get_v_files_from_dir(dir string, user_defines []string) []string {
	if dir == '' {
		return []string{}
	}
	mod_files := list_dir_entries(dir)
	mut v_files := []string{}
	mut defaults := []string{}
	for file in mod_files {
		if file == '' {
			continue
		}
		// Include .v files (including .c.v), exclude .js.v and test files
		if !file.ends_with('.v') || file.ends_with('.js.v') || file.contains('_test.') {
			continue
		}
		// skip platform-specific files
		if file.contains('.arm64.') || file.contains('.arm32.') || file.contains('.amd64.') {
			continue
		}
		// Skip files specialized for a different target OS before parsing/type checking.
		if pref.file_has_incompatible_os_suffix(file, os.user_os()) {
			continue
		}
		// Conditional compilation files: _d_<feature> included when -d <feature> is set,
		// _notd_<feature> included when -d <feature> is NOT set.
		if file.contains('_d_') {
			// Check if this is a _notd_ file (not-defined variant)
			if file.contains('_notd_') {
				// Include _notd_ files only when the feature is NOT defined
				feature := extract_define_feature(file, '_notd_')
				if feature.len > 0 && feature in user_defines {
					continue // feature is defined, skip the _notd_ variant
				}
			} else {
				// _d_ file (defined variant): include only when feature IS defined
				feature := extract_define_feature(file, '_d_')
				if feature.len == 0 || feature !in user_defines {
					continue // feature not defined, skip
				}
			}
		}
		path := os.join_path(dir, file)
		if path == '' {
			continue
		}
		// Collect _default.c.v files separately; they are only included when
		// no platform-specific variant exists (e.g. _darwin.c.v, _linux.c.v).
		if file.contains('default.c.v') {
			defaults << path
		} else {
			v_files << path
		}
	}
	// Add _default.c.v files only when no platform-specific variant was selected.
	for dfile in defaults {
		no_postfix := fname_without_platform_postfix(dfile)
		mut has_specialized := false
		for vf in v_files {
			if fname_without_platform_postfix(vf) == no_postfix {
				has_specialized = true
				break
			}
		}
		if !has_specialized {
			v_files << dfile
		}
	}
	return v_files
}

// fname_without_platform_postfix strips the platform-specific suffix from a file path,
// so that e.g. "free_memory_impl_darwin.c.v" and "free_memory_impl_default.c.v" both
// map to the same key, allowing detection of specialized vs default variants.
fn fname_without_platform_postfix(file string) string {
	return file.replace_each([
		'default.c.v', '_',
		'nix.c.v', '_',
		'windows.c.v', '_',
		'linux.c.v', '_',
		'darwin.c.v', '_',
		'macos.c.v', '_',
		'android.c.v', '_',
		'termux.c.v', '_',
		'android_outside_termux.c.v', '_',
		'freebsd.c.v', '_',
		'openbsd.c.v', '_',
		'netbsd.c.v', '_',
		'dragonfly.c.v', '_',
		'solaris.c.v', '_',
	])
}

// extract_define_feature extracts the feature name from a _d_ or _notd_ filename.
// e.g. "parse_d_parallel.v" with marker "_d_" returns "parallel"
// e.g. "array_notd_gcboehm_opt.v" with marker "_notd_" returns "gcboehm_opt"
fn extract_define_feature(file string, marker string) string {
	idx := file.index(marker) or { return '' }
	rest := file[idx + marker.len..]
	// Strip .v or .c.v suffix
	feature := if rest.ends_with('.c.v') {
		rest[..rest.len - 4]
	} else if rest.ends_with('.v') {
		rest[..rest.len - 2]
	} else {
		rest
	}
	return feature
}
