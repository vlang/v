// Copyright (c) 2020-2024 Joe Conigliaro. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module builder

import os

fn list_dir_entries(path string) []string {
	return os.ls(path) or { []string{} }
}

pub fn get_v_files_from_dir(dir string, user_defines []string) []string {
	mod_files := list_dir_entries(dir)
	mut v_files := []string{}
	for file in mod_files {
		// Include .v files (including .c.v), exclude .js.v and test files
		if !file.ends_with('.v') || file.ends_with('.js.v') || file.contains('_test.') {
			continue
		}
		// skip platform-specific files
		if file.contains('.arm64.') || file.contains('.arm32.') || file.contains('.amd64.') {
			continue
		}
		// skip OS-specific files for other platforms
		// Note: _nix files are for Unix-like systems including macOS and Linux
		$if macos {
			if file.contains('_windows.') || file.contains('_linux.') || file.contains('_android') {
				continue
			}
		} $else $if linux {
			if file.contains('_windows.') || file.contains('_macos.') || file.contains('_darwin.')
				|| file.contains('_android') {
				continue
			}
		} $else $if windows {
			if file.contains('_linux.') || file.contains('_macos.') || file.contains('_nix.')
				|| file.contains('_android') {
				continue
			}
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
		v_files << os.join_path(dir, file)
	}
	return v_files
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
