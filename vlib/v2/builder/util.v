// Copyright (c) 2020-2024 Joe Conigliaro. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module builder

import os

pub fn get_v_files_from_dir(dir string) []string {
	mod_files := os.ls(dir) or { panic('error getting ls from ${dir}') }
	mut v_files := []string{}
	for file in mod_files {
		if !file.ends_with('.v') || file.ends_with('.js.v') || file.ends_with('.c.v')
			|| file.contains('_test.') {
			continue
		}
		// skip platform-specific files
		if file.contains('.arm64.') || file.contains('.arm32.') || file.contains('.amd64.') {
			continue
		}
		// skip OS-specific files for other platforms
		$if macos {
			if file.contains('_windows.') || file.contains('_linux.') || file.contains('_nix.') {
				continue
			}
		} $else $if linux {
			if file.contains('_windows.') || file.contains('_macos.') {
				continue
			}
		} $else $if windows {
			if file.contains('_linux.') || file.contains('_macos.') || file.contains('_nix.') {
				continue
			}
		}
		// skip mutually exclusive conditional compilation files
		// For native backends, skip the _d_ variants (defined) and keep _notd_ (not defined)
		// This avoids duplicates from gcboehm, libbacktrace, etc.
		if file.contains('_d_') {
			continue
		}
		v_files << os.join_path(dir, file)
	}
	return v_files
}
