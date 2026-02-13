// Copyright (c) 2020-2024 Joe Conigliaro. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module builder

import os

fn list_dir_entries(path string) []string {
	return os.ls(path) or { []string{} }
}

pub fn get_v_files_from_dir(dir string) []string {
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
			if file.contains('_windows.') || file.contains('_macos.') || file.contains('_android') {
				continue
			}
		} $else $if windows {
			if file.contains('_linux.') || file.contains('_macos.') || file.contains('_nix.')
				|| file.contains('_android') {
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
