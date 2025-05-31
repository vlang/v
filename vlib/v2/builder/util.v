// Copyright (c) 2020-2024 Joe Conigliaro. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module builder

import os

fn get_v_files_from_dir(dir string) []string {
	mod_files := os.ls(dir) or { panic('error getting ls from ${dir}') }
	mut v_files := []string{}
	for file in mod_files {
		if !file.ends_with('.v') || file.ends_with('.js.v') || file.ends_with('_test.v') {
			continue
		}
		v_files << os.join_path(dir, file)
	}
	return v_files
}
