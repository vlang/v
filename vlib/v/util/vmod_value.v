// Copyright (c) 2025 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module util

import os
import v.vmod

// resolve_vmodroot replaces all occurrences of `@VMODROOT` in `str`, with an absolute path,
// formed by resolving, where the nearest `v.mod` is, given the folder `dir`.
pub fn resolve_vmodroot(str string, dir string) !string {
	mut mcache := vmod.get_cache()
	vmod_file_location := mcache.get_by_folder(dir)
	if vmod_file_location.vmod_file.len == 0 {
		// There was no actual v.mod file found.
		return error('To use @VMODROOT, you need to have a "v.mod" file in ${dir}, or in one of its parent folders.')
	}
	vmod_path := vmod_file_location.vmod_folder
	return str.replace('@VMODROOT', os.real_path(vmod_path))
}
