// Copyright (c) 2020-2024 Joe Conigliaro. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module pref

import os

fn (p &Preferences) effective_vroot() string {
	if p.vroot.len > 0 {
		return p.vroot
	}
	return os.getwd()
}

pub fn (p &Preferences) get_vlib_module_path(mod string) string {
	mod_path := mod.replace('.', os.path_separator)
	return module_path_join(module_path_join(p.effective_vroot(), 'vlib'), mod_path)
}

fn module_path_join(base string, name string) string {
	if base.len == 0 {
		return name
	}
	if name.len == 0 {
		return base
	}
	last := base[base.len - 1]
	if last == `/` || last == `\\` {
		return base + name
	}
	return base + os.path_separator + name
}

// check for relative and then vlib
pub fn (p &Preferences) get_module_path(mod string, importing_file_path string) string {
	mod_path := mod.replace('.', os.path_separator)
	vroot := p.effective_vroot()
	// TODO: is this the best order?
	// vlib
	vlib_path := module_path_join(module_path_join(vroot, 'vlib'), mod_path)
	if dir_exists(vlib_path) {
		return vlib_path
	}
	// ~/.vmodules
	vmodules_path := module_path_join(p.vmodules_path, mod_path)
	if dir_exists(vmodules_path) {
		// V convention: if a module dir has a src/ subdirectory, use that
		src_path := module_path_join(vmodules_path, 'src')
		if dir_exists(src_path) {
			return src_path
		}
		return vmodules_path
	}
	// relative to file importing it
	relative_path := module_path_join(os.dir(importing_file_path), mod_path)
	if dir_exists(relative_path) {
		return relative_path
	}
	panic('Preferences.get_module_path: cannot find module path for `${mod}`')
}
