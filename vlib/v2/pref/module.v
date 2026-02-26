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
	return os.join_path(p.effective_vroot(), 'vlib', mod_path)
}

// check for relative and then vlib
pub fn (p &Preferences) get_module_path(mod string, importing_file_path string) string {
	mod_path := mod.replace('.', os.path_separator)
	vroot := p.effective_vroot()
	// TODO: is this the best order?
	// vlib
	vlib_path := os.join_path(vroot, 'vlib', mod_path)
	if os.is_dir(vlib_path) {
		return vlib_path
	}
	// ~/.vmodules
	vmodules_path := os.join_path(p.vmodules_path, mod_path)
	if dir_exists(vmodules_path) {
		return vmodules_path
	}
	// relative to file importing it
	relative_path := os.join_path(os.dir(importing_file_path), mod_path)
	if dir_exists(relative_path) {
		return relative_path
	}
	panic('Preferences.get_module_path: cannot find module path for `${mod}`')
}
