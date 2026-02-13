// Copyright (c) 2020-2024 Joe Conigliaro. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module pref

import os

fn (p &Preferences) effective_vroot() string {
	mut candidate := if p.vroot.len > 0 {
		if os.is_abs_path(p.vroot) { p.vroot } else { os.real_path(p.vroot) }
	} else {
		os.real_path('.')
	}
	normalized := candidate.replace('\\', '/').trim_right('/')
	if normalized.contains('/cmd/v2') {
		repo_root := normalized.all_before('/cmd/v2')
		if repo_root.len > 0 {
			return repo_root
		}
	}
	if os.is_dir(os.join_path(candidate, 'vlib')) {
		return candidate
	}
	if normalized.contains('/cmd/v2') {
		repo_root := normalized.all_before('/cmd/v2')
		if os.is_dir(os.join_path(repo_root, 'vlib')) {
			return repo_root
		}
	}
	return candidate
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
