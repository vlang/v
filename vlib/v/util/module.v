module util

import os
import v.pref

pub fn qualify_import(pref &pref.Preferences, mod string, file_path string) string {
	mut mod_paths := pref.lookup_path.clone()
	mod_paths << os.vmodules_paths()
	mod_path := mod.replace('.', os.path_separator)
	for search_path in mod_paths {
		try_path := os.join_path(search_path, mod_path)
		if os.is_dir(try_path) {
			if m1 := mod_path_to_full_name(mod, try_path) {
				return m1
			}
		}
	}
	if m1 := mod_path_to_full_name(mod, file_path) {
		return m1
	}
	return mod
}

pub fn qualify_module(mod string, file_path string) string {
	if mod == 'main' {
		return mod
	}
	if m1 := mod_path_to_full_name(mod, file_path.all_before_last('/')) {
		return m1
	}
	return mod
}

// TODO:
// * properly define module location / v.mod rules
// * if possible split this function in two, one which gets the
// parent module path and another which turns it into the full name
// * create shared logic between these fns and builder.find_module_path
pub fn mod_path_to_full_name(mod string, path string) ?string {
	// TODO: explore using `pref.lookup_path` & `os.vmodules_paths()`
	// absolute paths instead of 'vlib' & '.vmodules'
	vmod_folders := ['vlib', '.vmodules', 'modules']
	mut in_vmod_path := false
	for vmod_folder in vmod_folders {
		if vmod_folder + os.path_separator in path {
			in_vmod_path = true
			break
		}
	}
	path_parts := path.split(os.path_separator)
	mod_path := mod.replace('.', os.path_separator)
	// go back through each parent in path_parts and join with `mod_path` to see the dir exists
	for i := path_parts.len - 1; i >= 0; i-- {
		try_path := os.join_path(path_parts[0..i].join(os.path_separator), mod_path)
		// found module path
		if os.is_dir(try_path) {
			// we know we are in one of the `vmod_folders`
			if in_vmod_path {
				// so we can work our way backwards until we reach a vmod folder
				for j := i; j >= 0; j-- {
					path_part := path_parts[j]
					// we reached a vmod folder
					if path_part in vmod_folders {
						mod_full_name := try_path.split(os.path_separator)[j + 1..].join('.')
						return mod_full_name
					}
				}
				// not in one of the `vmod_folders` so work backwards through each parent
				// looking for for a `v.mod` file and break at the first path without it
			} else {
				mut try_path_parts := try_path.split(os.path_separator)
				// last index in try_path_parts that contains a `v.mod`
				mut last_v_mod := -1
				for j := try_path_parts.len; j > 0; j-- {
					parent := try_path_parts[0..j].join(os.path_separator)
					if ls := os.ls(parent) {
						// currently CI clones some modules into the v repo to test, the condition
						// after `'v.mod' in ls` can be removed once a proper solution is added
						if 'v.mod' in ls && try_path_parts[i] != 'v' && 'vlib' !in ls {
							last_v_mod = j
							continue
						}
					}
					break
				}
				if last_v_mod > -1 {
					mod_full_name := try_path_parts[last_v_mod - 1..].join('.')
					return mod_full_name
				}
			}
		}
	}
	return error('module not found')
}
