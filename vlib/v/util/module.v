module util

import os
import v.pref

pub fn qualify_import(pref &pref.Preferences, mod string, file_path string) string {
	mut mod_paths := pref.lookup_path.clone()
	mod_paths << os.vmodules_paths()
	mod_path := mod.replace('.', os.path_separator)
	for search_path in mod_paths {
		try_path := os.join_path(search_path, mod_path)
		if m1 := mod_path_to_full_name(mod, try_path) {
			return m1
		}
	}
	if m1 := mod_path_to_full_name(mod, file_path) {
		// prinlnt('GOT HERE: $mod')
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

// TODO: properly define module location / v.mod rules
pub fn mod_path_to_full_name(mod string, path string) ?string {
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
	// go back through each path_parts looking for the destination `mod_path`
	for i := path_parts.len - 1; i >= 0; i-- {
		try_path := os.join_path(path_parts[0..i].join(os.path_separator), mod_path)
		// found module path
		if os.is_dir(try_path) {
			// if we know we are in one of the `vmod_folders`
			if in_vmod_path {
				// work our way baackwards until checking if we reached a vmod folder
				for j := i; j >= 0; j-- {
					path_part := path_parts[j]
					// we reached a vmod folder
					if path_part in vmod_folders {
						mod_full_name := try_path.split(os.path_separator)[j + 1..].join('.')
						return mod_full_name
					}
				}
				// we are not in one of the `vmod_folders` so we need to go back up each parent
				// looking for for a `v.mod` file and break at the first path without it
			} else {
				mut try_path_parts := try_path.split(os.path_separator)
				// last path in try_path_parts that contained `v.mod`
				mut last_v_mod := -1
				for j := try_path_parts.len; j > 0; j-- {
					parent := try_path_parts[0..j].join(os.path_separator)
					if ls := os.ls(parent) {
						if 'v.mod' in ls {
							// set last `v.mod` path
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
