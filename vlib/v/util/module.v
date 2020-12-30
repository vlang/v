module util

import os

pub fn mod_path_to_full_name(mod string, path string) ?string {
	vmod_folders := ['vlib', '.vmodules', 'modules']
	path_parts := path.split(os.path_separator)
	mut mod_path := mod.replace('.', os.path_separator)
	for i := path_parts.len - 1; i >= 0; i-- {
		try_path := os.join_path(path_parts[0..i].join(os.path_separator), mod_path)
		if os.is_dir(try_path) {
			for j := i; j >= 0; j-- {
				path_part := path_parts[j]
				if path_part in vmod_folders {
					x := try_path.split(os.path_separator)[j + 1..].join('.')
					return x
				}
			}
		}
	}
	return error('module not found')
}
