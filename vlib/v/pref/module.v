module pref

import v.vmod
import os

pub fn (mut p Preferences) find_module_path(mod, fpath string) ?string {
	// support @VROOT/v.mod relative paths:
	mcache := vmod.get_cache()
	vmod_file_location := mcache.get_by_file(fpath)
	mod_path := mod.replace('.', os.path_separator)
	mut module_lookup_paths := []string{}
	if vmod_file_location.vmod_file.len != 0 && vmod_file_location.vmod_folder !in p.module_search_paths {
		module_lookup_paths << vmod_file_location.vmod_folder
	}
	module_lookup_paths << p.module_search_paths
	for search_path in module_lookup_paths {
		try_path := os.join_path(search_path, mod_path)
		if os.is_dir(try_path) {
			return try_path
		}
	}
	smodule_lookup_paths := module_lookup_paths.join(', ')
	return error('module "$mod" not found in:\n$smodule_lookup_paths')
}

pub fn (p Preferences) v_files_from_dir(dir string) []string {
	if !os.exists(dir) {
		if dir == 'compiler' && os.is_dir('vlib') {
			println('looks like you are trying to build V with an old command')
			println('use `v -o v cmd/v` instead of `v -o v compiler`')
		}
		eprintln("$dir doesn't exist")
		exit(1)
	} else if !os.is_dir(dir) {
		eprintln("$dir isn't a directory!")
		exit(1)
	}
	mut files := os.ls(dir) or {
		panic(err)
	}
	if p.is_verbose {
		println('v_files_from_dir ("$dir")')
	}
	return p.should_compile_filtered_files(dir, files)
}
