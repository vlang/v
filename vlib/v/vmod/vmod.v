module vmod

import os

const mod_file_stop_paths = ['.git', '.hg', '.svn', '.v.mod.stop']

// used during lookup for v.mod to support @VROOT
const private_file_cacher = new_mod_file_cacher()

pub fn get_cache() &ModFileCacher {
	return private_file_cacher
}

// This file provides a caching mechanism for seeking quickly whether a
// given folder has a v.mod file in it or in any of its parent folders.
//
// ModFileCacher.get(folder) works in such a way, that given this tree:
// examples/hanoi.v
// vlib/v.mod
// vlib/v/tests/project_with_c_code/mod1/v.mod
// vlib/v/tests/project_with_c_code/mod1/wrapper.c.v
// -----------------
// ModFileCacher.get('examples')
// => ModFileAndFolder{'', 'examples'}
// ModFileCacher.get('vlib/v/tests')
// => ModFileAndFolder{'vlib/v.mod', 'vlib'}
// ModFileCacher.get('vlib/v')
// => ModFileAndFolder{'vlib/v.mod', 'vlib'}
// ModFileCacher.get('vlib/v/test/project_with_c_code/mod1')
// => ModFileAndFolder{'vlib/v/test/project_with_c_code/mod1/v.mod', 'vlib/v/test/project_with_c_code/mod1'}
pub struct ModFileAndFolder {
pub:
	// vmod_file contains the full path of the found 'v.mod' file, or ''
	// if no 'v.mod' file was found in file_path_dir, or in its parent folders.
	vmod_file string
	// vmod_folder contains the file_path_dir, if there is no 'v.mod' file in
	// *any* of the parent folders, otherwise it is the first parent folder,
	// where a v.mod file was found.
	vmod_folder string
}

@[heap]
pub struct ModFileCacher {
mut:
	cache map[string]ModFileAndFolder
	// folder_files caches os.ls(key)
	folder_files     map[string][]string
	hits             int
	misses           int
	get_files_hits   int
	get_files_misses int
}

pub fn new_mod_file_cacher() &ModFileCacher {
	return &ModFileCacher{}
}

@[if debug_mod_file_cacher ?]
pub fn (mcache &ModFileCacher) debug() {
	eprintln('ModFileCacher hits: ${mcache.hits}, misses: ${mcache.misses} | get_files_hits: ${mcache.get_files_hits} | get_files_misses: ${mcache.get_files_misses}')
	eprintln('	 ModFileCacher.cache.len: ${mcache.cache.len}')
	for k, v in mcache.cache {
		eprintln('	 K: ${k:-42s} | v.mod: ${v.vmod_file:-42s} | folder: `${v.vmod_folder}`')
	}
	eprintln('	 ModFileCacher.folder_files:')
	for k, v in mcache.folder_files {
		eprintln('	 K: ${k:-42s} | folder_files: ${v}')
	}
}

pub fn (mut mcache ModFileCacher) get_by_file(vfile string) ModFileAndFolder {
	return mcache.get_by_folder(os.dir(vfile))
}

pub fn (mut mcache ModFileCacher) get_by_folder(vfolder string) ModFileAndFolder {
	mfolder := os.real_path(vfolder)
	if mfolder in mcache.cache {
		mcache.hits++
		return mcache.cache[mfolder]
	}
	traversed_folders, res := mcache.traverse(mfolder)
	for tfolder in traversed_folders {
		mcache.add(tfolder, res)
	}
	mcache.misses++
	return res
}

fn (mut cacher ModFileCacher) add(path string, result ModFileAndFolder) {
	cacher.cache[path] = result
}

fn (mut mcache ModFileCacher) traverse(mfolder string) ([]string, ModFileAndFolder) {
	mut cfolder := mfolder
	mut folders_so_far := [cfolder]
	mut levels := 0
	for {
		if levels > 255 {
			break
		}
		if cfolder == '/' || cfolder == '' {
			break
		}
		if cfolder in mcache.cache {
			mcache.hits++
			res := mcache.cache[cfolder]
			if res.vmod_file.len == 0 {
				mcache.mark_folders_as_vmod_free(folders_so_far)
			} else {
				mcache.mark_folders_with_vmod(folders_so_far, res)
			}
			return []string{}, res
		}
		files := mcache.get_files(cfolder)
		if 'v.mod' in files {
			// TODO: actually read the v.mod file and parse its contents to see
			// if its source folder is different
			res := ModFileAndFolder{
				vmod_file:   os.join_path(cfolder, 'v.mod')
				vmod_folder: cfolder
			}
			return folders_so_far, res
		}
		if mcache.check_for_stop(files) {
			break
		}
		cfolder = os.dir(cfolder)
		folders_so_far << cfolder
		levels++
	}
	mcache.mark_folders_as_vmod_free(folders_so_far)
	return [mfolder], ModFileAndFolder{
		vmod_file:   ''
		vmod_folder: mfolder
	}
}

fn (mut mcache ModFileCacher) mark_folders_with_vmod(folders_so_far []string, vmod ModFileAndFolder) {
	for f in folders_so_far {
		mcache.add(f, vmod)
	}
}

fn (mut mcache ModFileCacher) mark_folders_as_vmod_free(folders_so_far []string) {
	// No need to check these folders anymore,
	// because their parents do not contain v.mod files
	for f in folders_so_far {
		mcache.add(f, vmod_file: '', vmod_folder: f)
	}
}

fn (mcache &ModFileCacher) check_for_stop(files []string) bool {
	for i in mod_file_stop_paths {
		if i in files {
			return true
		}
	}
	return false
}

fn (mut mcache ModFileCacher) get_files(cfolder string) []string {
	if cfolder in mcache.folder_files {
		mcache.get_files_hits++
		return mcache.folder_files[cfolder]
	}
	mcache.get_files_misses++
	mut files := []string{}
	if os.exists(cfolder) && os.is_dir(cfolder) {
		if listing := os.ls(cfolder) {
			files = listing.clone()
		}
	}
	mcache.folder_files[cfolder] = files
	return files
}
