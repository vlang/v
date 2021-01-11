module vmod

import os

// This file provides a caching mechanism for seeking quickly whether a
// given folder has a v.mod file in it or in any of its parent folders.
//
// ModFileCacher.get(folder) works in such a way, that given this tree:
// examples/hanoi.v
// vlib/v.mod
// vlib/v/tests/project_with_c_code/mod1/v.mod
// vlib/v/tests/project_with_c_code/mod1/wrapper.v
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

[ref_only]
pub struct ModFileCacher {
mut:
	cache map[string]ModFileAndFolder
	// folder_files caches os.ls(key)
	folder_files map[string][]string
}

pub fn new_mod_file_cacher() &ModFileCacher {
	return &ModFileCacher{}
}

pub fn (mcache &ModFileCacher) dump() {
	$if debug {
		eprintln('ModFileCacher DUMP:')
		eprintln('	 ModFileCacher.cache:')
		for k, v in mcache.cache {
			eprintln('	 K: ${k:-32s} | V: "${v.vmod_file:32s}" | "${v.vmod_folder:32s}" ')
		}
		eprintln('	 ModFileCacher.folder_files:')
		for k, v in mcache.folder_files {
			eprintln('	 K: ${k:-32s} | V: $v.str()')
		}
	}
}

pub fn (mut mcache ModFileCacher) get_by_file(vfile string) ModFileAndFolder {
	return mcache.get_by_folder(os.dir(vfile))
}

pub fn (mut mcache ModFileCacher) get_by_folder(vfolder string) ModFileAndFolder {
	mfolder := os.real_path(vfolder)
	if mfolder in mcache.cache {
		return mcache.cache[mfolder]
	}
	traversed_folders, res := mcache.traverse(mfolder)
	for tfolder in traversed_folders {
		mcache.add(tfolder, res)
	}
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
				vmod_file: os.join_path(cfolder, 'v.mod')
				vmod_folder: cfolder
			}
			return folders_so_far, res
		}
		if mcache.check_for_stop(cfolder, files) {
			break
		}
		cfolder = os.dir(cfolder)
		folders_so_far << cfolder
		levels++
	}
	mcache.mark_folders_as_vmod_free(folders_so_far)
	return [mfolder], ModFileAndFolder{
		vmod_file: ''
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

const (
	mod_file_stop_paths = ['.git', '.hg', '.svn', '.v.mod.stop']
)

fn (mcache &ModFileCacher) check_for_stop(cfolder string, files []string) bool {
	for i in mod_file_stop_paths {
		if i in files {
			return true
		}
	}
	return false
}

fn (mut mcache ModFileCacher) get_files(cfolder string) []string {
	if cfolder in mcache.folder_files {
		return mcache.folder_files[cfolder]
	}
	mut files := []string{}
	if os.exists(cfolder) && os.is_dir(cfolder) {
		if listing := os.ls(cfolder) {
			files = listing.clone()
		}
	}
	mcache.folder_files[cfolder] = files
	return files
}

// used during lookup for v.mod to support @VROOT
const (
	private_file_cacher = new_mod_file_cacher()
)

pub fn get_cache() &ModFileCacher {
	return private_file_cacher
}
