module compiler

import os
import filepath


// This file provides a caching mechanism for seeking quickly whether a
// given folder has a v.mod file in it or in any of its parent folders.
//
// ModFileCacher.get(folder) works in such a way, that given this tree:
// examples/hanoi.v
// vlib/v.mod
// vlib/compiler/tests/project_with_c_code/mod1/v.mod
// vlib/compiler/tests/project_with_c_code/mod1/wrapper.v
// -----------------
// ModFileCacher.get('examples')
// => ModFileAndFolder{'', 'examples'}
// ModFileCacher.get('vlib/compiler/tests')
// => ModFileAndFolder{'vlib/v.mod', 'vlib'}
// ModFileCacher.get('vlib/compiler')
// => ModFileAndFolder{'vlib/v.mod', 'vlib'}
// ModFileCacher.get('vlib/project_with_c_code/mod1')
// => ModFileAndFolder{'vlib/project_with_c_code/mod1/v.mod', 'vlib/project_with_c_code/mod1'}


struct ModFileAndFolder {
	// vmod_file contains the full path of the found 'v.mod' file, or ''
	// if no 'v.mod' file was found in file_path_dir, or in its parent folders.
	vmod_file string

	// vmod_folder contains the file_path_dir, if there is no 'v.mod' file in
	// *any* of the parent folders, otherwise it is the first parent folder,
	// where a v.mod file was found.
	vmod_folder string
}

struct ModFileCacher {
mut:
	cache map[string]ModFileAndFolder
	// folder_files caches os.ls(key)
	folder_files map[string][]string
}

fn new_mod_file_cacher() &ModFileCacher {
	return &ModFileCacher{}
}

fn (mcache &ModFileCacher) dump() {
	$if debug {
		eprintln('ModFileCacher DUMP:')
		eprintln('	 ModFileCacher.cache:')
		for k,v in mcache.cache {
			eprintln('	 K: ${k:-32s} | V: "${v.vmod_file:32s}" | "${v.vmod_folder:32s}" ')
		}
		eprintln('	 ModFileCacher.folder_files:')
		for k,v in mcache.folder_files {
			eprintln('	 K: ${k:-32s} | V: ${v.str()}')
		}
	}
}

fn (mcache mut ModFileCacher) get(mfolder string) ModFileAndFolder {
	if mfolder in mcache.cache {
		return mcache.cache[ mfolder ]
	}
	traversed_folders, res := mcache.traverse( mfolder )
	for tfolder in traversed_folders {
		mcache.add( tfolder, res )
	}
	return res
}

fn (cacher mut ModFileCacher) add(path string, result ModFileAndFolder) {
	cacher.cache[ path ] = result
}

fn (mcache mut ModFileCacher) traverse(mfolder string) ([]string, ModFileAndFolder) {
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
			res := mcache.cache[ cfolder ]
			if res.vmod_file.len == 0 {
				mcache.mark_folders_as_vmod_free( folders_so_far )
			}else{
				mcache.mark_folders_with_vmod( folders_so_far, res )
			}
			return []string, res
		}
		files := mcache.get_files( cfolder )
		if 'v.mod' in files {
			// TODO: actually read the v.mod file and parse its contents to see
			// if its source folder is different
			res := ModFileAndFolder{ vmod_file: filepath.join( cfolder, 'v.mod'), vmod_folder: cfolder }
			return folders_so_far, res
		}
		if mcache.check_for_stop( cfolder, files ) {
			break
		}
		cfolder = filepath.basedir( cfolder )
		folders_so_far << cfolder
		levels++
	}
	mcache.mark_folders_as_vmod_free( folders_so_far )
	return [mfolder], ModFileAndFolder{ vmod_file: '', vmod_folder: mfolder }
}

fn (mcache mut ModFileCacher) mark_folders_with_vmod( folders_so_far []string, vmod ModFileAndFolder ) {
	for f in folders_so_far {
		mcache.add( f, vmod )
	}
}

fn (mcache mut ModFileCacher) mark_folders_as_vmod_free( folders_so_far []string ) {
	// No need to check these folders anymore,
	// because their parents do not contain v.mod files
	for f in folders_so_far {
		mcache.add( f, ModFileAndFolder{ vmod_file: '', vmod_folder: f } )
	}
}

const ( MOD_FILE_STOP_PATHS = ['.git', '.hg', '.svn', '.v.mod.stop' ] )
fn (mcache &ModFileCacher) check_for_stop(cfolder string, files []string) bool {
	for i in MOD_FILE_STOP_PATHS {
		if i in files {
			return true
		}
	}
	return false
}

fn (mcache mut ModFileCacher) get_files(cfolder string) []string {
	if cfolder in mcache.folder_files {
		return mcache.folder_files[ cfolder ]
	}
	files := os.ls(cfolder) or { return [] }
	mcache.folder_files[ cfolder ] = files
	return files
}
