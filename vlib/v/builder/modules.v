module builder

import (
	os
	filepath
)

fn (b mut Builder) set_module_search_paths() {
	msearch_path := if b.prefs.vpath.len > 0 { b.prefs.vpath } else { b.prefs.mod_path }
	// Module search order:
	// 0) V test files are very commonly located right inside the folder of the
	// module, which they test. Adding the parent folder of the module folder
	// with the _test.v files, *guarantees* that the tested module can be found
	// without needing to set custom options/flags.
	// 1) search in the *same* directory, as the compiled final v program source
	// (i.e. the . in `v .` or file.v in `v file.v`)
	// 2) search in the modules/ in the same directory.
	// 3) search in vlib/
	// 4.1) search in -vpath (if given)
	// 4.2) search in ~/.vmodules/ (i.e. modules installed with vpm) (no -vpath)
	b.prefs.module_search_paths = []
	if b.prefs.is_test {
		b.prefs.module_search_paths << filepath.basedir(b.prefs.compile_dir) // pdir of _test.v
	}
	b.prefs.module_search_paths << b.prefs.compile_dir
	b.prefs.module_search_paths << filepath.join(b.prefs.compile_dir,'modules')
	b.prefs.module_search_paths << b.prefs.vlib_path
	b.prefs.module_search_paths << msearch_path
	if b.prefs.user_mod_path.len > 0 {
		b.prefs.module_search_paths << b.prefs.user_mod_path
	}
	if b.prefs.is_verbose {
		b.log('b.prefs.module_search_paths: $b.prefs.module_search_paths')
	}
}


[inline]
fn module_path(mod string) string {
	// submodule support
	return mod.replace('.', os.path_separator)
}

fn (b &Builder) find_module_path(mod string) ?string {
	mod_path := module_path(mod)
	for search_path in b.prefs.module_search_paths {
		try_path := filepath.join(search_path,mod_path)
		if b.prefs.is_verbose {
			println('  >> trying to find $mod in $try_path ..')
		}
		if os.is_dir(try_path) {
			if b.prefs.is_verbose {
				println('  << found $try_path .')
			}
			return try_path
		}
	}
	return error('module "$mod" not found')
}
