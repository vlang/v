module builder

import (
	os
	filepath
)

[inline]
fn module_path(mod string) string {
	// submodule support
	return mod.replace('.', filepath.separator)
}

fn (b &Builder) find_module_path(mod string) ?string {
	mod_path := module_path(mod)
	for search_path in b.module_search_paths {
		try_path := filepath.join(search_path,mod_path)
		if b.pref.is_verbose {
			println('  >> trying to find $mod in $try_path ..')
		}
		if os.is_dir(try_path) {
			if b.pref.is_verbose {
				println('  << found $try_path .')
			}
			return try_path
		}
	}
	return error('module "$mod" not found')
}
