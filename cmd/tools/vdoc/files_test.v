module main

import os
import arrays

fn test_get_module_list() {
	tpath := os.join_path(os.vtmp_dir(), 'vod_test_module')
	os.rmdir_all(tpath) or {}
	os.mkdir_all(tpath)!
	defer {
		os.rmdir_all(tpath) or {}
	}

	os.chdir(tpath)!

	/* Create some submodules.
	Modules inside `testdata` and `tests` directories and modules that
	only contain `_test.v` files should be ignored by default. */
	submodules_no_ignore := ['alpha', 'charly', 'charly/alpha']
	submodules_to_ignore := ['alpha/bravo', 'bravo', 'tests', 'testdata', 'testdata/echo']
	for p in arrays.append(submodules_no_ignore, submodules_to_ignore) {
		os.mkdir(p)!
		mod_name := p.all_after('/')
		os.write_file(os.join_path(p, '${mod_name}.v'), 'module ${mod_name}')!
	}
	// Create a module that only contains a `_test.v` file.
	os.mkdir('delta')!
	os.write_file(os.join_path('delta', 'delta_test.v'), 'module delta')!

	os.write_file('.vdocignore', 'bravo\nalpha/bravo\n')!
	ignore_paths := get_ignore_paths(tpath)!
	assert os.join_path(tpath, 'bravo') in ignore_paths
	assert os.join_path(tpath, 'alpha/bravo') in ignore_paths
	// Only `bravo` modules are part of `.vdocignore`. Ensure that no others are ignored.
	assert ignore_paths.all(it.contains('bravo'))

	// `get_modules_list` uses `get_ignore_paths` internally.
	mod_list := get_modules_list(tpath, []string{})
	assert mod_list.len == submodules_no_ignore.len
	assert mod_list.all(it.contains('alpha') || it.contains('charly')), mod_list.str()
}
