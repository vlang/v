module main

import os
import arrays

fn test_trim_doc_node_description() {
	mod := 'foo'
	mut readme := '## Description

`foo` is a module that provides tools and utility functions to assist in working with bar.
It also assists with composing and testing baz.'
	expected := 'is a module that provides tools and utility functions to assist in working with'
	res := trim_doc_node_description(mod, readme).trim_space()
	assert res == expected

	readme = '# Foo
`foo` is a module that provides tools and utility functions to assist in working with bar.
It also assists with composing and testing baz.'
	res2 := trim_doc_node_description(mod, readme).trim_space()
	assert res2 == res
}

fn test_get_module_list() {
	tpath := os.join_path(os.vtmp_dir(), 'vod_test_module')
	os.rmdir_all(tpath) or {}
	os.mkdir_all(tpath)!
	defer {
		os.rmdir_all(tpath) or {}
	}

	os.chdir(tpath)!

	// For information on leading slash rules, refer to the doc comment of `get_ignore_files`.
	ignore_rules := ['bravo', '/echo', '/foxtrot/golf', 'hotel.v/']
	os.write_file('.vdocignore', ignore_rules.join_lines())!

	/* Create some submodules.
	Modules inside `testdata` and `tests` directories and modules that
	only contain `_test.v` files should be ignored by default. */
	submodules_no_ignore := ['alpha', 'charly', 'charly/alpha', 'charly/echo', 'charly/foxtrot/golf',
		'foxtrot', 'golf']
	submodules_to_ignore := ['alpha/bravo', 'bravo', 'echo', 'foxtrot/golf', 'tests', 'testdata',
		'testdata/echo', 'hotel.v']
	for p in arrays.append(submodules_no_ignore, submodules_to_ignore) {
		os.mkdir_all(p)!
		mod_name := p.all_after_last('/')
		os.write_file(os.join_path(p, '${mod_name}.v'), 'module ${mod_name}')!
	}
	// Create a module that only contains a `_test.v` file.
	os.mkdir('delta')!
	os.write_file(os.join_path('delta', 'delta_test.v'), 'module delta')!
	// Create a file with a name whose pattern is in the ignore list with a trailing slash.
	os.write_file(os.join_path('alpha', 'hotel.v'), 'module alpha')!

	mod_list := get_modules_list(tpath)
	assert mod_list.len == submodules_no_ignore.len + 1 // +1 since `alpha/hotel.v` was added separately.
	for m in submodules_no_ignore {
		assert mod_list.any(it.contains(os.join_path(tpath, m)))
	}
	for m in submodules_to_ignore {
		assert !mod_list.any(it.contains(os.join_path(tpath, m)))
	}
	assert !mod_list.any(it.contains(os.join_path(tpath, 'delta')))
	// `hotel.v/` is added as ignore pattern with a tailing slash.
	// The directory should be ignored while the file should be included.
	assert !mod_list.any(it.contains(os.join_path(tpath, 'hotel.v')))
	assert os.join_path(tpath, 'alpha', 'hotel.v') in mod_list
}
