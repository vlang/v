module main

import os

// cmd/tools/vdoc/tests/testdata/module_list_with_ignore/.vdocignore
fn test_get_module_list() {
	tpath := os.join_path(@VEXEROOT, 'cmd', 'tools', 'vdoc', 'tests', 'testdata', 'module_list_with_ignore')

	ignore_paths := get_ignore_paths(tpath)!
	assert ignore_paths.all(it.contains('bravo'))

	mod_list := get_modules_list(tpath, []string{}).map(it.all_after(tpath))

	// Test ignored.
	assert mod_list.all(!it.contains('tests') && !it.contains('testdata') && !it.contains('echo')
		&& !it.contains('bravo')), mod_list.str()
	// Test expected.
	assert mod_list[1..].all(it == '' || it.contains('alpha') || it.contains('charlie')
		|| it.contains('delta')), mod_list.str()
}
