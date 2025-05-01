// vtest retry: 2
// vtest build: !windows
module main

import os
import arrays

const vexe_path = @VEXE
const vexe_ = os.quoted_path(vexe_path)
const tpath = os.join_path(os.vtmp_dir(), 'vod_test_module')

fn testsuite_begin() {
	os.rmdir_all(tpath) or {}
	os.mkdir_all(tpath)!
	os.chdir(tpath)!
}

fn testsuite_end() {
	os.rmdir_all(tpath) or {}
}

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

fn test_ignore_rules() {
	os.write_file('.vdocignore', ['pattern1', 'pattern2', '/path1'].join_lines())!
	os.mkdir('subdir')!
	os.write_file(os.join_path('subdir', '.vdocignore'), ['pattern3', '/path2'].join_lines())!
	rules := IgnoreRules.get('.')
	assert rules.patterns['.'] == ['pattern1', 'pattern2']
	assert rules.patterns['./subdir'] == ['pattern3']
	assert rules.paths == {
		'./path1':        true
		'./subdir/path2': true
	}
}

fn test_get_module_list() {
	// For information on leading slash rules, refer to the comments in `IgnoreRules.get`.
	ignore_rules := ['bravo', '/echo', '/foxtrot/golf', 'hotel.v/', 'india/juliett']
	os.write_file('.vdocignore', ignore_rules.join_lines())!

	/* Create some submodules.
	Modules inside `testdata` and `tests` directories and modules that
	only contain `_test.v` files should be ignored by default. */
	// Modules NOT to ignore.
	submodules_no_ignore := [
		'alpha',
		'alpha_bravo', // test `bravo`
		'bravo_charly', // test `bravo`
		'charly',
		'charly/alpha',
		'charly/delta', // test `delta` in separate ignore file in `alpha`
		'charly/echo', // test `/echo`
		'charly/foxtrot/golf', // test `/foxtrot/golf`
		'foxtrot',
		'golf',
		'hotel', // will include a `hotel.v` file, whose pattern is in the ignore list with a trailing slash
	]
	// Modules TO ignore.
	submodules_to_ignore := [
		'alpha/bravo', // test `bravo`
		'alpha/delta', // test `delta` in separate ignore file
		'alpha/india/juliett/kilo', // test `india/juliett`
		'bravo', // test `bravo`
		'echo', // test `/echo`
		'foxtrot/golf', // test `/foxtrot/golf`
		'hotel.v', // test `hotel.v/`
		'tests', // test default
		'testdata', // test default
		'testdata/foxtrot', // test default
	]
	for p in arrays.append(submodules_no_ignore, submodules_to_ignore) {
		os.mkdir_all(p)!
		mod_name := p.all_after_last('/')
		os.write_file(os.join_path(p, '${mod_name}.v'), 'module ${mod_name}')!
	}
	// Create a module that only contains a `_test.v` file.
	os.mkdir('delta')!
	os.write_file(os.join_path('delta', 'delta_test.v'), 'module delta')!
	// Add a `.vdocignore` file to a submodule.
	os.write_file(os.join_path('alpha', '.vdocignore'), 'delta\n')!

	mod_list := get_modules(tpath)
	// dump(mod_list)
	assert mod_list.len == submodules_no_ignore.len
	for m in submodules_no_ignore.map(os.join_path(tpath, it)) {
		assert m in mod_list
	}
	for m in submodules_to_ignore.map(os.join_path(tpath, it)) {
		assert m !in mod_list
	}
	// `delta` only contains a `_test.v` file.
	assert !mod_list.any(it.contains(os.join_path(tpath, 'delta')))
}

fn test_get_readme_md_src() {
	// a special testcase for `src` dir get_readme
	// https://github.com/vlang/v/issues/24232

	os.mkdir('src')!
	os.write_file('v.mod', "Module {
        name: 'foobar'
        description: 'foobar'
        version: '0.0.0'
        license: 'MIT'
        dependencies: []
}
")!
	os.write_file('src/foobar.v', 'module foobar

// square calculates the second power of `x`
pub fn square(x int) int {
        return x * x
}
')!
	res := os.execute_opt('${vexe_} doc -m src/ -v') or { panic(err) }
	assert res.exit_code == 0
	assert res.output.contains('square')
}
