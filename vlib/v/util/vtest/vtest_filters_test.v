module vtest

import os

const github_actions_env = 'GITHUB_ACTIONS'
const skip_ownership_env = 'VTEST_SKIP_OWNERSHIP'

fn ownership_and_autofree_fixture() string {
	root := os.join_path(os.vtmp_dir(), 'vtest_ownership_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	return root
}

// GitHub Actions disables this coverage globally, and a local run opts in
// explicitly, so the two switches must be read as one flag.
fn test_skip_ownership_autofree_tests_reads_both_switches() {
	github := github_actions_env
	skip := skip_ownership_env
	previous_github := os.getenv_opt(github)
	previous_skip := os.getenv_opt(skip)
	defer {
		if value := previous_github {
			os.setenv(github, value, true)
		} else {
			os.unsetenv(github)
		}
		if value := previous_skip {
			os.setenv(skip, value, true)
		} else {
			os.unsetenv(skip)
		}
	}
	os.unsetenv(github)
	os.unsetenv(skip)
	assert !skip_ownership_autofree_tests()
	os.setenv(skip, '1', true)
	assert skip_ownership_autofree_tests()
	os.setenv(skip, '0', true)
	os.setenv(github, 'true', true)
	assert skip_ownership_autofree_tests()
	os.unsetenv(skip)
	assert skip_ownership_autofree_tests()
	// Only `true` marks Actions, and only `1` marks the opt-out.
	os.setenv(github, 'false', true)
	assert !skip_ownership_autofree_tests()
	os.setenv(github, '', true)
	assert !skip_ownership_autofree_tests()
	os.setenv(github, '', true)
	os.setenv(skip, 'yes', true)
	assert !skip_ownership_autofree_tests()
	os.setenv(github, 'true', true)
	assert skip_ownership_autofree_tests()
}

fn test_is_ownership_autofree_test_recognises_paths() {
	root := ownership_and_autofree_fixture()
	defer {
		os.rmdir_all(root) or {}
	}
	ownership_file := os.join_path(root, 'ownership', 'a_test.v')
	autofree_file := os.join_path(root, 'autofree_thing_test.v')
	plain_file := os.join_path(root, 'plain_test.v')
	for path in [ownership_file, autofree_file, plain_file] {
		os.mkdir_all(os.dir(path)) or { panic(err) }
		os.write_file(path, '') or { panic(err) }
	}
	assert is_ownership_autofree_test(ownership_file)
	assert is_ownership_autofree_test(autofree_file)
	assert !is_ownership_autofree_test(plain_file)
	// Windows separators and letter case must not hide the marker.
	assert is_ownership_autofree_test(ownership_file.replace('/', '\\'))
	assert is_ownership_autofree_test(ownership_file.to_upper())
	// A path that does not exist is simply not an ownership test.
	assert !is_ownership_autofree_test(os.join_path(root, 'missing_test.v'))
}

fn test_is_ownership_autofree_test_reads_the_vflags_directive() {
	root := ownership_and_autofree_fixture()
	defer {
		os.rmdir_all(root) or {}
	}
	path := os.join_path(root, 'a_test.v')
	os.write_file(path, '') or { panic(err) }
	assert !is_ownership_autofree_test(path)
	// The flags are read the way the `v` launcher reads them to pick the ownership compiler.
	for vflags in ['-autofree', '-no-parallel -autofree', '-autofree -gc none', '-ownership',
		'--ownership', '-d ownership', '-d ownership -gc none', '-define ownership=on', '-downership'] {
		os.write_file(path, '// vtest vflags: ${vflags}\nfn test_x() {}\n') or { panic(err) }
		assert is_ownership_autofree_test(path), vflags
	}
	for vflags in ['-prod', '-d autofree', '-d ownership_extra', '-autofree_extra', '-gc none',
		'-d', ''] {
		os.write_file(path, '// vtest vflags: ${vflags}\nfn test_x() {}\n') or { panic(err) }
		assert !is_ownership_autofree_test(path), vflags
	}
	// Like the test runner, only a directive at the start of a line counts.
	os.write_file(path, 'fn test_x() {\n\t// vtest vflags: -autofree\n}\n') or { panic(err) }
	assert !is_ownership_autofree_test(path)
}

fn test_is_ownership_autofree_test_reads_the_ownership_directive() {
	root := ownership_and_autofree_fixture()
	defer {
		os.rmdir_all(root) or {}
	}
	path := os.join_path(root, 'a_test.v')
	for source in ['// vtest ownership\nimport os\n', '// vtest build: !windows\n// vtest ownership\n',
		'// vtest ownership // builds the ownership compiler\n', '// vtest ownership\r\nimport os\r\n',
		'import os\n\nfn test_x() {}\n// vtest ownership\n'] {
		os.write_file(path, source) or { panic(err) }
		assert is_ownership_autofree_test(path), source
	}
	for source in ['// vtest ownership_extra\n', '// vtest ownerships\n', '//vtest ownership\n',
		'fn test_x() {\n\t// vtest ownership\n}\n', "const x = '// vtest ownership'\n"] {
		os.write_file(path, source) or { panic(err) }
		assert !is_ownership_autofree_test(path), source
	}
}

// Since #29365, tests pass compiler flags as `os.exec` array elements. A flag in the
// source, in a string or in an array, is not enough: many tests pass `-ownership` only to
// check how the launcher parses it or that a compiler without ownership support rejects it.
fn test_is_ownership_autofree_test_ignores_flags_outside_the_directives() {
	root := ownership_and_autofree_fixture()
	defer {
		os.rmdir_all(root) or {}
	}
	path := os.join_path(root, 'a_test.v')
	for source in [
		"fn test_x() {\n\tos.exec([v3_bin, '-ownership', '-d', 'ownership', source])\n}\n",
		"fn test_x() {\n\tos.execute('\${vexe} -autofree -o out main.v')\n}\n",
		"fn test_x() {\n\tfor mode in ['-ownership', '-autofree'] {}\n}\n",
		'fn t() {\n\tx := 1 // -gc boehm\n\ty := 2 /* -ownership */\n}\n',
		'// Builds with `v -ownership` and `-d ownership`.\nfn test_x() {}\n',
	] {
		os.write_file(path, source) or { panic(err) }
		assert !is_ownership_autofree_test(path), source
	}
}

// The repository files from the CI report on #29365 that the detector must keep apart.
fn test_is_ownership_autofree_test_classifies_the_repository_tests() {
	vroot := os.real_path(os.join_path(os.dir(@FILE), '..', '..', '..', '..'))
	// They build the ownership compiler, or compile with `-ownership`/`-autofree`, and pass
	// those flags as `os.exec` array elements.
	for rel_path in [
		'vlib/v/compiler_tests/mutable_array_field_index_move_codegen_test.v',
		'vlib/v/compiler_tests/nested_default_clone_codegen_test.v',
		'vlib/v/compiler_tests/owned_array_borrow_storage_codegen_test.v',
		'vlib/v/transform/owned_array_storage_test.v',
	] {
		path := os.join_path(vroot, rel_path)
		assert os.is_file(path), path
		assert is_ownership_autofree_test(path), rel_path
	}
	// They pass the flags as data to argument parsers, check that a compiler without ownership
	// support rejects them, or (modulecache_test) compile with them in one test function out
	// of many. Their files are not ownership tests, and neither is this one.
	for rel_path in ['vlib/v/driver/c_compiler_flags_test.v', 'vlib/v/modulecache/modulecache_test.v',
		'vlib/v/compiler_tests/driver_cli_test.v', 'cmd/v/launcher_test.v',
		'vlib/v/util/vtest/vtest_filters_test.v'] {
		path := os.join_path(vroot, rel_path)
		assert os.is_file(path), path
		assert !is_ownership_autofree_test(path), rel_path
	}
}

fn test_filter_vtest_only_matches_on_the_configured_path() {
	previous := os.getenv_opt('VTEST_ONLY')
	defer {
		if value := previous {
			os.setenv('VTEST_ONLY', value, true)
		} else {
			os.unsetenv('VTEST_ONLY')
		}
	}
	os.unsetenv('VTEST_ONLY')
	// NOTE: with VTEST_ONLY unset every path matches: `''.split(',')` is `['']`,
	// and every string contains the empty pattern.
	assert filter_vtest_only(['a_test.v', 'b_test.v'], basepath: 'x') == ['x/a_test.v', 'x/b_test.v']
	os.setenv('VTEST_ONLY', 'a_', true)
	assert filter_vtest_only(['a_test.v', 'b_test.v'], basepath: 'x') == ['x/a_test.v']
	assert filter_vtest_only(['sub/a_test.v', 'b_test.v'], basepath: 'x') == ['x/sub/a_test.v']
	os.setenv('VTEST_ONLY', 'b_,nope', true)
	assert filter_vtest_only(['a_test.v', 'b_test.v'], basepath: '') == ['b_test.v']
	// The base path is joined before the match, so it takes part in it.
	assert filter_vtest_only(['c_test.v'], basepath: 'x') == []
	os.setenv('VTEST_ONLY', 'x/', true)
	assert filter_vtest_only(['c_test.v'], basepath: 'x') == ['x/c_test.v']
}
