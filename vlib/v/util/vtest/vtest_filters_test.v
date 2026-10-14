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

fn test_is_ownership_autofree_test_reads_the_vflags_directive_and_the_source() {
	root := ownership_and_autofree_fixture()
	defer {
		os.rmdir_all(root) or {}
	}
	path := os.join_path(root, 'a_test.v')
	os.write_file(path, '') or { panic(err) }
	assert !is_ownership_autofree_test(path)
	os.write_file(path, '// vtest vflags: -autofree\n') or { panic(err) }
	assert is_ownership_autofree_test(path)
	os.write_file(path, '// vtest vflags: -d ownership\n') or { panic(err) }
	assert is_ownership_autofree_test(path)
	os.write_file(path, '// vtest vflags: -prod\n') or { panic(err) }
	assert !is_ownership_autofree_test(path)
	// A flag anywhere in the source counts, not only in the directive.
	os.write_file(path, 'fn t() {\n\tx := 1 // -gc boehm\n\ty := 2 /* -ownership */\n}\n') or {
		panic(err)
	}
	assert is_ownership_autofree_test(path)
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
