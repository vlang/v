import os

fn deep_glob() ! {
	os.chdir(@VMODROOT)!
	matches := os.glob('vlib/v/*/*.v') or { panic(err) }
	assert matches.len > 10
	assert 'vlib/v/parser/parser.v' in matches
	assert 'vlib/v/pref/pref.v' in matches
	assert 'vlib/v/token/token.v' in matches
	for f in matches {
		if !f.starts_with('vlib/v/') {
			assert false
		}
		assert f.ends_with('.v')
	}
}

fn redeep_glob() ! {
	os.chdir(@VMODROOT)!
	matches := os.glob('vlib/v/**/*.v') or { panic(err) }
	assert matches.len > 10
	assert 'vlib/v/parser/parser.v' in matches
	assert 'vlib/v/pref/pref.v' in matches
	assert 'vlib/v/token/token.v' in matches
	for f in matches {
		if !f.starts_with('vlib/v/') {
			assert false
		}
		assert f.ends_with('.v')
	}
}

fn test_glob_can_find_v_files_3_levels_deep() {
	$if !windows {
		deep_glob()!
		redeep_glob()!
	}
	assert true
}

fn test_glob_can_find_files_in_current_folder() {
	os.chdir(@VMODROOT)!
	matches := os.glob('*')!
	assert '.gitignore' in matches
	assert 'makev.bat' in matches
	assert 'Makefile' in matches
	assert 'Dockerfile' in matches
	assert 'README.md' in matches
	assert 'v.mod' in matches
	assert 'cmd/' in matches
	assert 'vlib/' in matches
	assert 'thirdparty/' in matches
}

fn test_glob_can_be_used_with_multiple_patterns() {
	os.chdir(@VMODROOT)!
	matches := os.glob('*', 'cmd/tools/*')!
	assert 'README.md' in matches
	assert 'Makefile' in matches
	assert 'cmd/tools/test_if_v_test_system_works.v' in matches
}

fn test_glob_star() {
	os.chdir(@VMODROOT)!
	matches := os.glob('*ake*')!
	assert 'Makefile' in matches
	assert 'makev.bat' in matches
}

fn test_glob_not_found() {
	os.glob('an_unknown_folder/*.v') or {
		assert true
		return
	}
}

fn test_glob_multi_segment_wildcard_matches_first_and_last_segment() {
	// a pattern with more than one interior `*` must match the first and the
	// *last* segment: `a*b*c` matches names starting with `a` and ending with
	// `c`, not names ending with the middle segment `b`.
	// See https://github.com/vlang/v/issues/30152
	cwd := os.getwd()
	tmp := os.join_path(os.vtmp_dir(), 'v_glob_test_${os.getpid()}')
	os.mkdir_all(tmp)!
	os.chdir(tmp)!
	defer {
		os.chdir(cwd) or {}
		os.rmdir_all(tmp) or {}
	}
	for name in ['ab', 'axxb', 'axbyc', 'azbzc', 'axc', 'acbc', 'a', 'aa', 'aaa'] {
		os.write_file(name, 'x')!
	}
	matches := os.glob('a*b*c')!
	assert 'axbyc' in matches
	assert 'azbzc' in matches
	assert 'ab' !in matches
	assert 'axxb' !in matches
	assert 'axc' !in matches
	assert 'acbc' in matches
	// the two-segment case is unchanged
	two := os.glob('a*b')!
	assert 'ab' in two
	assert 'axxb' in two
	assert 'axbyc' !in two
	assert os.glob('a*a')! == ['aa', 'aaa']
	assert os.glob('*b*c')! == ['acbc', 'axbyc', 'azbzc']
	assert os.glob('a*b*')! == ['ab', 'acbc', 'axbyc', 'axxb', 'azbzc']
	assert os.glob('*b*c*')! == ['acbc', 'axbyc', 'azbzc']
	assert os.glob('a**b**c')! == ['acbc', 'axbyc', 'azbzc']
}
