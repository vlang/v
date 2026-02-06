import os

fn deep_glob() ! {
	os.chdir(@VMODROOT)!
	matches := os.glob('vlib/v/*/*.v') or { panic(err) }
	assert matches.len > 10
	assert 'vlib/v/ast/ast.v' in matches
	assert 'vlib/v/ast/table.v' in matches
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
	assert 'vlib/v/ast/ast.v' in matches
	assert 'vlib/v/ast/table.v' in matches
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
	assert 'make.bat' in matches
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
	$if !windows {
		assert 'cmd/tools/test_if_v_test_system_works.v' in matches
	}
	$if windows {
		assert 'test_if_v_test_system_works.v' in matches
	}
}

fn test_glob_star() {
	os.chdir(@VMODROOT)!
	matches := os.glob('*ake*')!
	assert 'Makefile' in matches
	assert 'make.bat' in matches
}

fn test_glob_not_found() {
	os.glob('an_unknown_folder/*.v') or {
		assert true
		return
	}
}
