import os

fn test_glob_can_find_v_files_3_levels_deep() ? {
	os.chdir(@VMODROOT)
	matches := os.glob('vlib/v/*/*.v') ?
	assert matches.len > 10
	assert 'vlib/v/ast/ast.v' in matches
	assert 'vlib/v/ast/table.v' in matches
	assert 'vlib/v/token/token.v' in matches
	for f in matches {
		// println(f)
		if !f.starts_with('vlib/v/') {
			assert false
		}
		assert f.ends_with('.v')
	}
}

fn test_glob_can_find_files_in_current_folder() ? {
	os.chdir(@VMODROOT)
	matches := os.glob('*') ?
	assert 'README.md' in matches
	assert 'v.mod' in matches
	assert 'cmd' in matches
	assert 'vlib' in matches
	for f in matches {
		assert !f.ends_with('.v')
	}
}

fn test_glob_star() ? {
	os.chdir(@VMODROOT)
	matches := os.glob('*ake*') ?
	assert 'Makefile' in matches
	assert 'make.bat' in matches
}

fn test_glob_not_found() ? {
	os.glob('an_unknown_folder/*.v') or {
		assert true
		return
	}
}
