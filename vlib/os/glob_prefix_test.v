import os

// `./x` and `../x` say where the search starts. They used to match nothing at
// all, so they are checked on their own, away from the patterns that name a
// folder directly. `glob(3)` keeps such a prefix verbatim in what it reports,
// and so does `os.glob`.

fn test_glob_keeps_a_dot_prefix() {
	$if windows {
		return
	}
	os.chdir(@VMODROOT)!
	bare := os.glob('vlib/os/*.v')!
	dotted := os.glob('./vlib/os/*.v')!
	assert bare.len > 10
	assert dotted == bare.map('./${it}')
}

fn test_glob_keeps_a_parent_prefix() {
	$if windows {
		return
	}
	os.chdir(os.join_path(@VMODROOT, 'cmd'))!
	matches := os.glob('../vlib/os/*.v')!
	assert matches.len > 10
	assert '../vlib/os/os.v' in matches
	for match_ in matches {
		assert match_.starts_with('../vlib/os/')
		assert match_.ends_with('.v')
	}
	os.chdir(@VMODROOT)!
}

fn test_glob_keeps_a_repeated_prefix_verbatim() {
	$if windows {
		return
	}
	os.chdir(@VMODROOT)!
	assert os.glob('././vlib/os/*.v')! == os.glob('vlib/os/*.v')!.map('././${it}')
	os.chdir(os.join_path(@VMODROOT, 'cmd'))!
	assert '.././vlib/os/os.v' in os.glob('.././vlib/os/*.v')!
	os.chdir(@VMODROOT)!
}
