import os

// build_enable_long_paths_tool compiles the tool and returns its path.
// Tool tests build the tool like this, because a `_test.v` file cannot see
// the private functions of its sibling tool source.
fn build_enable_long_paths_tool() !string {
	root := os.join_path(os.vtmp_dir(), 'enable_long_paths_${os.getpid()}')
	os.mkdir_all(root)!
	tool := os.join_path(root, 'venable-long-paths' + $if windows { '.exe' } $else { '' })
	build := os.exec([@VEXE, '-o', tool, os.join_path(@VMODROOT, 'cmd', 'tools',
		'venable-long-paths.v')])
	assert build.exit_code == 0, build.output
	return tool
}

fn test_enable_long_paths_help() {
	tool := build_enable_long_paths_tool()!
	defer {
		os.rmdir_all(os.dir(tool)) or {}
	}
	res := os.exec([tool, '--help'])
	assert res.exit_code == 0, res.output
	assert res.output.contains('Usage:'), res.output
	assert res.output.contains('--check'), res.output
}

fn test_enable_long_paths_rejects_unknown_options() {
	tool := build_enable_long_paths_tool()!
	defer {
		os.rmdir_all(os.dir(tool)) or {}
	}
	res := os.exec([tool, '--bogus'])
	assert res.exit_code == 1, res.output
	assert res.output.contains('unknown option `--bogus`'), res.output
}

fn test_enable_long_paths_check_reports_without_writing() {
	tool := build_enable_long_paths_tool()!
	defer {
		os.rmdir_all(os.dir(tool)) or {}
	}
	// Read-only: safe on every machine, including CI.
	status := os.long_paths_status()
	status_word := match status {
		.enabled { 'enabled' }
		.disabled { 'disabled' }
		.unknown { 'unknown' }
	}
	res := os.exec([tool, '--check'])
	assert res.output.trim_space() == 'Win32 long paths are ${status_word}.', res.output
	if status == os.LongPathsStatus.enabled {
		assert res.exit_code == 0, res.output
	} else {
		assert res.exit_code == 1, res.output
	}
}

fn test_enable_long_paths_reports_when_already_enabled() {
	if os.long_paths_status() != os.LongPathsStatus.enabled {
		// Enabling would change machine state; the access-denied branch is
		// verified manually on a non-administrator machine.
		eprintln('skipping: Win32 long paths are not enabled here')
		return
	}
	tool := build_enable_long_paths_tool()!
	defer {
		os.rmdir_all(os.dir(tool)) or {}
	}
	res := os.exec([tool])
	assert res.exit_code == 0, res.output
	assert res.output.contains('already enabled'), res.output
}
