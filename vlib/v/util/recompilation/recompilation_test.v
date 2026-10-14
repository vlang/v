module recompilation

import os

const recompilation_scenario_env = 'RECOMPILATION_SCENARIO'

// Package managers `touch` that file to keep `v up`/`v self` from rebuilding a
// distro install, so the path is a contract, not an implementation detail.
fn test_disabling_file_lives_beside_the_tool_sources() {
	path := disabling_file('/vroot')
	assert path == os.join_path_single(os.join_path('/vroot', 'cmd', 'tools'), '.disable_autorecompilation')
	assert path.contains('tools')
	// Absent by default in a checkout that has not disabled it.
	assert !os.exists(disabling_file(@VEXEROOT))
}

// The guard only fires when the marker is present; when it is not, the tool is
// allowed to run and nothing is printed.
fn test_must_be_enabled_passes_when_the_marker_is_absent() {
	root := os.join_path(os.vtmp_dir(), 'recompilation_enabled_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(os.join_path(root, 'cmd', 'tools')) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	must_be_enabled(root, 'please install V from source')
	assert !os.exists(disabling_file(root))
}

// The abort path prints both lines and exits, so it is observed in a child.
fn test_must_be_enabled_aborts_when_the_marker_is_present() {
	if os.getenv(recompilation_scenario_env) != '' {
		abort_scenario(os.getenv(recompilation_scenario_env))
		return
	}
	root := os.join_path(os.vtmp_dir(), 'recompilation_disabled_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(os.join_path(root, 'cmd', 'tools')) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(disabling_file(root), '') or { panic(err) }
	mut environment := os.environ()
	environment[recompilation_scenario_env] = root
	// The child runs only this one test, so its output is nothing else.
	environment['VTEST_ONLY'] = 'test_must_be_enabled_aborts_when_the_marker_is_present'
	mut process := os.new_process(os.executable())
	process.set_environment(environment)
	process.set_redirect_stdio()
	process.run()
	process.wait()
	output := process.stdout_slurp() + process.stderr_slurp()
	process.close()
	assert process.code == 1, output
	assert output.contains('Recompilation is disabled, since there is a "${disabling_file(root)}" file present.'), output
	assert output.contains('please install V from source'), output
}

fn abort_scenario(root string) {
	must_be_enabled(root, 'please install V from source')
	println('must_be_enabled returned, it should have exited')
}
