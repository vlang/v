module cmdexec

import os

// os.Process aborts the whole process on Windows when CreateProcess cannot find
// the program - `failed CreateProcess: The system cannot find the file
// specified.` and exit(1), without naming it. A command that cannot start has to
// come back as a failing result instead, so probes can fall back and real
// failures say what was missing.
fn test_a_missing_command_fails_instead_of_aborting() {
	res := run('a_command_that_does_not_exist_anywhere', ['-v'])
	assert res.exit_code != 0
	assert res.output.contains('a_command_that_does_not_exist_anywhere'), res.output
}

fn test_a_missing_absolute_command_fails_instead_of_aborting() {
	missing := os.join_path(os.vtmp_dir(), 'v_cmdexec_missing_${os.getpid()}')
	res := run(missing, [])
	assert res.exit_code != 0
	assert res.output.contains(missing), res.output
}

fn test_a_present_command_still_runs() {
	$if windows {
		// Use an absolute path. os.Process abs_path()s a bare filename against the
		// caller's folder before CreateProcessW, so a bare `cmd` only resolves when
		// the test happens to run inside System32. COMSPEC is the documented way to
		// locate the shell.
		mut shell := os.getenv('COMSPEC')
		if shell == '' {
			shell = os.find_abs_path_of_executable('cmd') or { '' }
		}
		assert shell != '', 'neither COMSPEC nor `cmd` on PATH'
		res := run_in(shell, ['/c', 'echo', 'hello'], '')
		assert res.exit_code == 0, res.output
		assert res.output.contains('hello'), res.output
	} $else {
		res := run_in('/bin/echo', ['hello'], '')
		assert res.exit_code == 0, res.output
		assert res.output.contains('hello'), res.output
	}
}

// A relative program that carries a separator is resolved against the *caller's*
// folder, not the work folder: os.Process abs_path()s the filename before the
// child switches folders. A preflight check that probed `work_folder/./wrapper`
// instead would report a wrapper that runs fine as missing.
fn test_a_relative_command_resolves_against_the_callers_folder() {
	$if windows {
		return
	}
	tmp := os.join_path(os.vtmp_dir(), 'v_cmdexec_rel_${os.getpid()}')
	os.mkdir_all(tmp) or { panic(err) }
	defer {
		os.rmdir_all(tmp) or {}
	}
	work := os.join_path(tmp, 'work')
	os.mkdir_all(work) or { panic(err) }
	script := os.join_path(tmp, 'wrapper')
	os.write_file(script, '#!/bin/sh\necho wrapped\n') or { panic(err) }
	os.chmod(script, 0o755) or { panic(err) }
	previous := os.getwd()
	os.chdir(tmp) or { panic(err) }
	defer {
		os.chdir(previous) or {}
	}
	res := run_in('./wrapper', [], work)
	assert res.exit_code == 0, res.output
	assert res.output.contains('wrapped'), res.output
}
