module cmdexec

import os

const resolve_program_probe_env = 'VTEST_CMDEXEC_RESOLVE_PROGRAM_PROBE'

fn testsuite_begin() {
	if os.getenv(resolve_program_probe_env) != '1' {
		return
	}
	println(os.real_path(os.executable()))
	println(os.real_path(os.getwd()))
	for arg in os.args[1..] {
		println(arg)
	}
	exit(0)
}

fn test_run_keeps_the_local_executable_selected_before_spawning() {
	parent := os.getwd()
	test_executable := os.executable()
	root := os.join_path(os.vtmp_dir(), 'cmdexec resolve program ${os.getpid()}')
	path_dir := os.join_path(root, 'path programs')
	work_dir := os.join_path(root, 'child directory')
	program := 'cmdexec probe' + $if windows { '.exe' } $else { '' }
	local_binary := os.join_path(root, program)
	path_binary := os.join_path(path_dir, program)
	os.mkdir_all(path_dir)!
	os.mkdir_all(work_dir)!
	previous_path := os.getenv_opt('PATH')
	previous_probe := os.getenv_opt(resolve_program_probe_env)
	defer {
		os.chdir(parent) or { panic(err) }
		if value := previous_path {
			os.setenv('PATH', value, true)
		} else {
			os.unsetenv('PATH')
		}
		if value := previous_probe {
			os.setenv(resolve_program_probe_env, value, true)
		} else {
			os.unsetenv(resolve_program_probe_env)
		}
		os.rmdir_all(root) or {}
	}
	os.cp(test_executable, local_binary)!
	os.cp(test_executable, path_binary)!
	$if !windows {
		os.chmod(local_binary, 0o700)!
		os.chmod(path_binary, 0o700)!
	}
	os.setenv('PATH', path_dir, true)
	os.setenv(resolve_program_probe_env, '1', true)
	os.chdir(root)!
	args := ['one argument', '', 'quote"here', '*']
	mut expected := [os.real_path(local_binary), os.real_path(work_dir)]
	expected << args

	// A different executable with the same basename exists on PATH. Once the
	// local file has passed the executable check, spawning must not select it.
	for candidate in [program, '.' + os.path_separator + program, local_binary] {
		resolved := resolve_program(candidate) or { panic('could not resolve ${candidate}') }
		assert os.is_abs_path(resolved)
		assert os.real_path(resolved) == os.real_path(local_binary)
		result := run_in(candidate, args, work_dir)
		assert result.exit_code == 0, result.output
		assert result.output.split_into_lines() == expected, result.output
		merged := run_in_merged(candidate, args, work_dir)
		assert merged.exit_code == 0, merged.output
		assert merged.output.split_into_lines() == expected, merged.output
	}

	expected[1] = os.real_path(root)
	plain := run(program, args)
	assert plain.exit_code == 0, plain.output
	assert plain.output.split_into_lines() == expected, plain.output
	bounded := run_with_timeout(program, args, 5_000)
	assert bounded.exit_code == 0, bounded.output
	assert bounded.output.split_into_lines() == expected, bounded.output

	// PATH lookup is still used when there is no executable in the parent cwd.
	os.rm(local_binary)!
	expected[0] = os.real_path(path_binary)
	expected[1] = os.real_path(work_dir)
	from_path := run_in(program, args, work_dir)
	assert from_path.exit_code == 0, from_path.output
	assert from_path.output.split_into_lines() == expected, from_path.output
}
