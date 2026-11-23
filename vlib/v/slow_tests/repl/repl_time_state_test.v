// vtest build: !musl? && !sanitized_job?
module main

import os
import v.slow_tests.repl.runner

fn test_repl_keeps_time_assignments_stable_across_reads() {
	vexec := runner.full_path_to_v(5)
	temp_dir := os.join_path(os.vtmp_dir(), 'v_repl_time_state_${os.getpid()}')
	os.mkdir_all(temp_dir) or { panic(err) }
	defer {
		os.rmdir_all(temp_dir) or {}
	}
	clock_file := os.join_path(temp_dir, 'clock.txt')
	initial_timestamp := '1700000000'
	next_timestamp := '1700000001'
	os.write_file(clock_file, initial_timestamp)!
	// Changing the initializer's input makes a replay observable immediately.
	input := [
		'import os',
		'import time',
		"a := time.unix(os.read_file('clock.txt')!.i64()).as_local()",
		'a',
		"  os.write_file('clock.txt', '${next_timestamp}')!",
		'a',
		'exit',
	].join('\n')
	original_vexe := os.getenv_opt('VEXE')
	os.setenv('VEXE', vexec, true)
	defer {
		if old_vexe := original_vexe {
			os.setenv('VEXE', old_vexe, true)
		} else {
			os.unsetenv('VEXE')
		}
	}
	mut repl := os.new_process(vexec)
	repl.set_args(['repl', '-replfolder', temp_dir, '-replprefix', 'time_state.'])
	repl.set_redirect_stdio()
	repl.create_no_window = true
	repl.run()
	repl.stdin_write(input + '\n')
	repl.wait()
	output_raw := repl.stdout_slurp() + repl.stderr_slurp()
	exit_code := repl.code
	repl.close()
	assert exit_code == 0, output_raw
	output := output_raw.replace_each(['\r', '', '>>> ', '', '>>>', '', '... ', '',
		temp_dir + os.path_separator, '', os.dir(vexec) + os.path_separator, '']).trim_right('\n\r')
	lines := output.split_into_lines()
	assert os.read_file(clock_file)! == next_timestamp
	assert lines.len == 2, 'expected 2 repl outputs, got ${lines.len}: ${lines}'
	assert lines[0] == lines[1], 'expected the stored time to remain stable, got ${lines}'
}

fn test_repl_bypasses_local_cmd_exe_on_windows() {
	$if !windows {
		return
	}
	vexec := runner.full_path_to_v(5)
	temp_dir := os.join_path(os.vtmp_dir(), 'v_repl_cmd_bypass_${os.getpid()}')
	os.rmdir_all(temp_dir) or {}
	os.mkdir_all(temp_dir) or { panic(err) }
	defer {
		os.rmdir_all(temp_dir) or {}
	}
	fake_cmd_source := os.join_path(temp_dir, 'fake_cmd.v')
	fake_cmd_exe := os.join_path(temp_dir, 'cmd.exe')
	os.write_file(fake_cmd_source, "fn main() {\n\tprintln('fake cmd invoked')\n}\n") or {
		panic(err)
	}
	build_fake_cmd :=
		os.exec([vexec, '-o', fake_cmd_exe, fake_cmd_source])
	assert build_fake_cmd.exit_code == 0, build_fake_cmd.output
	original_vexe := os.getenv('VEXE')
	os.setenv('VEXE', vexec, true)
	original_vflags := os.getenv('VFLAGS')
	os.setenv('VFLAGS', '', true)
	defer {
		os.setenv('VEXE', original_vexe, true)
		os.setenv('VFLAGS', original_vflags, true)
	}
	mut repl := os.new_process(vexec)
	repl.set_args(['repl', '-replfolder', temp_dir, '-replprefix', 'cmd_bypass.'])
	repl.set_redirect_stdio()
	repl.create_no_window = true
	repl.run()
	repl.stdin_write('println(1 + 1)\nexit\n')
	repl.wait()
	output := repl.stdout_slurp() + repl.stderr_slurp()
	repl.close()
	assert repl.code == 0, output
	assert !output.contains('fake cmd invoked'), output
	assert output.trim_space() == '2', output
}
