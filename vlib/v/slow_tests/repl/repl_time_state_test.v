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
	input_file := os.join_path(temp_dir, 'input.txt')
	input := [
		'import time',
		'a := time.now()',
		'a',
		'time.sleep(1200 * time.millisecond)',
		'a',
		'exit',
	].join('\n')
	os.write_file(input_file, input) or { panic(err) }
	cmd := 'VEXE=${os.quoted_path(vexec)} ${os.quoted_path(vexec)} repl -replfolder ${os.quoted_path(temp_dir)} -replprefix "time_state." < ${os.quoted_path(input_file)}'
	res := os.execute(cmd)
	assert res.exit_code == 0, res.output
	output := res.output.replace_each(['\r', '', '>>> ', '', '>>>', '', '... ', '',
		temp_dir + os.path_separator, '', os.dir(vexec) + os.path_separator, '']).trim_right('\n\r')
	lines := output.split_into_lines()
	assert lines.len == 2, 'expected 2 repl outputs, got ${lines.len}: ${lines}'
	assert lines[0] == lines[1], 'expected the stored time to remain stable, got ${lines}'
}
