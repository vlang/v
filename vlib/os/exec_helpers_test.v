import os
import time

const exec_helpers_dir = os.join_path(os.vtmp_dir(), 'exec helpers ${os.getpid()}')
const exec_helpers_exe = os.join_path(exec_helpers_dir, $if windows { 'args.exe' } $else { 'args' })
const exec_helpers_probe = '--vtest-exec-helpers'

fn run_exec_helpers_probe(args []string) {
	match args[0] {
		'stream-output' {
			unbuffer_stdout()
			// Exercise a wait for the first byte and a wait for the rest of a line.
			time.sleep(100 * time.millisecond)
			println('first')
			print('sec')
			time.sleep(100 * time.millisecond)
			println('ond')
			println('')
			print('last')
			exit(23)
		}
		'fail' {
			exit(23)
		}
		else {
			for arg in args {
				println(arg)
			}
		}
	}
}

fn testsuite_begin() {
	if os.args.len >= 3 && os.args[1] == exec_helpers_probe {
		run_exec_helpers_probe(os.args[2..])
		exit(0)
	}
	os.rmdir_all(exec_helpers_dir) or {}
	os.mkdir_all(exec_helpers_dir)!
	os.cp(os.executable(), exec_helpers_exe)!
}

fn testsuite_end() {
	os.rmdir_all(exec_helpers_dir) or {}
}

fn test_exec_helpers_preserve_arguments_and_exit_codes() {
	marker := os.join_path(exec_helpers_dir, 'injected')
	args := [exec_helpers_exe, exec_helpers_probe, '', 'two words', "'quotes'", '"double quotes"',
		'trailing\\', '; touch ${marker}', '$(touch ${marker})', '`touch ${marker}`', '& echo injected',
		'| echo injected', '> ${marker}', '%PATH%']
	expected := args[2..].join('\n') + '\n'
	for result in [os.exec(args), os.exec_opt(args)!, os.exec_or_panic(args), os.exec_or_exit(args)] {
		assert result.exit_code == 0, result.output
		assert result.output.replace('\r\n', '\n') == expected, result.output
	}
	mut stream := os.start_new_command_args(args)!
	for argument in args[2..] {
		assert stream.read_line().trim_right('\r') == argument
	}
	assert stream.read_line() == ''
	assert stream.eof
	stream.close()!
	assert stream.exit_code == 0
	assert !os.exists(marker)
	failing_command := [exec_helpers_exe, exec_helpers_probe, 'fail']
	assert os.system_args(failing_command) == 23
	assert os.exec(failing_command).exit_code == 23
	os.exec_opt(failing_command) or {
		assert err.msg() == ''
		return
	}
	assert false
}

fn test_exec_helpers_reject_empty_arguments() {
	assert os.exec([]string{}).exit_code == -1
	assert os.system_args([]string{}) == -1
	assert os.system_args(['v_exec_helpers_missing_${os.getpid()}']) == -1
	os.exec_opt([]string{}) or {
		assert err.msg() == 'exec requires at least one argument'
		return
	}
	assert false
}

fn test_start_new_command_args_streams_literal_arguments() {
	mut command := os.start_new_command_args([@VEXE, 'version'])!
	line := command.read_line()
	assert line.starts_with('V '), line
	for !command.eof {
		command.read_line()
	}
	command.close()!
	assert command.exit_code == 0
	os.start_new_command_args([]string{}) or {
		assert err.msg() == 'start_new_command_args requires at least one argument'
		return
	}
	assert false
}

fn test_start_new_command_args_waits_for_delayed_output() {
	mut command := os.start_new_command_args([exec_helpers_exe, exec_helpers_probe, 'stream-output'])!
	assert command.read_line().trim_right('\r') == 'first'
	assert !command.eof
	assert command.read_line().trim_right('\r') == 'second'
	assert !command.eof
	assert command.read_line().trim_right('\r') == ''
	assert !command.eof
	assert command.read_line() == 'last'
	assert command.eof
	assert command.read_line() == ''
	command.close()!
	assert command.exit_code == 23
}
