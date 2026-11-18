import os

const exec_helpers_dir = os.join_path(os.vtmp_dir(), 'exec helpers ${os.getpid()}')
const exec_helpers_exe = os.join_path(exec_helpers_dir, $if windows { 'args.exe' } $else { 'args' })
const exec_helpers_source = '
import os
import time

fn main() {
	match os.args[1] {
		"stream-output" {
			unbuffer_stdout()
			// Exercise a wait for the first byte and a wait for the rest of a line.
			time.sleep(100 * time.millisecond)
			println("first")
			print("sec")
			time.sleep(100 * time.millisecond)
			println("ond")
			println("")
			print("last")
			exit(23)
		}
		"fail" {
			exit(23)
		}
		else {
			for arg in os.args[1..] {
				println(arg)
			}
		}
	}
}
'

fn testsuite_begin() {
	os.rmdir_all(exec_helpers_dir) or {}
	os.mkdir_all(exec_helpers_dir)!
	source := os.join_path(exec_helpers_dir, 'args.v')
	os.write_file(source, exec_helpers_source)!
	build := os.exec([@VEXE, '-o', exec_helpers_exe, source])
	assert build.exit_code == 0, build.output
}

fn testsuite_end() {
	os.rmdir_all(exec_helpers_dir) or {}
}

fn test_exec_helpers_preserve_arguments_and_exit_codes() {
	marker := os.join_path(exec_helpers_dir, 'injected')
	args := [exec_helpers_exe, '', 'two words', "'quotes'", '"double quotes"', 'trailing\\',
		'; touch ${marker}', '$(touch ${marker})', '`touch ${marker}`', '& echo injected',
		'| echo injected', '> ${marker}', '%PATH%']
	expected := args[1..].join('\n') + '\n'
	for result in [os.exec(args), os.exec_opt(args)!, os.exec_or_panic(args), os.exec_or_exit(args)] {
		assert result.exit_code == 0, result.output
		assert result.output.replace('\r\n', '\n') == expected, result.output
	}
	mut stream := os.start_new_command_args(args)!
	for argument in args[1..] {
		assert stream.read_line().trim_right('\r') == argument
	}
	assert stream.read_line() == ''
	assert stream.eof
	stream.close()!
	assert stream.exit_code == 0
	assert !os.exists(marker)
	assert os.system_args([exec_helpers_exe, 'fail']) == 23
	assert os.exec([exec_helpers_exe, 'fail']).exit_code == 23
	os.exec_opt([exec_helpers_exe, 'fail']) or {
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
	mut command := os.start_new_command_args([exec_helpers_exe, 'stream-output'])!
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
