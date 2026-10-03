import os

fn test_exec_helpers_preserve_arguments_and_exit_codes() {
	root := os.join_path(os.vtmp_dir(), 'exec helpers ${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'args.v')
	binary := os.join_path(root, if os.user_os() == 'windows' { 'args.exe' } else { 'args' })
	os.write_file(source, "import os\nfn main() {\n if os.args[1] == 'fail' { exit(23) }\n for arg in os.args[1..] { println(arg) }\n}\n")!
	build := os.exec([@VEXE, '-o', binary, source])
	assert build.exit_code == 0, build.output
	marker := os.join_path(root, 'injected')
	args := [binary, '', 'two words', "'quotes'", '"double quotes"', 'trailing\\', '; touch ${marker}',
		'$(touch ${marker})', '`touch ${marker}`', '& echo injected', '| echo injected', '> ${marker}',
		'%PATH%']
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
	assert os.system_args([binary, 'fail']) == 23
	assert os.exec([binary, 'fail']).exit_code == 23
	os.exec_opt([binary, 'fail']) or {
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
	root := os.join_path(os.vtmp_dir(), 'exec delayed writer ${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'delayed.v')
	binary := os.join_path(root, if os.user_os() == 'windows' { 'delayed.exe' } else { 'delayed' })
	os.write_file(source, '
import time

fn main() {
	unbuffer_stdout()
	time.sleep(100 * time.millisecond)
	println("first")
	time.sleep(100 * time.millisecond)
	print("sec")
	time.sleep(100 * time.millisecond)
	println("ond")
	time.sleep(100 * time.millisecond)
	println("")
	time.sleep(100 * time.millisecond)
	print("last")
	exit(23)
}
')!
	build := os.exec([@VEXE, '-o', binary, source])
	assert build.exit_code == 0, build.output
	mut command := os.start_new_command_args([binary])!
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
