import os
import time

fn test_inline_code_separate_and_equals_arguments() {
	for eval_args in [['-e', 'println(1)'], ['-e=println(1)']] {
		mut args := [@VEXE, '-new-compiler', '-no-retry-compilation', '-nocache']
		args << eval_args
		result := os.exec(args)
		assert result.exit_code == 0, result.output
		assert result.output.trim_space() == '1'
	}
}

fn test_inline_code_forwards_arguments_and_exit_status() {
	result := os.exec([@VEXE, '-new-compiler', '-no-retry-compilation', '-nocache', '-e',
		'import os; println(os.args[1..]); exit(7)', '--custom', 'with spaces'])
	assert result.exit_code == 7, result.output
	assert result.output.trim_space() == "['--custom', 'with spaces']"
}

fn test_inline_code_reports_missing_source() {
	result := os.exec([@VEXE, '-new-compiler', '-no-retry-compilation', '-e'])
	assert result.exit_code != 0
	assert result.output.contains('option `-e` requires a value')
}

fn test_http_shortcut_accepts_the_option() {
	// Syntax checking validates the shortcut without leaving a server running.
	result := os.exec([@VEXE, '-new-compiler', '-no-retry-compilation', '-nocache', '-check-syntax',
		'-http', 'doc', 'version', '--help', '-e', '-old-compiler'])
	assert result.exit_code == 0, result.output
	assert !result.output.contains('unknown option')
}

fn test_inline_code_forwards_recognized_commands_and_flags() {
	argument_sets := [['version', '--version'], ['doc'], ['--help', '-h'],
		['-http', '-e', '-e=println(2)'], ['-old-compiler', '-new-compiler', '-vls-mode', '-autofree',
			'-ownership', '-race', '-json-errors', 'with spaces']]
	for eval_args in [['-e', 'import os; println(os.args[1..])'],
		['-e=import os; println(os.args[1..])']] {
		for program_args in argument_sets {
			mut args := [@VEXE, '-new-compiler', '-no-retry-compilation', '-nocache']
			args << eval_args
			args << program_args
			result := os.exec(args)
			assert result.exit_code == 0, result.output
			assert result.output.trim_space() == program_args.str(), result.output
		}
	}
}

fn assert_inline_interrupt_cleanup(eval_args []string, ready_message string, signal string, case_id int) {
	root := os.join_path(@VEXEROOT, '.v3_inline_interrupt_${os.getpid()}_${case_id}')
	os.mkdir(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	mut args := ['-new-compiler', '-no-retry-compilation', '-nocache']
	args << eval_args
	mut process := os.new_process(@VEXE)
	process.use_pgroup = true
	process.set_work_folder(root)
	process.set_args(args)
	process.set_redirect_stdio()
	process.run()
	mut output := ''
	mut started := false
	for _ in 0 .. 1800 {
		output += process.stdout_read() + process.stderr_read()
		if output.contains(ready_message) {
			started = true
			break
		}
		if !process.is_alive() {
			break
		}
		time.sleep(100 * time.millisecond)
	}
	if !started {
		if process.is_alive() {
			process.signal_pgkill()
		}
		process.wait()
		output += process.stdout_slurp() + process.stderr_slurp()
		process.close()
		assert false, 'inline program did not start: ${output}'
	}
	prefix := '.v3_inline_${process.pid}_'
	before := os.ls(root) or { panic(err) }
	interrupt := os.exec(['/bin/kill', '-${signal}', '-${process.pid}'])
	for _ in 0 .. 100 {
		output += process.stdout_read() + process.stderr_read()
		if !process.is_alive() {
			break
		}
		time.sleep(100 * time.millisecond)
	}
	timed_out := process.is_alive()
	if timed_out {
		process.signal_pgkill()
	}
	process.wait()
	output += process.stdout_slurp() + process.stderr_slurp()
	exit_code := process.code
	process.close()
	assert interrupt.exit_code == 0, interrupt.output
	assert !timed_out, 'inline wrapper did not exit after ${signal}: ${output}'
	assert exit_code != 0, output
	assert before.any(it.starts_with(prefix) && it.ends_with('.vsh')), before.str()
	assert before.any(it.starts_with(prefix) && !it.ends_with('.vsh')), before.str()
	after := os.ls(root) or { panic(err) }
	assert !after.any(it.starts_with(prefix)), after.str()
}

fn test_inline_code_interrupts_remove_temporary_source_and_program() {
	$if !windows {
		source := "import time; println('inline ready'); flush_stdout(); for { time.sleep(time.second) }"
		mut case_id := 0
		for signal in ['INT', 'QUIT'] {
			assert_inline_interrupt_cleanup(['-e', source], 'inline ready', signal, case_id)
			case_id++
			assert_inline_interrupt_cleanup(['-e=${source}'], 'inline ready', signal, case_id)
			case_id++
			// An ephemeral port avoids conflicts with an existing file server.
			assert_inline_interrupt_cleanup(['-d', 'http_on=localhost:0', '-http'],
				'Listening on http://', signal, case_id)
			case_id++
		}
	}
}
