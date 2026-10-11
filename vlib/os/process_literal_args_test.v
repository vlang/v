import os

fn test_exec_process_and_v_run_preserve_literal_arguments() {
	root := os.join_path(os.vtmp_dir(), 'literal process arguments ${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'argv.v')
	binary := os.join_path(root, $if windows { 'argv.exe' } $else { 'argv' })
	os.write_file(source, 'import os\nfn main() {\n\tfor arg in os.args[1..] {\n\t\tprintln("\${arg.len}:\${arg}")\n\t}\n}\n')!
	build := os.exec([@VEXE, '-o', binary, source])
	assert build.exit_code == 0, build.output

	env_name := 'V_LITERAL_PROCESS_ARGUMENT_${os.getpid()}'
	env_value := r'expanded C:\path\'
	os.setenv(env_name, env_value, true)
	defer { os.unsetenv(env_name) }
	mut arguments := ['', 'one argument', '$literal', 'quote " and punctuation ;', '%PATH%',
		'%${env_name}%', r'C:\trailing\', r'C:\two trailing\\', r'\\server\share\', '& | < > ; !',
		'tab\targument', 'héllo 世界']
	for count in 0 .. 6 {
		arguments << 'before' + '\\'.repeat(count) + '"after'
		arguments << 'trailing' + '\\'.repeat(count)
	}
	mut expected := ''
	for argument in arguments {
		expected += '${argument.len}:${argument}\n'
	}
	mut argv := [binary]
	argv << arguments
	result := os.exec(argv)
	assert result.exit_code == 0, result.output
	assert result.output.replace('\r\n', '\n') == expected, result.output

	mut process := os.new_process(binary)
	process.set_args(arguments)
	process.set_redirect_stdio()
	process.wait()
	output := process.stdout_slurp()
	errors := process.stderr_slurp()
	code := process.code
	process.close()
	assert code == 0, '${output}\n${errors}'
	assert output.replace('\r\n', '\n') == expected, output

	// The compiler forwards `v run` arguments through Process too.
	mut run_argv := [@VEXE, 'run', source]
	run_argv << arguments
	run_result := os.exec(run_argv)
	assert run_result.exit_code == 0, run_result.output
	assert run_result.output.replace('\r\n', '\n') == expected, run_result.output

	$if windows {
		// Command strings still delegate expansion to the shell.
		shell := os.execute('echo %${env_name}%')
		assert shell.exit_code == 0, shell.output
		assert shell.output.trim_right('\r\n') == env_value, shell.output
	}
}
