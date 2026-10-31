import os

const vexe = @VEXE

fn test_verbose_prints_command_result() {
	command := 'echo vrepeat_verbose_output'
	for option in ['-v', '--verbose'] {
		result :=
			os.exec([vexe, 'repeat', '-S', '-r', '1', '-w', '0', '${option}', '${command}'])
		assert result.exit_code == 0, result.output
		assert result.output.contains('exit code: 0'), result.output
		assert result.output.split_into_lines().any(it.trim_space() == 'vrepeat_verbose_output'), result.output
	}
}
