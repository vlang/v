import os

const vexe = @VEXE

fn test_verbose_prints_command_result() {
	command := 'echo vrepeat_verbose_output'
	for option in ['-v', '--verbose'] {
		result :=
			os.execute('${os.quoted_path(vexe)} repeat -S -r 1 -w 0 ${option} ${os.quoted_path(command)}')
		assert result.exit_code == 0, result.output
		assert result.output.contains('exit code: 0'), result.output
		assert result.output.split_into_lines().any(it.trim_right('\r') == 'vrepeat_verbose_output'), result.output
	}
}
