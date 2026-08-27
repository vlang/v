import os

const vexe = @VEXE

fn test_verbose_prints_command_result() {
	command := '${os.quoted_path(vexe)} version'
	for option in ['-v', '--verbose'] {
		result :=
			os.execute('${os.quoted_path(vexe)} repeat -S -r 1 -w 0 ${option} ${os.quoted_path(command)}')
		assert result.exit_code == 0, result.output
		assert result.output.contains('exit code: 0'), result.output
		assert result.output.contains('V 0.'), result.output
	}
}
