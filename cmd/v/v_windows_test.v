module main

import os

fn test_invalid_c_compiler_is_reported_before_command_dispatch() {
	$if !windows {
		return
	}
	missing_compiler := 'missing_compiler_27868'
	expected_error := 'builder error: C compiler `${missing_compiler}` was requested with `-cc`, but was not found.'
	for command in ['', 'not-a-command'] {
		result := os.execute('${os.quoted_path(@VEXE)} -cc ${missing_compiler} ${command}')
		assert result.exit_code == 1
		assert result.output.contains(expected_error), result.output
	}
	help_result := os.execute('${os.quoted_path(@VEXE)} -cc ${missing_compiler} help')
	assert help_result.exit_code == 1
	assert help_result.output.contains('provide only one help topic.'), help_result.output
	assert !help_result.output.contains(expected_error), help_result.output

	version_result := os.execute('${os.quoted_path(@VEXE)} -cc ${missing_compiler} version')
	assert version_result.exit_code == 0, version_result.output
	assert !version_result.output.contains(expected_error), version_result.output
}
