module cmdexec

import os

fn test_run_missing_executable_returns_a_failure_result() {
	name := 'v_cmdexec_missing_${os.getpid()}_does_not_exist'
	result := run(name, [])
	assert result.exit_code != 0
	assert result.output.contains('failed to find executable'), result.output
	assert result.output.contains(name), result.output
}
