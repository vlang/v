module cmdexec

import os

fn test_run_in_merged_with_env_gives_the_child_the_variables_on_top_of_its_own() {
	$if windows {
		return
	}
	shell := os.find_abs_path_of_executable('sh') or { return }
	os.setenv('V_CMDEXEC_ENV_TEST_KEPT', 'kept', true)
	os.setenv('V_CMDEXEC_ENV_TEST_REPLACED', 'old', true)
	defer {
		os.unsetenv('V_CMDEXEC_ENV_TEST_KEPT')
		os.unsetenv('V_CMDEXEC_ENV_TEST_REPLACED')
	}
	result := run_in_merged_with_env(shell, ['-c',
		'echo "$V_CMDEXEC_ENV_TEST_KEPT $V_CMDEXEC_ENV_TEST_REPLACED $V_CMDEXEC_ENV_TEST_ADDED"; echo err >&2'],
		'', {
			'V_CMDEXEC_ENV_TEST_REPLACED': 'new'
			'V_CMDEXEC_ENV_TEST_ADDED':    'added'
		})
	assert result.exit_code == 0, result.output
	assert result.output.contains('kept new added'), result.output
	assert result.output.contains('err'), result.output
	// The environment of this process is what it was.
	assert os.getenv('V_CMDEXEC_ENV_TEST_REPLACED') == 'old'
	assert os.getenv_opt('V_CMDEXEC_ENV_TEST_ADDED') == none
	// A program that is not there is reported like one of the other helpers.
	missing := run_in_merged_with_env('/nonexistent/v-cmdexec-env-test', []string{}, '',
		{
			'LC_ALL': 'C'
		})
	assert missing.exit_code != 0
}
