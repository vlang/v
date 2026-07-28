module errors

import os

fn test_relative_error_path_honors_absolute_path_requests() {
	old_value := os.getenv_opt('VERROR_PATHS')
	defer {
		if value := old_value {
			os.setenv('VERROR_PATHS', value, true)
		} else {
			os.unsetenv('VERROR_PATHS')
		}
	}
	path := os.join_path(os.getwd(), 'vlib', 'v3', 'errors', 'format.v')
	absolute_path := os.real_path(path).replace('\\', '/')
	os.setenv('VERROR_PATHS', 'absolute', true)
	assert relative_error_path(path) == absolute_path
	os.unsetenv('VERROR_PATHS')
	assert relative_error_path(path) == 'vlib/v3/errors/format.v'
}
