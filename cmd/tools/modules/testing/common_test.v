module testing

import os

fn test_should_retry_execution() {
	assert should_retry_execution(os.Result{
		exit_code: -1
		output:    'exec("test") failed'
	})
	assert should_retry_execution(os.Result{
		exit_code: 8
		output:    'exec failed (CreateProcess) with code 8: Not enough memory resources.'
	})
	assert should_retry_execution(os.Result{
		exit_code: 1
	})
	assert !should_retry_execution(os.Result{
		exit_code: 1
		output:    'test assertion failed'
	})
	assert !should_retry_execution(os.Result{
		exit_code: -1
		output:    'child crashed'
	})
}
