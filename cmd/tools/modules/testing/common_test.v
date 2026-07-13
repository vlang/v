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

fn test_add_automatic_execution_retry() {
	mut details := TestDetails{
		retry: 2
	}
	add_automatic_execution_retry(mut details, os.Result{
		exit_code: -1
		output:    'exec("test") failed'
	})
	assert details.retry == 3
	add_automatic_execution_retry(mut details, os.Result{
		exit_code: 1
		output:    'test assertion failed'
	})
	assert details.retry == 3
}
