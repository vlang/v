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

fn test_automatic_test_jobs_respects_memory_and_cpu_limits() {
	assert automatic_test_jobs(18, u64(128) * 1024 * 1024 * 1024, 0) == 4
	assert automatic_test_jobs(18, u64(32) * 1024 * 1024 * 1024, 0) == 4
	assert automatic_test_jobs(18, u64(16) * 1024 * 1024 * 1024, 0) == 2
	assert automatic_test_jobs(18, u64(8) * 1024 * 1024 * 1024, 0) == 1
	assert automatic_test_jobs(2, u64(128) * 1024 * 1024 * 1024, 0) == 2
	assert automatic_test_jobs(0, 0, 0) == 1
}

fn test_automatic_test_jobs_preserves_vjobs_override() {
	assert automatic_test_jobs(18, u64(16) * 1024 * 1024 * 1024, 7) == 7
}
