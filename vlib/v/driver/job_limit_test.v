module driver

fn test_a_diagnostics_server_check_takes_more_of_the_pool() {
	assert scoped_user_job_limit(true, true) == diagnostics_server_job_limit
	assert diagnostics_server_job_limit > scoped_user_job_count
}

fn test_a_build_or_a_one_shot_check_keeps_the_scoped_limit() {
	assert scoped_user_job_limit(false, true) == scoped_user_job_count
	assert scoped_user_job_limit(true, false) == scoped_user_job_count
	assert scoped_user_job_limit(false, false) == scoped_user_job_count
}

fn test_scoped_user_pool_limits_preserve_explicit_macos_jobs() {
	assert should_limit_scoped_user_jobs(false, true, 'macos', '')
	assert !should_limit_scoped_user_jobs(false, true, 'macos', '1')
	assert !should_limit_scoped_user_jobs(false, true, 'macos', '18')
	assert should_limit_scoped_user_jobs(false, true, 'linux', '')
	assert should_limit_scoped_user_jobs(false, true, 'linux', '18')
	for host in ['linux', 'macos'] {
		assert !should_limit_scoped_user_jobs(true, true, host, '')
		assert !should_limit_scoped_user_jobs(false, false, host, '')
	}
	assert !should_limit_scoped_user_jobs(false, true, 'windows', '')
}
