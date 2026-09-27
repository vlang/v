module driver

fn test_a_diagnostics_server_check_takes_more_of_the_pool() {
	assert scoped_linux_job_limit(true, true) == diagnostics_server_job_limit
	assert diagnostics_server_job_limit > scoped_linux_user_job_limit
}

fn test_a_build_or_a_one_shot_check_keeps_the_scoped_limit() {
	assert scoped_linux_job_limit(false, true) == scoped_linux_user_job_limit
	assert scoped_linux_job_limit(true, false) == scoped_linux_user_job_limit
	assert scoped_linux_job_limit(false, false) == scoped_linux_user_job_limit
}
