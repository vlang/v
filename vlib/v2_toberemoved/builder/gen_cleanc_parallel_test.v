module builder

fn test_cleanc_parallel_pass5_job_count_caps_runtime_jobs() {
	assert cleanc_parallel_pass5_job_count(128, 1000) == max_cleanc_pass5_jobs
	assert cleanc_parallel_pass5_job_count(64, 3) == 3
	assert cleanc_parallel_pass5_job_count(2, 1000) == 2
	assert cleanc_parallel_pass5_job_count(4, 0) == 0
	assert cleanc_parallel_pass5_job_count(0, 4) == 0
}
