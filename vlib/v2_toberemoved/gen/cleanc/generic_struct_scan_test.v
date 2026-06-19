module cleanc

fn test_generic_struct_scan_job_count_caps_runtime_jobs() {
	assert generic_struct_scan_job_count(128, 1000) == max_generic_struct_scan_jobs
	assert generic_struct_scan_job_count(64, 15) == 3
	assert generic_struct_scan_job_count(2, 1000) == 2
	assert generic_struct_scan_job_count(4, 0) == 0
	assert generic_struct_scan_job_count(0, 4) == 0
}
