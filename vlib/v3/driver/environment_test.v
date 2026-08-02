module driver

import os

fn restore_driver_environment(name string, old_value string, was_set bool) {
	if was_set {
		os.setenv(name, old_value, true)
	} else {
		os.unsetenv(name)
	}
}

fn test_v3_environment_coverage_dir_reads_vcovdir() {
	name := 'VCOVDIR'
	old_value := os.getenv(name)
	was_set := name in os.environ()
	defer {
		restore_driver_environment(name, old_value, was_set)
	}
	os.unsetenv(name)
	assert v3_environment_coverage_dir() == ''
	path := os.join_path(os.temp_dir(), 'v3_environment_coverage_${os.getpid()}')
	os.setenv(name, path, true)
	assert v3_environment_coverage_dir() == os.real_path(path)
}

fn test_v3_environment_run_only_reads_vtest_only_fn() {
	name := 'VTEST_ONLY_FN'
	old_value := os.getenv(name)
	was_set := name in os.environ()
	defer {
		restore_driver_environment(name, old_value, was_set)
	}
	os.unsetenv(name)
	assert v3_environment_run_only() == []
	os.setenv(name, 'test_one,test_two', true)
	assert v3_environment_run_only() == ['test_one', 'test_two']
}

fn test_v3_run_only_cache_identity_distinguishes_patterns() {
	assert v3_run_only_cache_identity([]) == ''
	first := v3_run_only_cache_identity(['test_one'])
	second := v3_run_only_cache_identity(['test_two'])
	assert first != second
	left := v3_run_only_cache_identity(['a', 'bc'])
	right := v3_run_only_cache_identity(['ab', 'c'])
	assert left != right
}

fn test_v3_effective_warns_are_errors_includes_prod() {
	assert !v3_effective_warns_are_errors(false, false)
	assert v3_effective_warns_are_errors(true, false)
	assert v3_effective_warns_are_errors(false, true)
	assert v3_effective_warns_are_errors(true, true)
}
