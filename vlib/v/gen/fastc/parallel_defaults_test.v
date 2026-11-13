module fastc

import os
import v.pref

fn test_generation_defaults_to_all_host_cores_and_honors_overrides() {
	previous_jobs := os.getenv_opt('VJOBS')
	previous_serial := os.getenv_opt('V3_FASTC_NO_PARALLEL')
	defer {
		if value := previous_jobs { os.setenv('VJOBS', value, true) } else { os.unsetenv('VJOBS') }
		if value := previous_serial {
			os.setenv('V3_FASTC_NO_PARALLEL', value, true)
		} else {
			os.unsetenv('V3_FASTC_NO_PARALLEL')
		}
	}
	os.unsetenv('VJOBS')
	os.unsetenv('V3_FASTC_NO_PARALLEL')
	mut prefs := pref.new_preferences()
	cores := fastc_nr_cpus()
	assert fastc_parallel_worker_limit(prefs) == cores
	assert fastc_parallel_job_count(cores + 4, prefs) == cores
	os.setenv('VJOBS', '3', true)
	assert fastc_parallel_worker_limit(prefs) == 3
	prefs.no_parallel = true
	assert fastc_parallel_worker_limit(prefs) == 1
	prefs.no_parallel = false
	os.setenv('V3_FASTC_NO_PARALLEL', '1', true)
	assert fastc_parallel_worker_limit(prefs) == 1
}
