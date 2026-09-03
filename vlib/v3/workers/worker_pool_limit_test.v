module workers

import os

fn test_pool_size_limit_is_process_local_and_only_lowers_future_pool_sizes() {
	$if !windows {
		legacy_env_name := 'V3_INTERNAL_POOL_SIZE_LIMIT'
		old_value := os.getenv_opt(legacy_env_name)
		defer {
			if value := old_value {
				os.setenv(legacy_env_name, value, true)
			} else {
				os.unsetenv(legacy_env_name)
			}
		}
		os.setenv(legacy_env_name, 'caller-value', true)
		limit_pool_size(2)
		limit_pool_size(4)
		assert os.getenv(legacy_env_name) == 'caller-value'
		mut pool := new(5)
		assert pool.stats().launch_attempts == 2
		pool.close()
	}
}
