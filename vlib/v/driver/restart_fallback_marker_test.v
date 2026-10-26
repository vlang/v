module driver

import os

// A build that restarts itself as a child process hands its failures to the child.
// The marker it staged must go first, or its launcher replays the build at exit.
fn test_restart_releases_the_staged_fallback_marker() {
	old_value := os.getenv(macos_v3_fallback_file_env)
	was_set := os.getenv_opt(macos_v3_fallback_file_env) != none
	defer {
		if was_set {
			os.setenv(macos_v3_fallback_file_env, old_value, true)
		} else {
			os.unsetenv(macos_v3_fallback_file_env)
		}
	}
	marker := os.join_path(os.vtmp_dir(), 'v3_restart_marker_${os.getpid()}')
	defer {
		os.rm(marker) or {}
	}
	os.setenv(macos_v3_fallback_file_env, marker, true)
	assert stage_macos_v3_compiler_error_fallback(marker, 'source parsing')
	assert os.is_file(marker)
	release_v3_fallback_marker()
	assert !os.is_file(marker)
	// Without a marker, as when the fallback is disabled, there is nothing to remove.
	os.unsetenv(macos_v3_fallback_file_env)
	release_v3_fallback_marker()
}
