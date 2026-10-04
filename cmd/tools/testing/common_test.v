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

fn test_noncompiling_test_sessions_use_cpu_jobs() {
	assert test_session_jobs(false, 18, u64(8) * 1024 * 1024 * 1024, 0) == 18
	assert test_session_jobs(false, 7, u64(8) * 1024 * 1024 * 1024, 7) == 7
	assert test_session_jobs(true, 18, u64(8) * 1024 * 1024 * 1024, 0) == 1
}

fn test_decode_mountinfo_path() {
	assert decode_mountinfo_path(r'/docker/my\040container') == '/docker/my container'
	assert decode_mountinfo_path(r'/docker/my\134container') == r'/docker/my\container'
}

fn test_cgroup_v2_memory_limit_uses_parent_limit() {
	mount_point := os.join_path(os.vtmp_dir(), 'testing_cgroup_memory_limit_${os.getpid()}')
	defer {
		os.rmdir_all(mount_point) or {}
	}
	os.mkdir_all(os.join_path(mount_point, 'container', 'work'))!
	os.write_file(os.join_path(mount_point, 'container', 'memory.max'), '8589934592')!
	os.write_file(os.join_path(mount_point, 'container', 'work', 'memory.max'), 'max')!
	cgroups := '0::/container/work'
	mountinfo := '36 25 0:32 / ${mount_point} rw,nosuid,nodev,noexec,relatime - cgroup2 cgroup rw'
	assert cgroup_memory_limit_from_contents(cgroups, mountinfo)! == u64(8) * 1024 * 1024 * 1024
}

fn test_cgroup_memory_limit_preserves_colons_in_paths() {
	$if windows {
		// Colons are reserved in Windows file names, so this Unix cgroup path cannot be created.
		return
	}
	mount_point := os.join_path(os.vtmp_dir(), 'testing_cgroup_memory_limit_${os.getpid()}')
	defer {
		os.rmdir_all(mount_point) or {}
	}
	os.mkdir_all(os.join_path(mount_point, 'container:team'))!
	os.write_file(os.join_path(mount_point, 'container:team', 'memory.max'), '8589934592')!
	cgroups := '0::/container:team'
	mountinfo := '36 25 0:32 / ${mount_point} rw,nosuid,nodev,noexec,relatime - cgroup2 cgroup rw'
	assert cgroup_memory_limit_from_contents(cgroups, mountinfo)! == u64(8) * 1024 * 1024 * 1024
}

fn test_cgroup_v1_memory_limit_is_used() {
	mount_point := os.join_path(os.vtmp_dir(), 'testing_cgroup_memory_limit_${os.getpid()}')
	defer {
		os.rmdir_all(mount_point) or {}
	}
	os.mkdir_all(os.join_path(mount_point, 'docker', 'container'))!
	os.write_file(os.join_path(mount_point, 'docker', 'container', 'memory.limit_in_bytes'), '8589934592')!
	cgroups := '5:memory:/docker/container'
	mountinfo := '29 23 0:26 / ${mount_point} rw,relatime - cgroup cgroup rw,memory'
	assert cgroup_memory_limit_from_contents(cgroups, mountinfo)! == u64(8) * 1024 * 1024 * 1024
}

fn test_cgroup_v1_memory_limit_is_preferred_on_hybrid_hosts() {
	test_root := os.join_path(os.vtmp_dir(), 'testing_cgroup_memory_limit_${os.getpid()}')
	v1_mount_point := os.join_path(test_root, 'v1')
	v2_mount_point := os.join_path(test_root, 'v2')
	defer {
		os.rmdir_all(test_root) or {}
	}
	os.mkdir_all(os.join_path(v1_mount_point, 'container'))!
	os.mkdir_all(os.join_path(v2_mount_point, 'container'))!
	os.write_file(os.join_path(v1_mount_point, 'container', 'memory.limit_in_bytes'), '8589934592')!
	cgroups := '0::/container\n5:memory:/container'
	v2_mount := '36 25 0:32 / ${v2_mount_point} rw,nosuid,nodev,noexec,relatime'
	v1_mount := '29 23 0:26 / ${v1_mount_point} rw,relatime'
	mountinfo := '${v2_mount} - cgroup2 cgroup rw\n${v1_mount} - cgroup cgroup rw,memory'
	assert cgroup_memory_limit_from_contents(cgroups, mountinfo)! == u64(8) * 1024 * 1024 * 1024
}

fn test_cgroup_namespace_relative_path_uses_non_root_mount() {
	mount_point := os.join_path(os.vtmp_dir(), 'testing_cgroup_memory_limit_${os.getpid()}')
	defer {
		os.rmdir_all(mount_point) or {}
	}
	os.mkdir_all(mount_point)!
	os.write_file(os.join_path(mount_point, 'memory.max'), '8589934592')!
	cgroups := '0::/'
	mountinfo := '36 25 0:32 /docker/container ${mount_point} rw,nosuid,nodev,noexec,relatime - cgroup2 cgroup rw'
	assert cgroup_memory_limit_from_contents(cgroups, mountinfo)! == u64(8) * 1024 * 1024 * 1024
}

fn test_cgroup_memory_limit_decodes_mountinfo_paths() {
	mount_point := os.join_path(os.vtmp_dir(), 'testing cgroup memory limit ${os.getpid()}')
	defer {
		os.rmdir_all(mount_point) or {}
	}
	os.mkdir_all(os.join_path(mount_point, 'work'))!
	os.write_file(os.join_path(mount_point, 'work', 'memory.max'), '8589934592')!
	cgroups := '0::/docker container/work'
	escaped_mount_root := r'/docker\040container'
	escaped_mount_point := mount_point.replace(' ', r'\040')
	mountinfo := '36 25 0:32 ${escaped_mount_root} ${escaped_mount_point} rw,nosuid,nodev,noexec,relatime - cgroup2 cgroup rw'
	assert cgroup_memory_limit_from_contents(cgroups, mountinfo)! == u64(8) * 1024 * 1024 * 1024
}

fn test_effective_test_memory_uses_lower_cgroup_limit() {
	physical_memory := u64(32) * 1024 * 1024 * 1024
	cgroup_memory_limit := u64(8) * 1024 * 1024 * 1024
	assert effective_test_memory(physical_memory, cgroup_memory_limit) == cgroup_memory_limit
	assert effective_test_memory(cgroup_memory_limit, physical_memory) == cgroup_memory_limit
}

fn test_os_suffix_selection_preserves_android_outside_termux() {
	for suffix in ['_test.v', '_test.c.v'] {
		file := 'fixture_android_outside_termux' + suffix
		assert test_file_is_target_of('android', file)
		for host in ['termux', 'linux', 'macos', 'windows'] {
			assert !test_file_is_target_of(host, file), 'host=${host}, file=${file}'
		}
	}
}

fn test_os_suffix_selection_preserves_simple_and_unspecified_targets() {
	for suffix in ['_test.v', '_test.c.v'] {
		assert test_file_is_target_of('termux', 'fixture_termux' + suffix)
		assert !test_file_is_target_of('android', 'fixture_termux' + suffix)
		assert test_file_is_target_of('windows', 'fixture_windows' + suffix)
		assert !test_file_is_target_of('linux', 'fixture_windows' + suffix)
		assert test_file_is_target_of('linux', 'fixture_nix' + suffix)
		assert !test_file_is_target_of('windows', 'fixture_nix' + suffix)
		for host in ['android', 'termux', 'linux', 'macos', 'windows'] {
			assert test_file_is_target_of(host, 'fixture' + suffix)
		}
	}
}

fn test_build_v_args_failed_accepts_literal_arguments() {
	assert !build_v_args_failed([@VEXE, 'version'])
	assert build_v_args_failed([]string{})
}

fn test_find_started_redis_on_default_port_accepts_an_unrewritten_process_title() {
	// `set-proc-title no` keeps the original command line, which does not show the port.
	lines := [
		'  PID TTY      STAT   TIME COMMAND',
		' 1234 ?        Ssl    0:10 redis-server /tmp/redis.conf',
	]
	assert find_started_redis_on_default_port(lines)! == lines[1]
}

fn test_find_started_redis_on_default_port_matches_the_whole_port() {
	other_port := ' 1234 ?        Ssl    0:10 redis-server *:63790'
	default_port := ' 5678 ?        Ssl    0:10 redis-server *:6379'
	if _ := find_started_redis_on_default_port([other_port]) {
		assert false, 'a redis-server on port 63790 does not serve the default port'
	}
	if _ := find_started_redis_on_default_port([]string{}) {
		assert false, 'no process is not a started redis-server'
	}
	assert find_started_redis_on_default_port([other_port, default_port])! == default_port
}
