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
	os.write_file(os.join_path(mount_point, 'docker', 'container', 'memory.limit_in_bytes'),
		'8589934592')!
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
