module arm64

import os

// test_getrusage_is_always_linked_from_the_system_library validates this v3 regression case.
fn test_getrusage_is_always_linked_from_the_system_library() {
	assert '_getrusage' in force_external_syms
	assert '_mach_task_self' in force_external_syms
	assert '_mach_task_self_' in force_external_syms
	assert '_proc_pid_rusage' in force_external_syms
}

fn test_native_output_replaces_an_existing_file() {
	root := os.join_path(os.vtmp_dir(), 'arm64-replace-output-${os.getpid()}')
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	temporary := os.join_path(root, 'temporary')
	output := os.join_path(root, 'output')
	os.write_file(temporary, 'new output')!
	os.write_file(output, 'previous output')!
	assert replace_output_file(temporary, output)
	assert os.read_file(output)! == 'new output'
}

fn test_failed_posix_rename_preserves_the_existing_output() {
	$if windows {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'arm64-failed-rename-${os.getpid()}')
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	missing := os.join_path(root, 'missing')
	output := os.join_path(root, 'output')
	os.write_file(output, 'previous output')!
	assert !replace_output_file(missing, output)
	assert os.read_file(output)! == 'previous output'
}
