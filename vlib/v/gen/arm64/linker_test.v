module arm64

import os

// test_getrusage_is_always_linked_from_the_system_library validates this v3 regression case.
fn test_getrusage_is_always_linked_from_the_system_library() {
	assert '_getrusage' in force_external_syms
	assert '_mach_task_self' in force_external_syms
	assert '_mach_task_self_' in force_external_syms
	assert '_proc_pid_rusage' in force_external_syms
}

// test_freestanding_runtime_definitions_win_over_forced_imports checks that a
// `-freestanding` object's own memcpy/exit bind locally, while a hosted object
// still forces them to libSystem and an undefined forced name stays external.
fn test_freestanding_runtime_definitions_win_over_forced_imports() {
	mut m := MachOObject.new()
	m.add_symbol('_memcpy', 0, true, 1)
	m.add_symbol('_exit', 4, true, 1)
	m.add_undefined('_malloc')
	mut hosted := Linker.new(m)
	hosted.collect_local_runtime_syms()
	assert hosted.is_forced_external('_memcpy')
	assert hosted.is_forced_external('_exit')
	mut freestanding := Linker.new(m)
	freestanding.freestanding = true
	freestanding.collect_local_runtime_syms()
	assert !freestanding.is_forced_external('_memcpy')
	assert !freestanding.is_forced_external('_exit')
	assert freestanding.is_forced_external('_malloc')
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
