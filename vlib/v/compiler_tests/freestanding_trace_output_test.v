import os

fn test_raw_trace_hooks_do_not_enter_allocator_or_free_stack_views() ! {
	$if !linux || !amd64 {
		return
	}
	clang := os.find_abs_path_of_executable('clang') or { return }
	work := os.join_path(os.vtmp_dir(), 'raw_trace_no_alloc_${os.getpid()}')
	os.mkdir_all(work)!
	defer { os.rmdir_all(work) or {} }
	// Reuse the target V implementation with only target selection and its
	// output sink replaced. The independent C sink rejects allocator reentry.
	for name in ['tracing_calls.c.v', 'tracing_calls_nix.c.v', 'tracing_calls_raw_d_freestanding.v'] {
		original := os.read_file(os.join_path(@VEXEROOT, 'vlib/v/trace_calls', name))!
		policy := original.replace_once('module trace_calls', 'module main')
			.replace('$if freestanding && macos && amd64 && !native {', '$if amd64 {')
			.replace('$if !(freestanding && macos && amd64 && !native) {', '$if !amd64 {')
			.replace('$if macos && amd64 && !native {', '$if amd64 {')
			.replace('eprint(', 'test_eprint(').replace('eprintln(', 'test_eprintln(')
		os.write_file(os.join_path(work, name.replace('_d_freestanding', '')), policy)!
	}
	fixture := os.join_path(@VEXEROOT, 'vlib/v/compiler_tests/testdata/raw_trace')
	for name in ['main.c.v', 'guard.c', 'guard.h'] {
		os.cp(os.join_path(fixture, name), os.join_path(work, name))!
	}
	for role in ['debug', 'prod', 'autofree'] {
		binary := os.join_path(work, 'trace')
		mut command := [@VEXE, '-no-retry-compilation', '-gc', 'none', '-cc', clang, '-ldflags',
			'-Wl,--wrap=malloc,--wrap=calloc,--wrap=realloc,--wrap=free', '-o', binary]
		if role == 'prod' { command << '-prod' }
		if role == 'autofree' { command << '-autofree' }
		command << work
		built := os.exec(command)
		assert built.exit_code == 0, '${role}: ${built.output}'
		ran := os.exec([binary])
		assert ran.exit_code == 0, '${role}: ${ran.output}'
		assert ran.output.trim_space() == 'allocation-free raw tracing passed', ran.output
	}
}
