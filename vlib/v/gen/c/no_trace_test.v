module c

import os

fn test_no_trace_omits_entry_but_preserves_callees_and_profiling() ! {
	cc := os.find_abs_path_of_executable('clang') or { return }
	root := os.join_path(os.vtmp_dir(), 'no_trace_runtime_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	fixture := os.read_file(os.join_path(@VEXEROOT, 'vlib/v/gen/c/testdata/no_profile.vv'))!
	os.write_file(source, fixture.replace('no_profile', 'no_trace'))!
	for selected in ['main.*', 'main.ignored'] {
		report := os.join_path(root, 'profile.txt')
		ran := os.exec([@VEXE, '-cc', cc, '-gc', 'none', '-nocache', '-no-retry-compilation',
			'-trace-calls', '-trace-fns', selected, '-profile', report, 'run', source])
		assert ran.exit_code == 0, ran.output
		assert !ran.output.contains('main.ignored/'), ran.output
		assert !ran.output.contains('ignored_method/'), ran.output
		assert !ran.output.contains('ignored_generic'), ran.output
		if selected == 'main.*' {
			assert ran.output.contains('main.observed_child/0'), ran.output
			assert ran.output.contains('main.observed_after/0'), ran.output
		}
		contents := os.read_file(report)!
		assert contents.contains('main__ignored '), contents
		assert contents.contains('ignored_method'), contents
		assert contents.contains('ignored_generic'), contents
	}
	// Exclusion from profiling alone does not suppress call tracing.
	os.write_file(source, fixture)!
	ran := os.exec([@VEXE, '-cc', cc, '-gc', 'none', '-nocache', '-no-retry-compilation', '-trace-calls',
		'-trace-fns', 'main.*', 'run', source])
	assert ran.exit_code == 0, ran.output
	assert ran.output.contains('main.ignored/0'), ran.output
	assert ran.output.contains('ignored_method/1'), ran.output
	assert ran.output.contains('ignored_generic'), ran.output
}
