// vtest build: linux || macos
import os

// The bundled `gc.c` amalgamation only has `HANDLE_FORK` through the `-DHANDLE_FORK=1` flag in
// builtin_d_gcboehm.c.v. Without it, a child forked from a program with other threads aborts in
// its first collection (`pthread_kill failed at suspend`).
fn test_forked_child_of_a_multi_threaded_program_can_collect() {
	test_dir := os.join_path(os.vtmp_dir(), 'gc_handle_fork_${os.getpid()}')
	os.mkdir_all(test_dir)!
	defer {
		os.rmdir_all(test_dir) or {}
	}
	program := os.join_path(test_dir, 'handle_fork')
	fixture := os.join_path(@VEXEROOT, 'vlib', 'builtin', 'testdata', 'gc_handle_fork.c.v')
	build := os.exec([@VEXE, '-prod', '-cc', 'cc', '-gc', 'boehm', '-d', 'use_bundled_libgc', '-o',
		program, fixture])
	assert build.exit_code == 0, build.output
	for run in 0 .. 5 {
		result := os.exec([program])
		assert result.exit_code == 0, 'run ${run}: exit code ${result.exit_code}\n${result.output}'
		assert result.output.contains('child ok'), result.output
	}
}
