// vtest build: linux || macos
import os

// The bundled `gc.c` amalgamation only has `PARALLEL_MARK` through the `-DPARALLEL_MARK=1`
// flag that `-d gc_parallel_mark` adds in builtin_d_gcboehm.c.v, on Linux and macOS.
fn test_production_bundled_gc_parallel_mark_opt_in() {
	test_dir := os.join_path(os.vtmp_dir(), 'gc_parallel_mark_${os.getpid()}')
	os.mkdir_all(test_dir)!
	defer {
		os.rmdir_all(test_dir) or {}
	}
	fixture := os.join_path(@VEXEROOT, 'vlib', 'builtin', 'testdata', 'gc_parallel_mark.c.v')
	for extra in [[]string{}, ['-d', 'gc_parallel_mark']] {
		program := os.join_path(test_dir, 'parallel_mark_${extra.len}')
		mut args := [@VEXE, '-prod', '-cc', 'cc', '-gc', 'boehm', '-d', 'use_bundled_libgc']
		args << extra
		args << ['-o', program, fixture]
		build := os.exec(args)
		assert build.exit_code == 0, build.output
		result := os.exec([program])
		assert result.exit_code == 0, result.output
	}
}
