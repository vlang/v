// vtest build: linux || macos
import os

const production_gc_dir = os.join_path(os.vtmp_dir(), 'gc_production_${os.getpid()}')
const production_gc_probe = os.join_path(production_gc_dir, 'gc_probe')

fn build_production_gc_probe(program string, extra_flags []string) {
	fixture := os.join_path(@VEXEROOT, 'vlib', 'builtin', 'testdata', 'gc_production.c.v')
	build := os.exec([@VEXE, '-prod', '-cc', 'cc', '-gc', 'boehm', '-d', 'use_bundled_libgc',
		...extra_flags, '-o', program, fixture])
	assert build.exit_code == 0, build.output
}

fn testsuite_begin() {
	// Each probe starts a fresh collector in its own process.
	os.mkdir_all(production_gc_dir) or { panic(err) }
	build_production_gc_probe(production_gc_probe, [])
}

fn testsuite_end() {
	os.rmdir_all(production_gc_dir) or {}
}

// The bundled `gc.c` amalgamation only has `HANDLE_FORK` through the `-DHANDLE_FORK=1` flag in
// builtin_d_gcboehm.c.v. Without it, a child forked from a program with other threads aborts in
// its first collection (`pthread_kill failed at suspend`).
fn test_forked_child_of_a_multi_threaded_program_can_collect() {
	for run in 0 .. 5 {
		result := os.exec([production_gc_probe, 'fork'])
		assert result.exit_code == 0, 'run ${run}: exit code ${result.exit_code}\n${result.output}'
		assert result.output.contains('child ok'), result.output
	}
}

fn test_production_bundled_gc_returns_free_pages() {
	result := os.exec([production_gc_probe, 'unmap'])
	assert result.exit_code == 0, result.output
}

// The bundled `gc.c` amalgamation only has `PARALLEL_MARK` through the `-DPARALLEL_MARK=1`
// flag that `-d gc_parallel_mark` adds in builtin_d_gcboehm.c.v, on Linux and macOS.
fn test_production_bundled_gc_parallel_mark_opt_in() {
	result := os.exec([production_gc_probe, 'parallel-mark'])
	assert result.exit_code == 0, result.output

	parallel_probe := os.join_path(production_gc_dir, 'gc_parallel_probe')
	build_production_gc_probe(parallel_probe, ['-d', 'gc_parallel_mark'])
	parallel_result := os.exec([parallel_probe, 'parallel-mark'])
	assert parallel_result.exit_code == 0, parallel_result.output
}

// HANDLE_FORK is only passed with GC threads. On macOS it would otherwise make a collector
// without threads refuse incremental mode, and print a `GC Warning` at startup.
fn test_incremental_mode_without_gc_threads() {
	$if !macos {
		return
	}
	test_dir := os.join_path(os.vtmp_dir(), 'gc_incremental_no_threads_${os.getpid()}')
	os.mkdir_all(test_dir)!
	defer {
		os.rmdir_all(test_dir) or {}
	}
	program := os.join_path(test_dir, 'incremental_no_threads')
	fixture := os.join_path(@VEXEROOT, 'vlib', 'builtin', 'testdata',
		'gc_incremental_no_threads.c.v')
	// `-prod` with cc compiles the bundled `gc.c`. (`-d use_bundled_libgc` without `-prod` does
	// not link with `-d no_gc_threads`: V's helpers still call GC_pthread_create.)
	build := os.exec([@VEXE, '-prod', '-cc', 'cc', '-gc', 'boehm_incr', '-d', 'no_gc_threads',
		'-o', program, fixture])
	assert build.exit_code == 0, build.output
	result := os.exec([program])
	assert result.exit_code == 0, result.output
	assert result.output.contains('incremental=1'), result.output
	assert !result.output.contains('GC Warning'), result.output
}
