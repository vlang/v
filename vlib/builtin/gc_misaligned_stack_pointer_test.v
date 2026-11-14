import os

// Regression test for issue #30995: a -prod build keeps some pointers in stack slots
// whose addresses are not a multiple of the pointer size, and the bundled libgc must
// still find them there. See testdata/gc_misaligned_stack_pointer.c.v.
fn test_production_bundled_gc_scans_misaligned_stack_slots() {
	$if macos || linux {
		test_dir := os.join_path(os.vtmp_dir(), 'gc_misaligned_stack_${os.getpid()}')
		os.mkdir_all(test_dir)!
		defer {
			os.rmdir_all(test_dir) or {}
		}
		program := os.join_path(test_dir, 'misaligned')
		fixture_dir := os.join_path(@VEXEROOT, 'vlib', 'builtin', 'testdata')
		fixture := os.join_path(fixture_dir, 'gc_misaligned_stack_pointer.c.v')
		// This nested -prod build must not depend on the test runner's shared cache.
		mut build_args := ['env', 'V3CACHE=${test_dir}', @VEXE, '-prod', '-cc', 'cc']
		build_args << ['-gc', 'boehm', '-d', 'use_bundled_libgc']
		build_args << ['-o', program, fixture]
		build := os.exec(build_args)
		assert build.exit_code == 0, build.output
		result := os.exec([program])
		assert result.exit_code == 0, result.output
	}
}
