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
		fixture := os.join_path(@VEXEROOT, 'vlib', 'builtin', 'testdata',
			'gc_misaligned_stack_pointer.c.v')
		build := os.exec([@VEXE, '-prod', '-cc', 'cc', '-gc', 'boehm', '-d', 'use_bundled_libgc',
			'-o', program, fixture])
		assert build.exit_code == 0, build.output
		result := os.exec([program])
		assert result.exit_code == 0, result.output
	}
}
