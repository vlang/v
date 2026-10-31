import os

fn test_production_bundled_gc_returns_free_pages() {
	$if macos || linux {
		test_dir := os.join_path(os.vtmp_dir(), 'gc_prod_unmap_${os.getpid()}')
		os.mkdir_all(test_dir)!
		defer {
			os.rmdir_all(test_dir) or {}
		}
		program := os.join_path(test_dir, 'unmap')
		fixture := os.join_path(@VEXEROOT, 'vlib', 'builtin', 'testdata', 'gc_prod_unmap.c.v')
		build := os.exec([@VEXE, '-prod', '-cc', 'cc', '-gc', 'boehm', '-d', 'use_bundled_libgc',
			'-o', program, fixture])
		assert build.exit_code == 0, build.output
		result := os.exec([program])
		assert result.exit_code == 0, result.output
	}
}
