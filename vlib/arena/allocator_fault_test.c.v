import os

fn test_fallible_allocator_rolls_back_each_allocation_failure() {
	$if !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'arena-fault-${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(@VEXEROOT, 'vlib/arena/testdata/fallible_failures/main.c.v')
	for optimized in [false, true] {
		for tracked in [false, true] {
			binary := os.join_path(root, 'fault-${optimized}-${tracked}')
			mut command := [@VEXE, '-gc', 'none', '-cc', 'clang', '-no-retry-compilation']
			if optimized { command << '-prod' }
			if tracked { command << ['-d', 'track_heap'] }
			command << ['-o', binary, source]
			built := os.exec(command)
			assert built.exit_code == 0, built.output
			for scenario in ['heap', 'chunk', 'registry-first', 'registry-grow', 'existing-chunk',
				'existing-registry'] {
				result := os.exec([binary, scenario])
				// The heap case also verifies that infallible allocation still reports OOM.
				expected := if scenario == 'heap' { 'vcalloc(0x8)\n' } else { '' } +
					'fallible arena fault rollback passed\n'
				assert result.exit_code == 0 && result.output == expected, '${optimized}/${tracked}/${scenario}: ${result.output}'
			}
		}
	}
}
