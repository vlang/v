module fastc

import os
import v.cmdexec

fn test_nested_guard_returns_and_call_result_map_reads_match_c() ! {
	root := os.join_path(os.vtmp_dir(), 'fastc_review_regressions_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	for backend in ['c', 'fastc'] {
		for i, fixture in [
			'options/or_block_nested_multi_guard_return_test.v',
			'options/call_result_map_optional_value_test.v',
			'builtin_maps/call_result_map_lookup_order_test.v',
		] {
			program := os.join_path(root, '${backend}_${i}')
			source := os.join_path(@VMODROOT, 'vlib', 'v', 'tests', fixture)
			build := cmdexec.run(@VEXE, ['-b', backend, '-enable-globals', '-o', program, source])
			assert build.exit_code == 0, '${backend}: ${fixture}\n${build.output}'
			// Explicitly run the executable: FastC's test-file command only builds it.
			run := cmdexec.run(program, [])
			assert run.exit_code == 0, '${backend}: ${fixture}\n${run.output}'
		}
	}
}
