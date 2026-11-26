import os

fn test_indexed_function_pointer_calls_in_debug_and_optimized_builds() ! {
	for production in [false, true] {
		mut command := [@VEXE, '-no-retry-compilation', '-gc', 'none', '-cc', 'clang']
		if production { command << '-prod' }
		command << os.join_path(@VEXEROOT, 'vlib', 'v', 'tests', 'fns', 'indexed_function_pointer_call_test.v')
		result := os.exec(command)
		assert result.exit_code == 0, result.output
	}
}
