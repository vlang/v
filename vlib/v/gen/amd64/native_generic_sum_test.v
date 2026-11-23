module amd64

import os

fn test_native_generic_binary_search_tree() {
	$if linux && amd64 {
		output := os.join_path(os.vtmp_dir(), 'amd64_generic_tree_${os.getpid()}')
		defer { os.rm(output) or {} }
		source := os.join_path(@VEXEROOT, 'examples', 'binary_search_tree.v')
		for flags in [[]string{}, ['-prod'], ['-no-parallel']] {
			mut command := [@VEXE, '-b', 'amd64', '-o', output]
			command << flags
			command << source
			compiled := os.exec(command)
			assert compiled.exit_code == 0, compiled.output
			result := os.exec([output])
			assert result.exit_code == 0, result.output
			assert result.output == '[1] after insertion tree size is 11\n' +
				'[2] after deletion tree size is 7, and these elements were deleted: 0.0 0.3 0.6 1.0 \n', result.output
		}
	}
}
