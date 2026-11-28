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

fn test_native_generic_sum_returns_an_inline_fixed_array() {
	$if linux && amd64 {
		root := os.join_path(os.vtmp_dir(), 'amd64_sum_array_${os.getpid()}')
		os.mkdir_all(root)!
		defer { os.rmdir_all(root) or {} }
		source := os.join_path(root, 'main.v')
		output := os.join_path(root, 'program')
		os.write_file(source, 'type Value[T] = T | bool

fn wrap(a [3]i64) Value[[3]i64] { return a }

fn main() {
	mut a := [i64(1), 2, 3]!
	v := wrap(a)
	a[0] = 9
	if v is [3]i64 {
		assert v[0] == 1
		assert v[2] == 3
	} else {
		panic("expected fixed array")
	}
}
')!
		for flags in [[]string{}, ['-prod']] {
			built := os.exec([@VEXE, '-b', 'amd64', ...flags, '-o', output, source])
			assert built.exit_code == 0, built.output
			run := os.exec([output])
			assert run.exit_code == 0, run.output
		}
	}
}
