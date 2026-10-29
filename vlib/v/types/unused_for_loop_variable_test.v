module types

import os

fn test_unused_for_loop_variables_in_check_modes() {
	fixture := os.join_path(@VEXEROOT, 'vlib/v/checker/tests/unused_for_loop_variable.vv')
	root := os.join_path(os.vtmp_dir(), 'unused_for_loop_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	os.write_file(source, os.read_file(fixture)!)!
	unused := ['j', 'k', 'array_value', 'array_index', 'unused_value', 'map_key', 'map_value',
		'iterator_value', 'reused']
	for flags in [[]string{}, ['-new-compiler'], ['-no-parallel', '-nocache']] {
		for warnings_as_errors in [false, true] {
			mut args := [@VEXE, ...flags, '-check']
			if warnings_as_errors {
				args << '-W'
			}
			args << source
			result := os.exec(args)
			assert result.exit_code == if warnings_as_errors { 1 } else { 0 }, result.output
			assert result.output.count('unused variable:') == unused.len, result.output
			for name in unused {
				assert result.output.count('unused variable: `${name}`') == 1, result.output
			}
		}
	}
}
