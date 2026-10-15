import os
import v.cmdexec

fn test_propagation_defers_preserve_lexical_destructor_order() {
	root := os.join_path(os.vtmp_dir(), 'ownership_propagation_scope_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	os.cp(os.join_path(@DIR, 'testdata', 'ownership_propagation_scope.vv'), source)!
	expected := 'body 5\ndrop 5\nbody 1\ndrop 1\ntake 2\ndrop 2\nbody 3\ndrop 3\nresult-defer\nresult-defer\nresult-failed\nbody 4\ndrop 4\noption-defer\noption-defer\noption-failed\npanic-defer'
	for mode in ['-autofree', '-ownership'] {
		result := cmdexec.run_with_timeout(@VEXE, [mode, '-gc', 'none', '-no-parallel', '-o',
			os.join_path(root, mode.trim_left('-')), 'run', source], 180_000)
		assert result.exit_code == 0, '${mode}: ${result.output}'
		assert result.output.trim_space() == expected, '${mode}: ${result.output}'
	}
}
