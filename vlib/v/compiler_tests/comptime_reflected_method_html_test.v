import os

fn test_reflected_method_can_call_html_parser() {
	root := os.join_path(os.vtmp_dir(), 'reflected_method_html_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	os.cp(@VEXEROOT + '/vlib/v/compiler_tests/testdata/reflected_method_html.vv', source)!
	for mode in ['parallel', 'serial', 'cached'] {
		output := os.join_path(root, 'app' + $if windows { '.exe' } $else { '' })
		mut args := [@VEXE, '-new-compiler', '-o', output]
		if mode != 'cached' {
			args << '-nocache'
		}
		if mode == 'serial' {
			args << '-no-parallel'
		}
		args << source
		compiled := os.exec(args)
		assert compiled.exit_code == 0, compiled.output
		assert !compiled.output.contains('unsupported node kind'), compiled.output
		run := os.exec([output])
		assert run.exit_code == 0, run.output
	}
}
