import os

fn test_compatible_c_function_returns_follow_each_calling_module() {
	root := os.join_path(os.vtmp_dir(), 'c_fn_module_returns_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	os.cp_all(os.join_path(@DIR, 'testdata', 'c_fn_module_returns'), root, true)!
	source := os.join_path(root, 'main.v')
	original := os.read_file(source)!
	assert original.contains('import typed\nimport opaque')
	executable := os.join_path(root, $if windows { 'probe.exe' } $else { 'probe' })
	for reverse in [false, true] {
		os.write_file(source, if reverse {
			original.replace('import typed\nimport opaque', 'import opaque\nimport typed')
		} else {
			original
		})!
		for no_cache in [false, true] {
			mut args := [@VEXE, '-gc', 'none', '-cc', @CCOMPILER, '-o', executable]
			if no_cache { args << '-nocache' }
			args << root
			built := os.exec(args)
			assert built.exit_code == 0, built.output
			executed := os.exec([executable])
			assert executed.exit_code == 0, executed.output
			assert executed.output.trim_space() == 'ok', executed.output
		}
	}
}
