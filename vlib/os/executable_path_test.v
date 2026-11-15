import os

fn test_executable_returns_complete_process_path() {
	executable := os.executable()
	assert executable.len > 0
	assert !executable.contains('\x00')
	assert os.is_file(executable)
	assert os.real_path(executable) == os.real_path(os.args[0])
}

fn test_executable_can_generate_c_for_hosted_targets() {
	root := os.join_path(os.vtmp_dir(), 'executable_hosted_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	os.write_file(source, 'import os\nfn main() { println(os.executable()) }\n')!
	for target in ['android', 'macos', 'cross'] {
		c_source := os.join_path(root, '${target}.c')
		// These frontend-only checks do not require a target SDK or C compiler.
		// Override an inherited MSVC selection when generating Unix or portable C.
		mut args := [@VEXE, '-new-compiler', '-no-retry-compilation', '-no-parallel', '-nocache',
			'-gc', 'none', '-cc', 'clang', '-os', target]
		if target != 'cross' {
			args << ['-arch', 'arm64']
		}
		args << ['-o', c_source, source]
		generated := os.exec(args)
		assert generated.exit_code == 0, '${target}: ${generated.output}'
		assert os.is_file(c_source), target
	}
}
