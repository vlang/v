import os

fn test_freestanding_linux_posix_io_abi_and_errors() ! {
	$if !linux || (!amd64 && !arm64) {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'freestanding_posix_io_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(@VMODROOT, 'vlib/v/compiler_tests/testdata/freestanding_posix_io/main.c.v')
	for with_vlibc in [false, true] {
		output := os.join_path(root, if with_vlibc { 'vlibc' } else { 'plain' })
		mut command := [@VEXE, '-no-retry-compilation', '-b', 'c', '-cc', 'clang', '-freestanding',
			'-cstrict', '-o', output]
		if with_vlibc { command << ['-d', 'vlibc'] }
		command << source
		built := os.exec(command)
		assert built.exit_code == 0, built.output
		ran := os.exec([output])
		assert ran.exit_code == 0, ran.output
		assert ran.output.trim_space() == 'POSIX IO ok', ran.output
	}
}
