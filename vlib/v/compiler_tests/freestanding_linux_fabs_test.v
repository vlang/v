import os

fn test_freestanding_linux_fabs_c_abi_preserves_magnitude_bits() ! {
	$if !linux || (!amd64 && !arm64) {
		return
	}
	cc := os.find_abs_path_of_executable('clang') or { return }
	work := os.join_path(os.vtmp_dir(), 'freestanding_linux_fabs_${os.getpid()}')
	os.mkdir_all(work)!
	defer { os.rmdir_all(work) or {} }
	source := os.join_path(@VEXEROOT, 'vlib', 'v', 'compiler_tests', 'testdata', 'freestanding_fabs', 'main.c.v')
	for optimized in [false, true] {
		binary := os.join_path(work, 'probe')
		mut command := [@VEXE, '-no-retry-compilation', '-freestanding', '-gc', 'none', '-cc',
			cc, '-o', binary, source]
		if optimized { command << '-prod' }
		compiled := os.exec(command)
		assert compiled.exit_code == 0, compiled.output
		run := os.exec([binary])
		assert run.exit_code == 0, run.output
		assert run.output.trim_space() == 'freestanding Linux fabs C ABI passed'
	}
}
