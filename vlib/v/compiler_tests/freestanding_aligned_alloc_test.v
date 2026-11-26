import os

fn test_freestanding_aligned_alloc_abi() ! {
	$if !linux || !amd64 {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'bare_aligned_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	for prod in [false, true] {
		output := os.join_path(root, 'aligned_${prod}')
		mut command := [@VEXE, '-no-retry-compilation', '-freestanding', '-gc', 'none', '-cc',
			'clang', '-o', output]
		if prod { command << '-prod' }
		command << os.join_path(@VMODROOT, 'vlib/builtin/testdata/freestanding_aligned_alloc/main.c.v')
		built := os.exec(command)
		assert built.exit_code == 0, built.output
		ran := os.exec([output])
		assert ran.exit_code == 0 && ran.output.trim_space() == 'freestanding aligned_alloc ok', ran.output
	}
}
