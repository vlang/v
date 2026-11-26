import os

fn test_exec_argument_storage_outlives_subprocess_start() ! {
	$if !linux && !macos {
		return
	}
	$if freestanding || syscalls {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'exec argv lifetime ${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(@VEXEROOT, 'vlib/os/testdata/exec_argv_lifetime/main.v')
	for optimized in [false, true] {
		for autofree in [false, true] {
			binary := os.join_path(root, 'argv_${optimized}_${autofree}')
			mut command := [@VEXE, '-no-retry-compilation', '-gc', 'none', '-cc', 'clang']
			if optimized { command << '-prod' }
			if autofree { command << '-autofree' }
			command << ['-o', binary, source]
			built := os.exec(command)
			assert built.exit_code == 0, built.output
			ran := os.exec([binary])
			assert ran.exit_code == 0 && ran.output == 'exec argument lifetime passed\n', '${optimized}/${autofree}: ${ran.output}'
		}
	}
}
