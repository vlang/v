import os

fn test_freestanding_process_syscalls_and_child_lifetimes() ! {
	$if !((linux && (amd64 || arm64)) || (macos && amd64)) {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'freestanding_process_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(@VEXEROOT, 'vlib/os/testdata/freestanding_process/main.v')
	for optimized in [false, true] {
		for mode in ['hosted', 'syscalls', 'syscalls_autofree', 'bare', 'autofree'] {
			binary := os.join_path(root, '${mode}_${optimized}')
			mut command := [@VEXE, '-no-retry-compilation', '-gc', 'none', '-cc', 'clang']
			if optimized { command << '-prod' }
			if mode in ['bare', 'autofree'] { command << '-freestanding' }
			if mode in ['autofree', 'syscalls_autofree'] { command << '-autofree' }
			if mode in ['syscalls', 'syscalls_autofree'] { command << '-syscalls' }
			if mode in ['hosted', 'syscalls', 'syscalls_autofree'] {
				command << ['-d', 'process_test_hooks', '-cflags', '-DPROCESS_TEST_HOOKS']
				$if linux {
					if mode != 'hosted' {
						command << ['-d', 'process_track_alloc', '-cflags', '-DPROCESS_TRACK_ALLOC',
							'-ldflags', '-Wl,--wrap=malloc,--wrap=calloc,--wrap=realloc,--wrap=free']
					}
				}
			}
			command << ['-o', binary, source]
			built := os.exec(command)
			assert built.exit_code == 0, '${mode}/${optimized}: ${built.output}'
			ran := os.exec([binary])
			assert ran.exit_code == 0, '${mode}/${optimized}: ${ran.output}'
			assert ran.output.trim_space() == 'freestanding Process passed', ran.output
		}
		// Instrumentation must not add a runtime callback anywhere between fork
		// and exec, including the string tests and syscall leaf wrappers.
		binary := os.join_path(root, 'instrumented_${optimized}')
		mut instrumented := [@VEXE, '-no-retry-compilation', '-gc', 'none', '-cc', 'clang', '-syscalls',
			'-trace-calls', '-profile', os.join_path(root, 'profile_${optimized}.txt')]
		if optimized { instrumented << '-prod' }
		$if linux {
			instrumented << ['-d', 'process_track_alloc', '-cflags', '-DPROCESS_TRACK_ALLOC', '-ldflags',
				'-Wl,--wrap=malloc,--wrap=calloc,--wrap=realloc,--wrap=free']
		}
		instrumented << ['-o', binary,
			os.join_path(@VEXEROOT, 'vlib/os/testdata/freestanding_process/instrumented.c.v')]
		built := os.exec(instrumented)
		assert built.exit_code == 0, built.output
		ran := os.exec([binary])
		assert ran.exit_code == 0 && ran.output.contains('instrumented Process passed'), ran.output
	}
}
