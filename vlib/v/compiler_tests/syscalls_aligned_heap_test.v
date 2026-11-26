import os
import v.cmdexec

fn test_syscalls_libc_alignment_payload_reallocation_and_bounded_live_heap() ! {
	$if !linux || (!amd64 && !arm64) {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'syscalls_aligned_heap_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	for compiler in ['gcc', 'clang'] {
		os.find_abs_path_of_executable(compiler) or { continue }
		for production in [false, true] {
			output := os.join_path(root, '${compiler}_${production}')
			mut command := ['-syscalls', '-cc', compiler, '-o', output]
			if production { command << '-prod' }
			command << os.join_path(@VEXEROOT, 'vlib/builtin/testdata/syscalls_aligned_heap/main.c.v')
			built := cmdexec.run(@VEXE, command)
			assert built.exit_code == 0, built.output
			result := cmdexec.run(output, [])
			assert result.exit_code == 0 && result.output == 'syscalls aligned heap ok\n', '${compiler}/${production}: ${result.exit_code} ${result.output}'
		}
	}
}

fn test_explicit_boehm_reclaims_allocations_in_syscall_workers() ! {
	$if !linux || (!amd64 && !arm64) {
		return
	}
	os.find_abs_path_of_executable('gcc') or { return }
	root := os.join_path(os.vtmp_dir(), 'syscalls_boehm_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	output := os.join_path(root, 'worker')
	fixture := os.join_path(@VEXEROOT, 'vlib/builtin/testdata/syscalls_boehm/main.c.v')
	for flags in [['-syscalls'], ['-prod', '-bare', '-shim-libc'],
		['-bare', '-shim-libc', '-ldflags', '-static']] {
		mut args := ['-cc', 'gcc', '-gc', 'boehm', '-o', output]
		args << flags
		args << fixture
		build := cmdexec.run(@VEXE, args)
		assert build.exit_code == 0, build.output
		deps := cmdexec.run('readelf', ['-d', output])
		assert deps.exit_code == 0, deps.output
		assert deps.output.contains('[libc.') == ('-shim-libc' !in flags), deps.output
		result := cmdexec.run(output, [])
		assert result.exit_code == 0, '${flags}: ${result.exit_code} ${result.output}'
		assert result.output == 'bare Boehm collected worker buffers\n', result.output
	}
}
