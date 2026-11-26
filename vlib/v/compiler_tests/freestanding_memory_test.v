import os

fn test_freestanding_memory_primitives_and_generated_aligned_storage() ! {
	$if !linux || (!amd64 && !arm64) {
		return
	}
	work := os.join_path(os.vtmp_dir(), 'freestanding_memory_${os.getpid()}')
	os.mkdir_all(work)!
	defer { os.rmdir_all(work) or {} }
	source := os.join_path(@VEXEROOT, 'vlib', 'builtin', 'testdata', 'freestanding_memory', 'main.c.v')
	for role in ['plain', 'vlibc', 'prod', 'c23'] {
		program := os.join_path(work, role)
		mut command := [@VEXE, '-no-retry-compilation', '-cc', 'clang', '-freestanding', '-o',
			program]
		if role in ['vlibc', 'prod'] { command << ['-d', 'vlibc'] }
		if role == 'prod' { command << '-prod' }
		// gnu2x is also accepted by Clang versions predating the gnu23 spelling.
		if role == 'c23' { command << ['-cflags', '-std=gnu2x'] }
		command << source
		built := os.exec(command)
		assert built.exit_code == 0, '${role}: ${built.output}'
		ran := os.exec([program])
		assert ran.exit_code == 0, '${role}: ${ran.output}'
		assert ran.output == 'freestanding memory passed\n', ran.output
	}
}

fn test_freestanding_allocation_failure_does_not_allocate_a_panic_message() ! {
	$if !linux || (!amd64 && !arm64) {
		return
	}
	work := os.join_path(os.vtmp_dir(), 'freestanding_oom_${os.getpid()}')
	os.mkdir_all(work)!
	defer { os.rmdir_all(work) or {} }
	source := os.join_path(work, 'oom.v')
	os.write_file(source, 'fn main() { unsafe { malloc(-1) } }')!
	program := os.join_path(work, 'oom')
	built := os.exec([@VEXE, '-no-retry-compilation', '-cc', 'clang', '-freestanding', '-o', program,
		source])
	assert built.exit_code == 0, built.output
	ran := os.exec([program])
	assert ran.exit_code == 1, ran.output
	assert ran.output.contains('memory allocation failure'), ran.output
	assert ran.output.count('memory allocation failure') == 1, ran.output
}
