import os
import v.cmdexec

fn review_findings_command(args []string) {
	timeout := if args.len == 1 { 15_000 } else { 180_000 }
	result := cmdexec.run_with_timeout(args[0], args[1..], timeout)
	assert result.exit_code == 0, '${args}: ${result.output}'
}

fn test_freestanding_native_review_regressions() ! {
	$if !linux || (!amd64 && !arm64) {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'freestanding_review_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	fixture := os.join_path(@VEXEROOT, 'vlib/builtin/testdata/freestanding_compat')
	// Compile native callers independently of V's C flags and compare against libc.
	object := os.join_path(root, 'probe.o')
	review_findings_command(['gcc', '-O2', '-fno-builtin', '-c', os.join_path(fixture,
		'review_findings_probe.c'), '-o', object])
	host := os.join_path(root, 'host')
	review_findings_command(['gcc', '-O2', '-fno-builtin', '-DHOSTED_REVIEW',
		os.join_path(fixture, 'review_findings_probe.c'), '-pthread', '-lm', '-o', host])
	review_findings_command([host])
	source := os.join_path(root, 'main.c.v')
	os.write_file(source, 'module main\n#flag ${object}\nfn C.v_review_findings_probe() int\nfn main() { exit(C.v_review_findings_probe()) }\n')!
	for compiler in ['gcc', 'clang'] {
		for production in [false, true] {
			binary := os.join_path(root, '${compiler}_${production}')
			mut args := [@VEXE, '-no-retry-compilation', '-gc', 'none', '-syscalls', '-shim-libc',
				'-cc', compiler, '-ldflags', '-static', '-o', binary]
			if production { args << '-prod' }
			args << source
			review_findings_command(args)
			review_findings_command([binary])
		}
	}
	cpp_object := os.join_path(root, 'once.o')
	review_findings_command(['g++', '-O2', '-c', os.join_path(fixture, 'review_once.cpp'), '-o',
		cpp_object])
	review_findings_command(['g++', '-O2', '-DHOSTED_REVIEW', os.join_path(fixture,
		'review_once.cpp'), '-pthread', '-o', host])
	review_findings_command([host])
	os.write_file(source, 'module main\n#flag ${cpp_object}\n#flag -lstdc++\n#flag -lgcc_eh\nfn C.v_review_once_probe() int\nfn main() { exit(C.v_review_once_probe()) }\n')!
	for production in [false, true] {
		binary := os.join_path(root, 'once_${production}')
		mut args := [@VEXE, '-no-retry-compilation', '-gc', 'none', '-syscalls', '-shim-libc',
			'-cc', 'gcc', '-ldflags', '-static', '-o', binary]
		if production { args << '-prod' }
		args << source
		review_findings_command(args)
		review_findings_command([binary])
	}
}
