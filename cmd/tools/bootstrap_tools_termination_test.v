import os
import v.cmdexec

fn test_self_hosted_bootstrap_tools_compile_and_terminate() {
	root := os.join_path(os.vtmp_dir(), 'bootstrap_tools_termination_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or { panic(err) } }
	vexe := os.join_path(@VMODROOT, 'v' + $if windows { '.exe' } $else { '' })
	for name in ['detect_tcc', 'vdoctor'] {
		source := os.join_path(@VMODROOT, 'cmd', 'tools', '${name}.v')
		output := os.join_path(root, name)
		// A bounded subprocess turns the unbounded compilation in #28390 into a failure.
		build := cmdexec.run_with_timeout(vexe, ['-new-compiler', '-no-retry-compilation', '-gc',
			'none', '-nocache', '-o', output, source], 120_000)
		assert build.exit_code == 0, build.output
		run := cmdexec.run_with_timeout(output, [], 30_000)
		assert run.exit_code == 0, run.output
		if name == 'vdoctor' {
			assert run.output.contains('V executable'), run.output
		}
	}
}
