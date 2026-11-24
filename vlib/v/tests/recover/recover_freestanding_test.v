import os

fn test_freestanding_panic_runs_defers_in_order_without_hosted_jump_symbols() ! {
	$if (!linux && !macos) || (!amd64 && !arm64) {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'recover_freestanding_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(@VMODROOT, 'vlib/v/tests/recover/testdata/freestanding/main.v')
	for production in [false, true] {
		exe := os.join_path(root, if production { 'production' } else { 'debug' })
		mut command := [@VEXE, '-nocache', '-cc', 'cc', '-freestanding', '-gc', 'none']
		if production { command << '-prod' }
		command << ['-o', exe, source]
		build := os.exec(command)
		assert build.exit_code == 0, build.output
		result := os.exec([exe])
		assert result.exit_code == 0, result.output
		symbols := os.exec(['nm', '-u', exe])
		assert symbols.exit_code == 0, symbols.output
		// On Mach-O, libc symbols have a leading underscore. Some ELF tools
		// also append a version to an imported symbol.
		assert !symbols.output.contains('setjmp'), symbols.output
		assert !symbols.output.contains('longjmp'), symbols.output
	}
}
