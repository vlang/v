import os

fn test_freestanding_flush_preserves_output_without_diagnostics() ! {
	$if !linux && !macos {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'freestanding_flush_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'flush.v')
	os.write_file(source, 'fn main() {
 println("ready")
 flush_stdout()
 flush_stderr()
}')!
	for with_vlibc in [false, true] {
		output := os.join_path(root, if with_vlibc { 'vlibc' } else { 'plain' })
		mut command := [@VEXE, '-no-retry-compilation', '-b', 'c', '-cc', 'clang', '-freestanding',
			'-o', output]
		if with_vlibc { command << ['-d', 'vlibc'] }
		command << source
		built := os.exec(command)
		assert built.exit_code == 0, built.output
		ran := os.exec([output])
		assert ran.exit_code == 0, ran.output
		assert ran.output == 'ready\n', ran.output
	}
}
