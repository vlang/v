import os

fn test_msvc_rejects_non_windows_targets_before_c_compilation() {
	root := os.join_path(os.vtmp_dir(), 'msvc_target_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'hello.v')
	os.write_file(source, 'fn main() {}')!
	for target in ['linux', 'macos', 'freebsd'] {
		for compiler in ['msvc', 'cl', 'cl.exe'] {
			result := os.exec([@VEXE, '-no-retry-compilation', '-nocache', '-os', target, '-cc',
				compiler, '-o', os.join_path(root, 'hello.c'), source])
			assert result.exit_code != 0
			assert result.output.contains('MSVC requires a Windows target; cannot compile target `${target}`'), result.output
			assert !result.output.contains('Cannot open include file'), result.output
		}
	}
}
