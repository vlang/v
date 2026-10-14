import os

fn test_windows_executable_suffix_does_not_rename_generated_c() {
	root := os.join_path(os.vtmp_dir(), 'windows_c_output_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'hello.v')
	os.write_file(source, 'fn main() { println("hello") }')!
	for backend in ['c'] {
		output := os.join_path(root, 'hello_${backend}')
		result := os.exec([@VEXE, '-no-retry-compilation', '-nocache', '-gc', 'none', '-os', 'windows',
			'-b', backend, '-cc', $if windows { 'cc' } $else { '/usr/bin/false' }, '-o', output,
			source])
		$if windows {
			assert result.exit_code == 0, result.output
			assert os.is_file(output + '.exe')
		} $else {
			// C is retained before a deliberately failing compiler, without a Windows SDK.
			assert result.exit_code != 0
		}
		assert os.is_file(output + '.c')
		assert !os.exists(output + '.exe.c')
	}
}
