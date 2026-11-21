import os

fn test_windows_executable_suffix_does_not_rename_generated_c() {
	root := os.join_path(os.vtmp_dir(), 'windows_c_output_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'hello.v')
	os.write_file(source, 'fn main() { println("hello") }')!
	// On macOS a failed V3 build is retried with the compatibility compiler, which can
	// cross compile with an installed MinGW; this test is about V3's own output.
	old_no_fallback := os.getenv_opt('V_MACOS_V3_NO_FALLBACK')
	os.setenv('V_MACOS_V3_NO_FALLBACK', '1', true)
	defer {
		if value := old_no_fallback {
			os.setenv('V_MACOS_V3_NO_FALLBACK', value, true)
		} else {
			os.unsetenv('V_MACOS_V3_NO_FALLBACK')
		}
	}
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
