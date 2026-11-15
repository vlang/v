import os

fn test_bundled_zstd_msan_declarations_compile() {
	compiler := os.find_abs_path_of_executable('clang') or {
		os.find_abs_path_of_executable('gcc') or { return }
	}
	source := os.join_path(@VEXEROOT, 'thirdparty', 'zstd', 'zstd.c')
	// Compile the MSan-only declarations even on hosts without its runtime.
	// The amalgamation must include stdint.h before declaring __msan_test_shadow.
	result := os.exec([compiler, '-std=c99', '-DZSTD_MEMORY_SANITIZER=1', '-fsyntax-only', source])
	assert result.exit_code == 0, result.output
}
