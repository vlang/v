import os
import v.cmdexec

fn test_bootstrap_preserves_the_legacy_x64_skip_define() {
	root := os.join_path(os.vtmp_dir(), 'bootstrap_backend_skips_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	// Older source snapshots guard their SSA references with skip_x64.
	os.write_file(source, r'fn main() {
	$if !skip_x64 ? {
		$if skip_ssa ? {
			$compile_error("legacy x64 dispatch still references disabled SSA")
		}
	}
	$if skip_x64 ? {
		$if !skip_amd64 ? {
			$compile_error("AMD64 skip aliases disagree")
		}
	}
	$if skip_amd64 ? {
		$if !skip_x64 ? {
			$compile_error("AMD64 skip aliases disagree")
		}
	}
}
')!
	for flags in [
		[]string{},
		['-compile-backend', 'amd64'],
		['-compile-backend', 'x64'],
		['-all-backends'],
	] {
		result := cmdexec.run_in_merged_with_env(@VEXE, ['-nocache', '-check', ...flags, source], root, {
			'VFLAGS':                 ''
			'V_MACOS_V3_NO_FALLBACK': '1'
		})
		assert result.exit_code == 0, '${flags}: ${result.output}'
	}
}
