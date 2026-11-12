import os

fn test_arm64_msvc_cpu_relax_mapping_is_target_specific() ! {
	root := os.join_path(os.vtmp_dir(), 'cpu_relax_flags_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	os.write_file(source, 'import sync.stdatomic
fn main() {
	mut value := u64(0)
	stdatomic.store_u64(&value, 1)
}
')!
	for target in [['arm64', 'msvc'], ['amd64', 'msvc'], ['arm64', 'clang']] {
		output := os.join_path(root, '${target[0]}_${target[1]}.c')
		// Omit platform builtin objects so the flag check needs no cross C toolchain.
		result := os.exec([@VEXE, '-no-builtin', '-nocache', '-gc', 'none', '-os', 'windows', '-arch',
			target[0], '-cc', target[1], '-dump-c-flags', '-', '-o', output, source])
		assert result.exit_code == 0, result.output
		expected := target[0] == 'arm64' && target[1] == 'msvc'
		assert result.output.contains('_mm_pause=__yield') == expected, result.output
	}
}
