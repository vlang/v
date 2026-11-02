import os
import rand

fn test_fifo_accepts_darwin_mkfifo_binding() {
	root := os.join_path(os.vtmp_dir(), 'fifo_mkfifo_abi_' + rand.ulid())
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.c.v')
	// Darwin's mode_t is u16; importing os must preserve a matching C binding.
	os.write_file(source, 'import os\nfn C.mkfifo(path &char, mode u16) i32\nfn main() { _ := os.args }\n')!
	for target in ['macos', 'ios'] {
		result := os.exec([@VEXE, '-new-compiler', '-no-retry-compilation', '-no-parallel', '-os',
			target, '-check', source])
		assert result.exit_code == 0, '${target}: ${result.output}'
	}
}
