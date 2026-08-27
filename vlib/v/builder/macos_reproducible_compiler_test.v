module builder

import crypto.sha256
import os

const macos_reproducible_compiler_vexe = @VEXE

fn test_macos_debug_compiler_build_is_reproducible() {
	$if !macos {
		return
	}
	test_dir := os.join_path(os.vtmp_dir(), 'macos_reproducible_compiler_${os.getpid()}')
	compiler_dir := os.join_path(test_dir, 'cmd', 'v')
	os.rmdir_all(test_dir) or {}
	os.mkdir_all(compiler_dir)!
	defer {
		os.rmdir_all(test_dir) or {}
	}
	source_path := os.join_path(compiler_dir, 'v.v')
	os.write_file(source_path, 'fn main() {\n\tprintln(42)\n}\n')!
	mut binary_hashes := []string{}
	for name in ['first', 'second'] {
		output_path := os.join_path(test_dir, name)
		cmd := '${os.quoted_path(macos_reproducible_compiler_vexe)} -old-compiler -g -keepc -o ${os.quoted_path(output_path)} ${os.quoted_path(source_path)}'
		result := os.execute(cmd)
		assert result.exit_code == 0, result.output
		binary_hashes << sha256.hexhash(os.read_file(output_path)!)
	}
	assert binary_hashes[0] == binary_hashes[1], binary_hashes.str()
}
