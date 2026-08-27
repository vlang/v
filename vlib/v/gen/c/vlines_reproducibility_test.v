import os

const vlines_reproducibility_vexe = @VEXE

fn test_debug_generated_c_is_reproducible_across_output_paths() {
	test_dir := os.join_path(os.vtmp_dir(), 'vlines_reproducibility_${os.getpid()}')
	os.rmdir_all(test_dir) or {}
	os.mkdir_all(test_dir)!
	defer {
		os.rmdir_all(test_dir) or {}
	}
	source_path := os.join_path(test_dir, 'main.v')
	os.write_file(source_path, 'fn main() {\n\tprintln(42)\n}\n')!
	mut generated := []string{}
	for name in ['first.c', 'second.c'] {
		output_path := os.join_path(test_dir, name)
		cmd := '${os.quoted_path(vlines_reproducibility_vexe)} -old-compiler -g -o ${os.quoted_path(output_path)} ${os.quoted_path(source_path)}'
		result := os.execute(cmd)
		assert result.exit_code == 0, result.output
		generated << os.read_file(output_path)!
	}
	assert generated[0] == generated[1]
	assert generated[0].contains('#line ')
	assert generated[0].contains('"<generated C>"')
}
