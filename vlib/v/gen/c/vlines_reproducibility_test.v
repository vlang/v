import os
import v.util

const vlines_reproducibility_vexe = @VEXE

fn test_debug_generated_c_keeps_real_output_path() {
	test_dir := os.join_path(os.vtmp_dir(), 'vlines_reproducibility_${os.getpid()}')
	os.rmdir_all(test_dir) or {}
	os.mkdir_all(test_dir)!
	defer {
		os.rmdir_all(test_dir) or {}
	}
	source_path := os.join_path(test_dir, 'main.v')
	os.write_file(source_path, 'fn main() {\n\tprintln(42)\n}\n')!
	output_name := 'vlines_${os.getpid()}_ordinary.exe'
	output_path := os.join_path(test_dir, output_name)
	generated_c_path := os.join_path(os.vtmp_dir(), '${output_name}.tmp.c')
	expected_reset_path := util.vlines_escape_path(generated_c_path, '')
	defer {
		os.rm(generated_c_path) or {}
		os.rm(generated_c_path + '.rsp') or {}
	}
	cmd := '${os.quoted_path(vlines_reproducibility_vexe)} -old-compiler -g -keepc -o ${os.quoted_path(output_path)} ${os.quoted_path(source_path)}'
	result := os.execute(cmd)
	assert result.exit_code == 0, result.output
	generated := os.read_file(generated_c_path)!
	debug_lines := generated.split_into_lines().filter(it.starts_with('#line '))
	assert generated.contains('"${expected_reset_path}"'), debug_lines.last()
	assert !generated.contains('"<generated C>"')
}

fn test_macos_debug_compiler_generated_c_is_reproducible_across_output_paths() {
	$if !macos {
		return
	}
	test_dir := os.join_path(os.vtmp_dir(), 'vlines_compiler_reproducibility_${os.getpid()}')
	compiler_dir := os.join_path(test_dir, 'cmd', 'v')
	os.rmdir_all(test_dir) or {}
	os.mkdir_all(compiler_dir)!
	defer {
		os.rmdir_all(test_dir) or {}
	}
	source_path := os.join_path(compiler_dir, 'v.v')
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
