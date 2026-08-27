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
	build_names := ['first', 'second', 'no_rsp']
	build_flags := ['', '', '-no-rsp']
	for i, name in build_names {
		extra_flags := build_flags[i]
		output_path := os.join_path(test_dir, name)
		cmd := '${os.quoted_path(macos_reproducible_compiler_vexe)} -old-compiler -g -keepc ${extra_flags} -o ${os.quoted_path(output_path)} ${os.quoted_path(source_path)}'
		result := os.execute(cmd)
		assert result.exit_code == 0, result.output
		binary_hashes << sha256.hexhash(os.read_file(output_path)!)
	}
	spaced_vtmp := os.join_path(test_dir, 'temporary files with spaces')
	os.mkdir_all(spaced_vtmp)!
	old_vtmp := os.getenv_opt('VTMP')
	os.setenv('VTMP', spaced_vtmp, true)
	spaced_output := os.join_path(test_dir, 'spaced_vtmp')
	spaced_cmd := '${os.quoted_path(macos_reproducible_compiler_vexe)} -old-compiler -g -keepc -o ${os.quoted_path(spaced_output)} ${os.quoted_path(source_path)}'
	spaced_result := os.execute(spaced_cmd)
	if previous_vtmp := old_vtmp {
		os.setenv('VTMP', previous_vtmp, true)
	} else {
		os.unsetenv('VTMP')
	}
	assert spaced_result.exit_code == 0, spaced_result.output
	binary_hashes << sha256.hexhash(os.read_file(spaced_output)!)
	assert binary_hashes.all(it == binary_hashes[0]), binary_hashes.str()
	mut cdebug_hashes := []string{}
	for name in ['cdebug_first', 'cdebug_second'] {
		output_path := os.join_path(test_dir, name)
		cmd := '${os.quoted_path(macos_reproducible_compiler_vexe)} -old-compiler -cg -keepc -o ${os.quoted_path(output_path)} ${os.quoted_path(source_path)}'
		result := os.execute(cmd)
		assert result.exit_code == 0, result.output
		cdebug_hashes << sha256.hexhash(os.read_file(output_path)!)
	}
	assert cdebug_hashes[0] == cdebug_hashes[1], cdebug_hashes.str()

	first_output := os.join_path(test_dir, 'first')
	nm_result := os.execute('nm -ap ${os.quoted_path(first_output)}')
	assert nm_result.exit_code == 0, nm_result.output
	oso_lines := nm_result.output.split_into_lines().filter(it.contains(' OSO '))
	assert oso_lines.len > 0, nm_result.output
	debug_object := oso_lines[0].all_after(' OSO ').trim_space()
	assert os.is_file(debug_object), debug_object

	dsym_path := first_output + '.dSYM'
	assert os.is_dir(dsym_path)
	os.rmdir_all(dsym_path)!
	dsymutil_result :=
		os.execute('dsymutil -o ${os.quoted_path(dsym_path)} ${os.quoted_path(first_output)}')
	assert dsymutil_result.exit_code == 0, dsymutil_result.output
	assert os.is_dir(dsym_path)
}
