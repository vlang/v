import os

const vexe = @VEXE
const vroot = os.dir(vexe)

fn assert_vself_preserves_full_cli(output string) {
	assert !output.contains('-selfhost'), output
	assert !output.contains('vlib/v3/v3.v'), output
	assert output.contains('cmd/v'), output
}

fn test_linux_tinyc_self_build_does_not_enable_prealloc() {
	$if !linux {
		return
	}
	noop := os.find_abs_path_of_executable('echo') or { return }
	tool := os.join_path(os.vtmp_dir(), 'vself_prealloc_test')
	defer {
		os.rm(tool) or {}
	}
	build := os.execute('${os.quoted_path(vexe)} -o ${os.quoted_path(tool)} ${os.quoted_path(os.join_path(vroot, 'cmd', 'tools', 'vself.v'))}')
	assert build.exit_code == 0, build.output
	for compiler in ['tcc', 'tinyc'] {
		result :=
			os.execute('VEXE=${os.quoted_path(noop)} ${os.quoted_path(tool)} self -cc ${compiler} -o /tmp/vself_${compiler}_test')
		assert result.exit_code == 0, result.output
		assert !result.output.contains('-new-compiler'), result.output
		assert !result.output.contains('-b fastc'), result.output
		assert !result.output.contains('-prealloc'), result.output
		assert_vself_preserves_full_cli(result.output)
	}
	for compiler in ['tcc', 'tinyc'] {
		result :=
			os.execute('VEXE=${os.quoted_path(noop)} VFLAGS="-cc ${compiler} -no-retry-compilation" ${os.quoted_path(tool)} self -o /tmp/vself_vflags_${compiler}_test')
		assert result.exit_code == 0, result.output
		assert !result.output.contains('-new-compiler'), result.output
		assert !result.output.contains('-b fastc'), result.output
		assert !result.output.contains('-prealloc'), result.output
		assert !result.output.contains('-cc'), result.output
		assert_vself_preserves_full_cli(result.output)
	}
	clang_result :=
		os.execute('VEXE=${os.quoted_path(noop)} ${os.quoted_path(tool)} self -cc clang -o /tmp/vself_clang_test')
	assert clang_result.exit_code == 0, clang_result.output
	assert !clang_result.output.contains('-new-compiler'), clang_result.output
	assert !clang_result.output.contains('-b fastc'), clang_result.output
	assert clang_result.output.contains('-prealloc'), clang_result.output
	assert_vself_preserves_full_cli(clang_result.output)
	clang_override_result :=
		os.execute('VEXE=${os.quoted_path(noop)} VFLAGS="-cc tcc" ${os.quoted_path(tool)} self -cc clang -o /tmp/vself_vflags_clang_test')
	assert clang_override_result.exit_code == 0, clang_override_result.output
	assert !clang_override_result.output.contains('-new-compiler'), clang_override_result.output
	assert !clang_override_result.output.contains('-b fastc'), clang_override_result.output
	assert clang_override_result.output.contains('-prealloc'), clang_override_result.output
	assert_vself_preserves_full_cli(clang_override_result.output)
	old_result :=
		os.execute('VEXE=${os.quoted_path(noop)} ${os.quoted_path(tool)} self -old-compiler -o /tmp/vself_old_test')
	assert old_result.exit_code == 0, old_result.output
	assert !old_result.output.contains('-new-compiler'), old_result.output
	assert !old_result.output.contains('-b fastc'), old_result.output
	assert_vself_preserves_full_cli(old_result.output)
}

fn test_linux_default_self_build_preserves_full_cli() {
	$if !linux {
		return
	}
	noop := os.find_abs_path_of_executable('echo') or { return }
	tool := os.join_path(os.vtmp_dir(), 'vself_v3_c_backend_test')
	defer {
		os.rm(tool) or {}
	}
	build := os.execute('${os.quoted_path(vexe)} -o ${os.quoted_path(tool)} ${os.quoted_path(os.join_path(vroot, 'cmd', 'tools', 'vself.v'))}')
	assert build.exit_code == 0, build.output
	result :=
		os.execute('env -u CC VEXE=${os.quoted_path(noop)} ${os.quoted_path(tool)} self -o /tmp/vself_v3_c_backend_test')
	assert result.exit_code == 0, result.output
	assert !result.output.contains('-b fastc'), result.output
	assert result.output.contains('-prealloc'), result.output
	assert_vself_preserves_full_cli(result.output)
}

fn test_macos_default_self_build_compiler_selection() {
	$if !macos {
		return
	}
	noop := os.find_abs_path_of_executable('echo') or { return }
	tool := os.join_path(os.vtmp_dir(), 'vself_macos_prealloc_test')
	defer {
		os.rm(tool) or {}
	}
	build := os.execute('${os.quoted_path(vexe)} -o ${os.quoted_path(tool)} ${os.quoted_path(os.join_path(vroot, 'cmd', 'tools', 'vself.v'))}')
	assert build.exit_code == 0, build.output
	default_result :=
		os.execute('env -u CC VEXE=${os.quoted_path(noop)} ${os.quoted_path(tool)} self -o /tmp/vself_macos_prealloc_test')
	assert default_result.exit_code == 0, default_result.output
	default_cc := if os.uname().machine in ['arm64', 'aarch64'] { 'tcc' } else { 'cc' }
	assert !default_result.output.contains('-b fastc'), default_result.output
	assert default_result.output.contains('-cc ${default_cc}'), default_result.output
	assert default_result.output.contains('-prealloc'), default_result.output
	assert_vself_preserves_full_cli(default_result.output)
	override_result :=
		os.execute('CC=cc VEXE=${os.quoted_path(noop)} ${os.quoted_path(tool)} self -o /tmp/vself_macos_prealloc_test')
	assert override_result.exit_code == 0, override_result.output
	assert !override_result.output.contains('-new-compiler'), override_result.output
	assert !override_result.output.contains('-b fastc'), override_result.output
	assert override_result.output.contains('-cc cc'), override_result.output
	assert override_result.output.contains('-prealloc'), override_result.output
	assert_vself_preserves_full_cli(override_result.output)
	old_result :=
		os.execute('CC=cc VEXE=${os.quoted_path(noop)} ${os.quoted_path(tool)} self -old-compiler -o /tmp/vself_macos_old_test')
	assert old_result.exit_code == 0, old_result.output
	assert !old_result.output.contains('-new-compiler'), old_result.output
	assert !old_result.output.contains('-b fastc'), old_result.output
	assert_vself_preserves_full_cli(old_result.output)
}

fn test_plain_self_replacement_preserves_cli_and_embedded_v3() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'vself_full_cli_replacement_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	for directory in ['vlib', 'thirdparty'] {
		os.symlink(os.join_path(vroot, directory), os.join_path(root, directory)) or { panic(err) }
	}

	// The stub keeps this test fast, but only emits a replacement when vself asks
	// for cmd/v. Targeting standalone v3.v makes the replacement fail.
	mock_source := os.join_path(root, 'mock_compiler.v')
	os.write_file(mock_source, "module main

import os

fn main() {
	if os.args.last() != 'cmd/v' {
		eprintln('expected cmd/v, got ' + os.args.last())
		exit(1)
	}
	mut output := ''
	for i, arg in os.args {
		if arg == '-o' && i + 1 < os.args.len {
			output = os.args[i + 1]
		}
	}
	if output == '' {
		eprintln('missing -o')
		exit(1)
	}
	os.cp(os.getenv('VSELF_TEST_FULL_CLI'), output) or {
		eprintln(err)
		exit(1)
	}
	println(os.args.last())
}
") or { panic(err) }
	isolated_vexe := os.join_path(root, 'v')
	mock_build := os.execute('${os.quoted_path(vexe)} -old-compiler -o ${os.quoted_path(isolated_vexe)} ${os.quoted_path(mock_source)}')
	assert mock_build.exit_code == 0, mock_build.output
	vself_tool := os.join_path(root, 'vself')
	vself_build := os.execute('${os.quoted_path(vexe)} -old-compiler -o ${os.quoted_path(vself_tool)} ${os.quoted_path(os.join_path(vroot, 'cmd', 'tools', 'vself.v'))}')
	assert vself_build.exit_code == 0, vself_build.output

	self_result := os.execute('env -u CC VFLAGS="" VOSARGS="" VSELF_TEST_FULL_CLI=${os.quoted_path(vexe)} VEXE=${os.quoted_path(isolated_vexe)} ${os.quoted_path(vself_tool)} self -silent')
	assert self_result.exit_code == 0, self_result.output
	assert self_result.output.contains('cmd/v'), self_result.output
	assert !self_result.output.contains('vlib/v3/v3.v'), self_result.output
	assert os.is_executable(isolated_vexe)
	assert os.is_executable(os.join_path(root, 'v_old'))

	version_result := os.execute('VFLAGS="" VOSARGS="" VEXE=${os.quoted_path(isolated_vexe)} ${os.quoted_path(isolated_vexe)} version')
	assert version_result.exit_code == 0, version_result.output
	assert version_result.output.starts_with('V '), version_result.output
	help_result := os.execute('VFLAGS="" VOSARGS="" VEXE=${os.quoted_path(isolated_vexe)} ${os.quoted_path(isolated_vexe)} help self')
	assert help_result.exit_code == 0, help_result.output
	assert help_result.output.contains('Rebuild V with the passed options.'), help_result.output

	program_source := os.join_path(root, 'main.v')
	os.write_file(program_source, 'fn main() { println(42) }\n') or { panic(err) }
	program := os.join_path(root, 'program')
	v3_build := os.execute('VFLAGS="" VOSARGS="" VEXE=${os.quoted_path(isolated_vexe)} ${os.quoted_path(isolated_vexe)} -new-compiler -gc none -silent -o ${os.quoted_path(program)} ${os.quoted_path(program_source)}')
	assert v3_build.exit_code == 0, v3_build.output
	program_result := os.execute(os.quoted_path(program))
	assert program_result.exit_code == 0, program_result.output
	assert program_result.output.trim_space() == '42', program_result.output
}
