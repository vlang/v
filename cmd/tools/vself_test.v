import os

const vexe = @VEXE
const vroot = os.dir(vexe)

fn assert_vself_builds_v3(output string) {
	assert output.contains('-selfhost'), output
	assert output.contains('vlib/v3/v3.v'), output
	assert !output.contains('cmd/v/v.v'), output
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
		assert_vself_builds_v3(result.output)
	}
	for compiler in ['tcc', 'tinyc'] {
		result :=
			os.execute('VEXE=${os.quoted_path(noop)} VFLAGS="-cc ${compiler} -no-retry-compilation" ${os.quoted_path(tool)} self -o /tmp/vself_vflags_${compiler}_test')
		assert result.exit_code == 0, result.output
		assert !result.output.contains('-new-compiler'), result.output
		assert !result.output.contains('-b fastc'), result.output
		assert !result.output.contains('-prealloc'), result.output
		assert !result.output.contains('-cc'), result.output
		assert_vself_builds_v3(result.output)
	}
	clang_result :=
		os.execute('VEXE=${os.quoted_path(noop)} ${os.quoted_path(tool)} self -cc clang -o /tmp/vself_clang_test')
	assert clang_result.exit_code == 0, clang_result.output
	assert !clang_result.output.contains('-new-compiler'), clang_result.output
	assert !clang_result.output.contains('-b fastc'), clang_result.output
	assert clang_result.output.contains('-prealloc'), clang_result.output
	assert_vself_builds_v3(clang_result.output)
	clang_override_result :=
		os.execute('VEXE=${os.quoted_path(noop)} VFLAGS="-cc tcc" ${os.quoted_path(tool)} self -cc clang -o /tmp/vself_vflags_clang_test')
	assert clang_override_result.exit_code == 0, clang_override_result.output
	assert !clang_override_result.output.contains('-new-compiler'), clang_override_result.output
	assert !clang_override_result.output.contains('-b fastc'), clang_override_result.output
	assert clang_override_result.output.contains('-prealloc'), clang_override_result.output
	assert_vself_builds_v3(clang_override_result.output)
	old_result :=
		os.execute('VEXE=${os.quoted_path(noop)} ${os.quoted_path(tool)} self -old-compiler -o /tmp/vself_old_test')
	assert old_result.exit_code == 0, old_result.output
	assert !old_result.output.contains('-new-compiler'), old_result.output
	assert !old_result.output.contains('-b fastc'), old_result.output
	assert_vself_builds_v3(old_result.output)
}

fn test_linux_default_self_build_uses_v3_fastc() {
	$if !linux {
		return
	}
	noop := os.find_abs_path_of_executable('echo') or { return }
	tool := os.join_path(os.vtmp_dir(), 'vself_v3_fastc_test')
	defer {
		os.rm(tool) or {}
	}
	build := os.execute('${os.quoted_path(vexe)} -o ${os.quoted_path(tool)} ${os.quoted_path(os.join_path(vroot, 'cmd', 'tools', 'vself.v'))}')
	assert build.exit_code == 0, build.output
	result :=
		os.execute('env -u CC VEXE=${os.quoted_path(noop)} ${os.quoted_path(tool)} self -o /tmp/vself_v3_fastc_test')
	assert result.exit_code == 0, result.output
	assert result.output.contains('-b fastc'), result.output
	assert !result.output.contains('fastc_real_builtin'), result.output
	assert !result.output.contains('-cc'), result.output
	assert_vself_builds_v3(result.output)
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
	assert default_result.output.contains('-b fastc'), default_result.output
	assert !default_result.output.contains('fastc_real_builtin'), default_result.output
	assert !default_result.output.contains('-cc'), default_result.output
	assert_vself_builds_v3(default_result.output)
	override_result :=
		os.execute('CC=cc VEXE=${os.quoted_path(noop)} ${os.quoted_path(tool)} self -o /tmp/vself_macos_prealloc_test')
	assert override_result.exit_code == 0, override_result.output
	assert !override_result.output.contains('-new-compiler'), override_result.output
	assert !override_result.output.contains('-b fastc'), override_result.output
	assert override_result.output.contains('-cc cc'), override_result.output
	assert override_result.output.contains('-prealloc'), override_result.output
	assert_vself_builds_v3(override_result.output)
	old_result :=
		os.execute('CC=cc VEXE=${os.quoted_path(noop)} ${os.quoted_path(tool)} self -old-compiler -o /tmp/vself_macos_old_test')
	assert old_result.exit_code == 0, old_result.output
	assert !old_result.output.contains('-new-compiler'), old_result.output
	assert !old_result.output.contains('-b fastc'), old_result.output
	assert_vself_builds_v3(old_result.output)
}
