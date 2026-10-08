import os
import v.pref

const vexe = @VEXE
const vroot = os.dir(vexe)

fn assert_vself_preserves_full_cli(output string) {
	assert !output.contains('-selfhost'), output
	assert !output.contains('vlib/v/v.v'), output
	assert output.contains('cmd/v'), output
}

fn assert_vself_uses_single_prod_build(output string) {
	assert output.contains('-no-memory-limit'), output
	assert output.contains('-prealloc'), output
	assert !output.contains('-fprofile'), output
	assert output.count('cmd/v') == 1, output
	assert_vself_preserves_full_cli(output)
}

fn vself_mock_compiler_source() string {
	return "module main

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
"
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
	build := os.exec([vexe, '-o', '${tool}', os.join_path(vroot, 'cmd', 'tools', 'vself.v')])
	assert build.exit_code == 0, build.output
	for compiler in ['tcc', 'tinyc'] {
		result :=
			os.exec(['env', 'VEXE=' + '${noop}', '${tool}', 'self', '-cc', compiler, '-o',
				'/tmp/vself_' + '${compiler}' + '_test'])
		assert result.exit_code == 0, result.output
		assert !result.output.contains('-new-compiler'), result.output
		assert !result.output.contains('-b fastc'), result.output
		assert !result.output.contains('-prealloc'), result.output
		assert_vself_preserves_full_cli(result.output)
	}
	for compiler in ['tcc', 'tinyc'] {
		result :=
			os.exec(['env', 'VEXE=' + '${noop}',
				'VFLAGS=-cc ' + '${compiler}' + ' -no-retry-compilation', '${tool}', 'self', '-o',
				'/tmp/vself_vflags_' + '${compiler}' + '_test'])
		assert result.exit_code == 0, result.output
		assert !result.output.contains('-new-compiler'), result.output
		assert !result.output.contains('-b fastc'), result.output
		assert !result.output.contains('-prealloc'), result.output
		assert !result.output.contains('-cc'), result.output
		assert_vself_preserves_full_cli(result.output)
	}
	clang_result :=
		os.exec(['env', 'VEXE=' + '${noop}', '${tool}', 'self', '-cc', 'clang', '-o',
			'/tmp/vself_clang_test'])
	assert clang_result.exit_code == 0, clang_result.output
	assert !clang_result.output.contains('-new-compiler'), clang_result.output
	assert !clang_result.output.contains('-b fastc'), clang_result.output
	assert clang_result.output.contains('-prealloc'), clang_result.output
	assert_vself_preserves_full_cli(clang_result.output)
	clang_override_result :=
		os.exec(['env', 'VEXE=' + '${noop}', 'VFLAGS=-cc tcc', '${tool}', 'self', '-cc', 'clang',
			'-o', '/tmp/vself_vflags_clang_test'])
	assert clang_override_result.exit_code == 0, clang_override_result.output
	assert !clang_override_result.output.contains('-new-compiler'), clang_override_result.output
	assert !clang_override_result.output.contains('-b fastc'), clang_override_result.output
	assert clang_override_result.output.contains('-prealloc'), clang_override_result.output
	assert_vself_preserves_full_cli(clang_override_result.output)
	old_result :=
		os.exec(['env', 'VEXE=' + '${noop}', '${tool}', 'self', '-old-compiler', '-o',
			'/tmp/vself_old_test'])
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
	build := os.exec([vexe, '-o', '${tool}', os.join_path(vroot, 'cmd', 'tools', 'vself.v')])
	assert build.exit_code == 0, build.output
	result :=
		os.exec(['env', '-u', 'CC', 'VFLAGS=', 'VEXE=' + '${noop}', '${tool}', 'self', '-o',
			'/tmp/vself_v3_c_backend_test'])
	assert result.exit_code == 0, result.output
	assert !result.output.contains('-b fastc'), result.output
	assert result.output.contains('-prealloc'), result.output
	assert_vself_preserves_full_cli(result.output)
	prod_result :=
		os.exec(['env', '-u', 'CC', 'VFLAGS=', 'VEXE=' + '${noop}', '${tool}', 'self', '-prod',
			'-o', '/tmp/vself_linux_prod_test'])
	assert prod_result.exit_code == 0, prod_result.output
	assert !prod_result.output.contains('-parallel-cc'), prod_result.output
	assert_vself_uses_single_prod_build(prod_result.output)
	vflags_parallel_result :=
		os.exec(['env', '-u', 'CC', 'VFLAGS=-parallel-cc', 'VEXE=' + '${noop}', '${tool}', 'self',
			'-prod', '-o', '/tmp/vself_linux_prod_vflags_test'])
	assert vflags_parallel_result.exit_code == 0, vflags_parallel_result.output
	assert_vself_uses_single_prod_build(vflags_parallel_result.output)
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
	build := os.exec([vexe, '-o', '${tool}', os.join_path(vroot, 'cmd', 'tools', 'vself.v')])
	assert build.exit_code == 0, build.output
	default_result :=
		os.exec(['env', '-u', 'CC', 'VFLAGS=', 'VEXE=' + '${noop}', '${tool}', 'self', '-o',
			'/tmp/vself_macos_prealloc_test'])
	assert default_result.exit_code == 0, default_result.output
	default_cc := if os.uname().machine in ['arm64', 'aarch64']
		&& !pref.host_rejects_tcc_executables() {
		'tcc'
	} else {
		'cc'
	}
	assert !default_result.output.contains('-b fastc'), default_result.output
	assert default_result.output.contains('-cc ${default_cc}'), default_result.output
	assert default_result.output.contains('-prealloc'), default_result.output
	assert_vself_preserves_full_cli(default_result.output)
	override_result :=
		os.exec(['env', 'CC=cc', 'VFLAGS=', 'VEXE=' + '${noop}', '${tool}', 'self', '-o',
			'/tmp/vself_macos_prealloc_test'])
	assert override_result.exit_code == 0, override_result.output
	assert !override_result.output.contains('-new-compiler'), override_result.output
	assert !override_result.output.contains('-b fastc'), override_result.output
	assert override_result.output.contains('-cc cc'), override_result.output
	assert override_result.output.contains('-prealloc'), override_result.output
	assert_vself_preserves_full_cli(override_result.output)
	prod_result :=
		os.exec(['env', 'CC=clang', 'VEXE=' + '${noop}', '${tool}', 'self', '-prod', '-o',
			'/tmp/vself_macos_prod_test'])
	assert prod_result.exit_code == 0, prod_result.output
	assert !prod_result.output.contains('-parallel-cc'), prod_result.output
	assert_vself_uses_single_prod_build(prod_result.output)
	old_result :=
		os.exec(['env', 'CC=cc', 'VFLAGS=', 'VEXE=' + '${noop}', '${tool}', 'self', '-old-compiler',
			'-o', '/tmp/vself_macos_old_test'])
	assert old_result.exit_code == 0, old_result.output
	assert !old_result.output.contains('-new-compiler'), old_result.output
	assert !old_result.output.contains('-b fastc'), old_result.output
	assert_vself_preserves_full_cli(old_result.output)
}

fn test_bsd_self_build_uses_system_cc_and_v3_safeguards() {
	$if windows {
		return
	}
	noop := os.find_abs_path_of_executable('echo') or { return }
	tool := os.join_path(os.vtmp_dir(), 'vself_bsd_defaults_test')
	defer {
		os.rm(tool) or {}
	}
	build := os.exec([vexe, '-d', 'vself_test_bsd_transition', '-o', '${tool}',
		os.join_path(vroot, 'cmd', 'tools', 'vself.v')])
	assert build.exit_code == 0, build.output
	default_result :=
		os.exec(['env', '-u', 'CC', 'VFLAGS=', 'VEXE=' + '${noop}', '${tool}', 'self', '-o',
			'/tmp/vself_bsd_defaults_test'])
	assert default_result.exit_code == 0, default_result.output
	assert default_result.output.contains('-cc cc'), default_result.output
	assert default_result.output.contains('-prealloc'), default_result.output
	assert_vself_preserves_full_cli(default_result.output)
	prod_result :=
		os.exec(['env', '-u', 'CC', 'VFLAGS=', 'VEXE=' + '${noop}', '${tool}', 'self', '-prod',
			'-o', '/tmp/vself_bsd_prod_test'])
	assert prod_result.exit_code == 0, prod_result.output
	assert !prod_result.output.contains('-parallel-cc'), prod_result.output
	assert_vself_uses_single_prod_build(prod_result.output)
	tinyc_result :=
		os.exec(['env', 'VFLAGS=', 'VEXE=' + '${noop}', '${tool}', 'self', '-cc', 'tcc', '-o',
			'/tmp/vself_bsd_tinyc_test'])
	assert tinyc_result.exit_code == 0, tinyc_result.output
	assert tinyc_result.output.contains('-prealloc'), tinyc_result.output
	assert_vself_preserves_full_cli(tinyc_result.output)
}

fn test_other_native_host_self_replacement_preserves_cli() {
	$if windows {
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
	// for cmd/v. Targeting standalone v.v makes the replacement fail.
	mock_source := os.join_path(root, 'mock_compiler.v')
	os.write_file(mock_source, vself_mock_compiler_source()) or { panic(err) }
	isolated_vexe := os.join_path(root, 'v')
	mock_build :=
		os.exec([vexe, '-o', isolated_vexe, mock_source])
	assert mock_build.exit_code == 0, mock_build.output
	vself_tool := os.join_path(root, 'vself')
	vself_build := os.exec([vexe, '-nocache', '-d', 'vself_test_other_transition', '-o',
		'${vself_tool}', os.join_path(vroot, 'cmd', 'tools', 'vself.v')])
	assert vself_build.exit_code == 0, vself_build.output

	self_result :=
		os.exec(['env', '-u', 'CC', 'VFLAGS=', 'VOSARGS=', 'VSELF_TEST_FULL_CLI=' + '${vexe}',
			'VEXE=' + '${isolated_vexe}', '${vself_tool}', 'self', '-silent'])
	assert self_result.exit_code == 0, self_result.output
	assert self_result.output.contains('cmd/v'), self_result.output
	assert !self_result.output.contains('vlib/v/v.v'), self_result.output
	assert os.is_executable(isolated_vexe)
	assert os.is_executable(os.join_path(root, 'v_old'))
	assert !os.exists(os.join_path(root, 'v1_fallback'))

	version_result :=
		os.exec(['env', 'VFLAGS=', 'VOSARGS=', 'VEXE=' + '${isolated_vexe}', isolated_vexe, 'version'])
	assert version_result.exit_code == 0, version_result.output
	assert version_result.output.starts_with('V '), version_result.output
	help_result :=
		os.exec(['env', 'VFLAGS=', 'VOSARGS=', 'VEXE=' + '${isolated_vexe}', isolated_vexe, 'help',
			'self'])
	assert help_result.exit_code == 0, help_result.output
	assert help_result.output.contains('Rebuild V with the passed options.'), help_result.output

	program_source := os.join_path(root, 'main.v')
	os.write_file(program_source, 'fn main() { println(42) }\n') or { panic(err) }
	program := os.join_path(root, 'program')
	v3_build :=
		os.exec(['env', 'VFLAGS=', 'VOSARGS=', 'VEXE=' + '${isolated_vexe}', isolated_vexe,
			'-new-compiler', '-gc', 'none', '-silent', '-o', '${program}', program_source])
	assert v3_build.exit_code == 0, v3_build.output
	program_result := os.exec([program])
	assert program_result.exit_code == 0, program_result.output
	assert program_result.output.trim_space() == '42', program_result.output
}

fn test_windows_plain_self_transition_does_not_install_fallback() {
	$if windows {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'vself_windows_transition_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	for directory in ['vlib', 'thirdparty'] {
		os.symlink(os.join_path(vroot, directory), os.join_path(root, directory)) or { panic(err) }
	}

	mock_source := os.join_path(root, 'mock_compiler.v')
	os.write_file(mock_source, vself_mock_compiler_source()) or { panic(err) }
	isolated_vexe := os.join_path(root, 'v')
	mock_build :=
		os.exec([vexe, '-o', isolated_vexe, mock_source])
	assert mock_build.exit_code == 0, mock_build.output
	vself_tool := os.join_path(root, 'vself')
	vself_build := os.exec([vexe, '-nocache', '-d', 'vself_test_windows_transition', '-o',
		'${vself_tool}', os.join_path(vroot, 'cmd', 'tools', 'vself.v')])
	assert vself_build.exit_code == 0, vself_build.output

	self_result :=
		os.exec(['env', '-u', 'CC', 'VFLAGS=', 'VOSARGS=', 'VSELF_TEST_FULL_CLI=' + '${vexe}',
			'VEXE=' + '${isolated_vexe}', '${vself_tool}', 'self', '-silent'])
	assert self_result.exit_code == 0, self_result.output
	assert !self_result.output.contains('-prealloc'), self_result.output
	assert !os.exists(os.join_path(root, 'v1_fallback.exe'))
}

fn test_self_build_keeps_fastc_backend() {
	$if windows {
		return
	}
	noop := os.find_abs_path_of_executable('echo') or { return }
	tool := os.join_path(os.vtmp_dir(), 'vself_fastc_backend_test')
	defer {
		os.rm(tool) or {}
	}
	build := os.exec([vexe, '-o', '${tool}', os.join_path(vroot, 'cmd', 'tools', 'vself.v')])
	assert build.exit_code == 0, build.output
	// Older drivers prune FastC from cmd/v unless asked, so `v self` and `v up`
	// request it explicitly.
	default_result :=
		os.exec(['env', 'VFLAGS=', 'VEXE=' + '${noop}', '${tool}', 'self', '-o',
			'/tmp/vself_fastc_default_test'])
	assert default_result.exit_code == 0, default_result.output
	assert default_result.output.contains('-compile-backend fastc'), default_result.output
	assert_vself_preserves_full_cli(default_result.output)
	prod_result :=
		os.exec(['env', 'VFLAGS=', 'VEXE=' + '${noop}', '${tool}', 'self', '-prod', '-o',
			'/tmp/vself_fastc_prod_test'])
	assert prod_result.exit_code == 0, prod_result.output
	assert prod_result.output.contains('-compile-backend fastc'), prod_result.output
	for opt_out in [['-d', 'skip_fastc'], ['-dskip_fastc'], ['-old-compiler']] {
		mut args := ['env', 'VFLAGS=', 'VEXE=' + '${noop}', '${tool}', 'self']
		args << opt_out
		args << ['-o', '/tmp/vself_fastc_opt_out_test']
		result := os.exec(args)
		assert result.exit_code == 0, result.output
		assert !result.output.contains('-compile-backend'), result.output
	}
	vflags_result :=
		os.exec(['env', 'VFLAGS=-d skip_fastc', 'VEXE=' + '${noop}', '${tool}', 'self', '-o',
			'/tmp/vself_fastc_vflags_test'])
	assert vflags_result.exit_code == 0, vflags_result.output
	assert !vflags_result.output.contains('-compile-backend'), vflags_result.output
}

fn test_arm64_self_build_preserves_native_options_and_full_cli() {
	$if windows {
		return
	}
	noop := os.find_abs_path_of_executable('echo') or { return }
	tool := os.join_path(os.vtmp_dir(), 'vself_arm64_options_${os.getpid()}')
	defer {
		os.rm(tool) or {}
	}
	build := os.exec([vexe, '-nocache', '-o', tool, os.join_path(vroot, 'cmd', 'tools', 'vself.v')])
	assert build.exit_code == 0, build.output
	for backend_args in [['-b', 'arm64'], ['-backend', 'arm64'], ['-b', 'fastc', '-b', 'arm64']] {
		mut args := ['env', 'VFLAGS=', 'VEXE=${noop}', tool, 'self']
		args << backend_args
		args << ['x2', '-o', '/tmp/vself_arm64_options_test']
		result := os.exec(args)
		assert result.exit_code == 0, result.output
		assert result.output.count('cmd/v') == 2, result.output
		assert !result.output.contains('-cc'), result.output
		assert !result.output.contains('-prealloc'), result.output
		assert !result.output.contains('-compile-backend fastc'), result.output
		assert result.output.contains('-gc none'), result.output
		assert result.output.contains('-nocache'), result.output
		assert result.output.contains('-no-memory-limit'), result.output
		assert_vself_preserves_full_cli(result.output)
	}
	for limit_flag in ['-memory-limit', '--memory-limit'] {
		for extra in [[]string{}, ['-prod']] {
			mut limited_args := ['env', 'VFLAGS=', 'VEXE=${noop}', tool, 'self', '-b', 'arm64',
				limit_flag, '16384', '-o', '/tmp/vself_arm64_limited_test']
			limited_args << extra
			limited := os.exec(limited_args)
			assert limited.exit_code == 0, limited.output
			assert limited.output.contains('${limit_flag} 16384'), limited.output
			assert !limited.output.contains('-no-memory-limit'), limited.output
			mut inherited_args := ['env', 'VFLAGS=-b arm64 ${limit_flag} 16384', 'VEXE=${noop}',
				tool, 'self', '-o', '/tmp/vself_arm64_inherited_limit_test']
			inherited_args << extra
			inherited := os.exec(inherited_args)
			assert inherited.exit_code == 0, inherited.output
			assert !inherited.output.contains('-no-memory-limit'), inherited.output
		}
	}
	vflags_result := os.exec(['env', 'VFLAGS=-b arm64', 'VEXE=${noop}', tool, 'self', '-o',
		'/tmp/vself_arm64_vflags_test'])
	assert vflags_result.exit_code == 0, vflags_result.output
	assert !vflags_result.output.contains('-cc'), vflags_result.output
	assert !vflags_result.output.contains('-prealloc'), vflags_result.output
	assert !vflags_result.output.contains('-compile-backend fastc'), vflags_result.output
	assert_vself_preserves_full_cli(vflags_result.output)
	c_override_result := os.exec(['env', 'VFLAGS=-b arm64', 'VEXE=${noop}', tool, 'self', '-b',
		'c', '-o', '/tmp/vself_arm64_c_override_test'])
	assert c_override_result.exit_code == 0, c_override_result.output
	assert c_override_result.output.contains('-prealloc'), c_override_result.output
	assert c_override_result.output.contains('-compile-backend fastc'), c_override_result.output
	assert_vself_preserves_full_cli(c_override_result.output)

	failing_compiler := os.find_abs_path_of_executable('false') or { return }
	failure := os.exec(['env', 'VFLAGS=', 'VEXE=${failing_compiler}', tool, 'self', '-b', 'arm64',
		'-o', '/tmp/vself_arm64_failed_test'])
	assert failure.exit_code != 0, failure.output
	assert !failure.output.contains('bootstrap fallback'), failure.output
}
