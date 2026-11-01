import os

const vexe = @VEXE
const vroot = os.dir(vexe)

fn assert_vself_preserves_full_cli(output string) {
	assert !output.contains('-selfhost'), output
	assert !output.contains('vlib/v/v.v'), output
	assert output.contains('cmd/v'), output
}

fn assert_vself_preserves_prod_options(output string) {
	assert output.contains('-no-memory-limit'), output
	assert output.contains('-prealloc'), output
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
	os.chmod(output, 0o755) or { panic(err) }
	os.write_file(output + '.manifest', 'replacement manifest') or { panic(err) }
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
	assert_vself_preserves_prod_options(prod_result.output)
	vflags_parallel_result :=
		os.exec(['env', '-u', 'CC', 'VFLAGS=-parallel-cc', 'VEXE=' + '${noop}', '${tool}', 'self',
			'-prod', '-o', '/tmp/vself_linux_prod_vflags_test'])
	assert vflags_parallel_result.exit_code == 0, vflags_parallel_result.output
	assert_vself_preserves_prod_options(vflags_parallel_result.output)
	assert !vflags_parallel_result.output.contains('-fprofile'), vflags_parallel_result.output
	assert vflags_parallel_result.output.count('cmd/v') == 1, vflags_parallel_result.output
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
	assert !default_result.output.contains('-b fastc'), default_result.output
	assert default_result.output.contains('-cc tcc'), default_result.output
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
	assert_vself_preserves_prod_options(prod_result.output)
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
	assert_vself_preserves_prod_options(prod_result.output)
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
	os.write_file('${isolated_vexe}.manifest', 'previous manifest')!
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
	assert os.read_file('${isolated_vexe}.manifest')! == 'replacement manifest'
	assert os.read_file(os.join_path(root, 'v_old.manifest'))! == 'previous manifest'
	assert !os.exists(os.join_path(root, 'v2.manifest'))
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

fn test_self_build_defaults_to_c_backend() {
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
	// Ordinary and production self-builds must not opt into optional backends.
	default_result :=
		os.exec(['env', 'VFLAGS=', 'VEXE=' + '${noop}', '${tool}', 'self', '-o',
			'/tmp/vself_fastc_default_test'])
	assert default_result.exit_code == 0, default_result.output
	assert !default_result.output.contains('-compile-backend'), default_result.output
	for backend in ['fastc', 'arm64', 'x64', 'wasm', 'eval', 'ssa'] {
		assert default_result.output.contains('-d skip_${backend}'), default_result.output
	}
	assert_vself_preserves_full_cli(default_result.output)
	prod_result :=
		os.exec(['env', 'VFLAGS=', 'VEXE=' + '${noop}', '${tool}', 'self', '-prod', '-o',
			'/tmp/vself_fastc_prod_test'])
	assert prod_result.exit_code == 0, prod_result.output
	assert !prod_result.output.contains('-compile-backend'), prod_result.output
	for opt_in in [['-compile-backend', 'fastc'], ['-all-backends']] {
		result := os.exec(['env', 'VFLAGS=', 'VEXE=${noop}', tool, 'self', ...opt_in, '-o',
			'/tmp/vself_backend_opt_in_test'])
		assert result.exit_code == 0, result.output
		assert result.output.contains(opt_in.join(' ')), result.output
		assert !result.output.contains('-d skip_fastc'), result.output
	}
	wasm_result := os.exec(['env', 'VFLAGS=-compile-backend wasm', 'VEXE=${noop}', tool, 'self',
		'-o', '/tmp/vself_wasm_opt_in_test'])
	assert wasm_result.exit_code == 0, wasm_result.output
	assert !wasm_result.output.contains('-d skip_wasm'), wasm_result.output
	assert !wasm_result.output.contains('-d skip_ssa'), wasm_result.output
	assert wasm_result.output.contains('-d skip_fastc'), wasm_result.output
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

fn test_self_bootstrap_handles_the_old_peak_rss_return_type() ! {
	$if windows {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'vself_bootstrap_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'vc'))!
	defer { os.rmdir_all(root) or {} }
	os.symlink(os.join_path(vroot, 'cmd'), os.join_path(root, 'cmd'))!
	mock_source := os.join_path(root, 'mock_compiler.v')
	os.write_file(mock_source, vself_mock_compiler_source())!
	mock := os.join_path(root, 'stage2')
	mock_build := os.exec([vexe, '-o', mock, mock_source])
	assert mock_build.exit_code == 0, mock_build.output
	tool := os.join_path(root, 'vself')
	build := os.exec([vexe, '-o', tool, os.join_path(vroot, 'cmd', 'tools', 'vself.v')])
	assert build.exit_code == 0, build.output
	// Force the first self-build to fail and exercise the C bootstrap fallback.
	broken := os.join_path(root, 'v')
	os.write_file(broken, '#!/bin/sh\nexit 1\n')!
	os.chmod(broken, 0o755)!
	os.write_file(os.join_path(root, 'vc', 'v.c'), '#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
typedef long i64;
#define V_PEAK_RSS_SCOPE
V_PEAK_RSS_SCOPE long long v3_bench_peak_rss_kb(void) { return 42; }
i64 v3_bench_peak_rss_kb(void);
int main(int argc, char **argv) {
    const char *output = NULL;
    for (int i = 1; i + 1 < argc; i++) {
        if (strcmp(argv[i], "-o") == 0) output = argv[i + 1];
    }
    if (!output || v3_bench_peak_rss_kb() != 42) return 1;
    FILE *input = fopen(getenv("VSELF_TEST_STAGE2"), "rb");
    FILE *dest = fopen(output, "wb");
    if (!input || !dest) return 1;
    int byte;
    while ((byte = fgetc(input)) != EOF) fputc(byte, dest);
    fclose(input);
    if (fclose(dest) != 0 || chmod(output, 0755) != 0) return 1;
    return 0;
}
')!
	output := os.join_path(root, 'replacement')
	// Make any attempt to launch Bash fail while leaving the system tools available.
	no_bash := os.join_path(root, 'no_bash')
	os.mkdir_all(no_bash)!
	bash := os.join_path(no_bash, 'bash')
	os.write_file(bash, '#!/bin/sh\nexit 127\n')!
	os.chmod(bash, 0o755)!
	result := os.exec(['env', 'PATH=${no_bash}:${os.getenv('PATH')}', 'VFLAGS=', 'VEXE=${broken}',
		'VSELF_TEST_STAGE2=${mock}', 'VSELF_TEST_FULL_CLI=${vexe}', tool, 'self', '-o', output])
	assert result.exit_code == 0, result.output
	assert os.is_executable(output), result.output
	assert !os.exists(os.join_path(root, '.vself_bootstrap_v1'))
	assert !os.exists(os.join_path(root, '.vself_bootstrap_v2'))
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
	assert !c_override_result.output.contains('-compile-backend'), c_override_result.output
	assert_vself_preserves_full_cli(c_override_result.output)

	failing_compiler := os.find_abs_path_of_executable('false') or { return }
	failure := os.exec(['env', 'VFLAGS=', 'VEXE=${failing_compiler}', tool, 'self', '-b', 'arm64',
		'-o', '/tmp/vself_arm64_failed_test'])
	assert failure.exit_code != 0, failure.output
	assert !failure.output.contains('bootstrap fallback'), failure.output
}

fn assert_no_preprocessor_includes(code string) {
	for line in code.split_into_lines() {
		text := line.trim_space()
		if text.starts_with('#') {
			directive := text[1..].trim_left(' \t')
			assert !directive.starts_with('include') && !directive.starts_with('import'), line
		}
	}
}

fn test_self_generated_compiler_c_needs_no_headers() {
	$if !macos && !linux {
		return
	}
	clang := os.find_abs_path_of_executable('clang') or { return }
	root := os.join_path(os.vtmp_dir(), 'vself_no_headers_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	compiler := os.join_path(root, 'compiler')
	build := os.exec(['env', '-u', 'VFLAGS', '-u', 'VEXE', '-u', 'CC', '-u', 'CFLAGS', '-u', 'LDFLAGS',
		'VTMP=${root}', vexe, '-cc', clang, '-gc', 'none', '-keepc', '-no-retry-compilation', '-o',
		compiler, 'self'])
	assert build.exit_code == 0, build.output
	files := os.walk_ext(root, '.tmp.c').filter(os.file_name(it).starts_with('compiler.'))
	assert files.len == 1, os.walk_ext(root, '.c').str()
	code := os.read_file(files[0])!
	assert_no_preprocessor_includes(code)
	checked := os.exec([clang, '-std=gnu11', '-nostdinc', '-fsyntax-only', '-Wno-everything',
		'-Wno-incompatible-function-pointer-types', '-Wno-int-conversion',
		'-Werror=implicit-function-declaration', '-x', 'c', files[0]])
	assert checked.exit_code == 0, checked.output
	version := os.exec([compiler, 'version'])
	assert version.exit_code == 0 && version.output.starts_with('V '), version.output
}

fn test_track_heap_guards_need_no_header() {
	$if !macos && !linux {
		return
	}
	clang := os.find_abs_path_of_executable('clang') or { return }
	root := os.join_path(os.vtmp_dir(), 'vself_heap_guards_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	generated := os.join_path(root, 'main.c')
	os.write_file(source, 'fn main() {}\n')!
	for mode in [['-gc', 'none'], ['-gc', 'boehm'], ['-gc', 'vgc'], ['-gc', 'none', '-prealloc']] {
		built := os.exec([vexe, '-nocache', ...mode, '-d', 'track_heap', '-o', generated, source])
		assert built.exit_code == 0, '${mode}: ${built.output}'
		if mode == ['-gc', 'none'] {
			assert_no_preprocessor_includes(os.read_file(generated)!)
			checked := os.exec([clang, '-std=gnu11', '-nostdinc', '-fsyntax-only', '-Wno-everything',
				'-Wno-incompatible-function-pointer-types', '-Wno-int-conversion', '-x', 'c',
				generated])
			assert checked.exit_code == 0, checked.output
		} else {
			checked := os.exec([vexe, '-nocache', '-cc', clang, ...mode, '-d', 'track_heap', '-o',
				os.join_path(root, 'invalid'), source])
			assert checked.exit_code != 0, mode.str()
			assert checked.output.contains('track_heap requires manual memory management'), checked.output
		}
	}
}
