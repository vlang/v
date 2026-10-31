module driver

import os

const msvc_clang = 'S:/llvm/bin/clang.exe'

fn test_a_clang_for_the_msvc_abi_is_rejected_on_windows() {
	msg := v3_windows_clang_msvc_abi_error(msvc_clang, 'clang', 'x86_64-pc-windows-msvc',
		'windows', []) or { '' }
	assert msg != ''
	assert msg.contains('-cc ${msvc_clang}')
	assert msg.contains('x86_64-pc-windows-msvc')
	// The message names every way out.
	assert msg.contains('--target=x86_64-w64-mingw32')
	assert msg.contains('-fuse-ld=lld')
	assert msg.contains('-cc msvc')
	assert msg.contains('-cc gcc')
}

fn test_a_mingw_clang_and_an_unprobed_one_pass() {
	for triple in ['x86_64-w64-windows-gnu', 'x86_64-w64-mingw32', 'x86_64-pc-windows-gnu',
		'aarch64-w64-mingw32', ''] {
		if msg := v3_windows_clang_msvc_abi_error(msvc_clang, 'clang', triple, 'windows',
			[])
		{
			assert false, '${triple}: ${msg}'
		}
	}
}

fn test_only_a_windows_clang_build_is_checked() {
	if msg := v3_windows_clang_msvc_abi_error(msvc_clang, 'clang', 'x86_64-pc-windows-msvc',
		'linux', [])
	{
		assert false, msg
	}
	for compiler in ['gcc', 'tinyc', 'msvc', 'mingw', 'cplusplus'] {
		if msg := v3_windows_clang_msvc_abi_error('cc', compiler, 'x86_64-pc-windows-msvc',
			'windows', [])
		{
			assert false, '${compiler}: ${msg}'
		}
	}
}

fn test_user_c_flags_that_retarget_clang_are_trusted() {
	retargeted := [
		['--target=x86_64-w64-mingw32', '--sysroot=S:/mingw64', '-fuse-ld=lld'],
		['-target', 'x86_64-w64-mingw32'],
		['-O2', '--target', 'x86_64-w64-windows-gnu'],
	]
	for flags in retargeted {
		assert v3_c_flags_retarget_clang(flags), flags.str()
		if msg := v3_windows_clang_msvc_abi_error(msvc_clang, 'clang', 'x86_64-pc-windows-msvc',
			'windows', flags)
		{
			assert false, msg
		}
	}
	for flags in [[]string{}, ['-O2'], ['-fuse-ld=lld'], ['--sysroot=S:/mingw64'], ['-Wl,--target=x']] {
		assert !v3_c_flags_retarget_clang(flags), flags.str()
	}
	_ := v3_windows_clang_msvc_abi_error(msvc_clang, 'clang', 'x86_64-pc-windows-msvc',
		'windows', ['-O2']) or {
		assert false
		''
	}
}

// msvc_abi_test_compiler is the compiler under test: the one running this test, or the
// V3 compiler beside a V1 fallback launcher.
fn msvc_abi_test_compiler() string {
	if os.base(@VEXE) in ['v1_fallback', 'v1_fallback.exe'] {
		return os.join_path(os.dir(@VEXE), 'v' + $if windows { '.exe' } $else { '' })
	}
	return @VEXE
}

// write_msvc_clang_stub writes a fake clang whose default target is the MSVC ABI. Every
// other invocation fails, so a build that reaches the C compiler fails there, visibly,
// and never in the probe.
fn write_msvc_clang_stub(dir string) string {
	$if windows {
		path := os.join_path(dir, 'clang.bat')
		os.write_file(path, '@echo off\r\nif "%1"=="-dumpmachine" (echo x86_64-pc-windows-msvc) else (exit 1)\r\n') or {
			panic(err)
		}
		return path
	} $else {
		path := os.join_path(dir, 'clang')
		os.write_file(path, '#!/bin/sh\nif [ "\$1" = "-dumpmachine" ]; then echo x86_64-pc-windows-msvc; else exit 1; fi\n') or {
			panic(err)
		}
		os.chmod(path, 0o700) or { panic(err) }
		return path
	}
}

// with_cflags runs `callback` with the CFLAGS environment variable set to `value` (unset
// when empty) and restores the previous value afterwards.
fn with_cflags(value string, callback fn ()) {
	previous := os.getenv_opt('CFLAGS')
	defer {
		if old := previous {
			os.setenv('CFLAGS', old, true)
		} else {
			os.unsetenv('CFLAGS')
		}
	}
	if value == '' {
		os.unsetenv('CFLAGS')
	} else {
		os.setenv('CFLAGS', value, true)
	}
	callback()
}

const msvc_abi_message = 'targets the MSVC ABI'

fn test_the_rejection_waits_for_the_c_build_and_honours_cflags() {
	compiler := msvc_abi_test_compiler()
	root := os.join_path(os.vtmp_dir(), 'v3_msvc_clang_abi_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	clang := write_msvc_clang_stub(root)
	source := os.join_path(root, 'hello.v')
	os.write_file(source, "fn main() {\n\tprintln('hi')\n}\n")!
	output := os.join_path(root, 'hello.exe')
	common := [compiler, '-new-compiler', '-os', 'windows', '-cc', clang]
	with_cflags('', fn [common, source, output] () {
		// Source listings build nothing, so the compiler's ABI is irrelevant to them.
		for listing in ['-print-v-files', '-print-watched-files'] {
			listed := os.exec([...common, listing, source])
			assert listed.exit_code == 0, '${listing}: ${listed.output}'
			assert !listed.output.contains(msvc_abi_message), listed.output
			assert listed.output.contains('hello.v'), listed.output
		}
		// A build is rejected before the stub, which cannot compile, is ever asked to.
		built := os.exec([...common, '-o', output, source])
		assert built.exit_code == 1, built.output
		assert built.output.contains(msvc_abi_message), built.output
	})
	// CFLAGS retargets clang like -cflags does: the build reaches the stub and fails there.
	with_cflags('--target=x86_64-w64-mingw32 -fuse-ld=lld', fn [common, source, output] () {
		built := os.exec([...common, '-o', output, source])
		assert !built.output.contains(msvc_abi_message), built.output
		assert built.exit_code != 0, built.output
	})
}
