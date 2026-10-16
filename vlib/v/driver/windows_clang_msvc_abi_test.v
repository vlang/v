module driver

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
