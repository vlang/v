import os
import v.cmdexec

fn test_c_only_compiler_builds_and_reuses_full_backend_compiler() {
	root := os.join_path(os.vtmp_dir(), 'c_only_dispatch_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	os.write_file(source, 'fn main() { println(42) }\n')!
	cache := os.join_path(root, 'tools')
	mut environment := {
		'VFLAGS':                 ''
		'VTOOLS_CACHE_DIR':       cache
		'VTOOLS_NO_CACHE':        ''
		'VTOOLS_CACHE_VERBOSE':   '1'
		'V_MACOS_V3_NO_FALLBACK': '1'
	}
	// Keep this regression valid when the test runner itself includes optional backends.
	compiler := os.join_path(root, 'v_c_only' + $if windows { '.exe' } $else { '' })
	build := cmdexec.run_in_merged_with_env(@VEXE, ['-nocache', '-gc', 'none', '-prealloc', '-o',
		compiler, os.join_path(@VMODROOT, 'cmd', 'v')], root, environment)
	assert build.exit_code == 0, build.output
	// C compilation must not create an optional compiler at all.
	c_output := os.join_path(root, 'default.c')
	c_build := cmdexec.run_in_merged_with_env(compiler, ['-nocache', '-o', c_output, source], root, environment)
	assert c_build.exit_code == 0, c_build.output
	assert !os.exists(cache), c_build.output
	// Forwarding VFLAGS and CLI flags must select the last backend exactly once.
	environment['VFLAGS'] = '-b wasm'
	fastc_output := os.join_path(root, 'fastc.c')
	first := cmdexec.run_in_merged_with_env(compiler, ['-b', 'fastc', '-o', fastc_output, source], root, environment)
	assert first.exit_code == 0, first.output
	assert os.is_file(fastc_output), first.output
	assert first.output.contains('recompiling `v3_full`'), first.output
	second := cmdexec.run_in_merged_with_env(compiler, ['-b', 'fastc', '-o', fastc_output, source], root, environment)
	assert second.exit_code == 0, second.output
	assert !second.output.contains('recompiling `v3_full`'), second.output
	// The cached compiler includes other backends as well.
	wasm_source := os.join_path(root, 'empty.v')
	os.write_file(wasm_source, 'fn main() {}\n')!
	wasm_output := os.join_path(root, 'empty.wasm')
	wasm := cmdexec.run_in_merged_with_env(compiler, ['-b', 'wasm', '-o', wasm_output, wasm_source], root, environment)
	assert wasm.exit_code == 0, wasm.output
	assert !wasm.output.contains('recompiling `v3_full`'), wasm.output
	assert os.read_bytes(wasm_output)![..4] == [u8(0), `a`, `s`, `m`]
	// Native compilation must run through the same full compiler.
	$if macos && arm64 {
		native := cmdexec.run_in_merged_with_env(compiler, ['-b', 'arm64', 'run', source], root, environment)
		assert native.exit_code == 0, native.output
		assert native.output.trim_space().split_into_lines().last() == '42', native.output
		assert !native.output.contains('recompiling `v3_full`'), native.output
	}
	// Disabling tool caching builds a disposable full compiler and cleans its stage.
	uncached_tmp := os.join_path(root, 'uncached')
	os.mkdir(uncached_tmp)!
	environment['VTMP'] = uncached_tmp
	environment['VTOOLS_NO_CACHE'] = '1'
	uncached := cmdexec.run_in_merged_with_env(compiler, ['-b', 'fastc', '-o', fastc_output, source], root, environment)
	assert uncached.exit_code == 0, uncached.output
	assert uncached.output.contains('building `v3_full`'), uncached.output
	assert os.ls(uncached_tmp)!.filter(it.starts_with('.v-toolcache-stage-')) == []
}
