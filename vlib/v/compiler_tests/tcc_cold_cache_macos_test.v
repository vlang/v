module main

import os
import v.cmdexec

fn test_macos_tcc_large_cold_cache_respects_no_retry() ! {
	if !os.is_file(os.join_path(@VEXEROOT, 'thirdparty', 'tcc', 'tcc.exe')) {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'tcc large cold cache ${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'large_cold_bridge' }\n")!
	os.write_file(os.join_path(root, 'native.m'), 'static int bridge_answer(void) { return 42; }\n')!
	source := os.join_path(root, 'main.c.v')
	exe := os.join_path(root, 'program')
	// Cross the large-source AST cutoff with an unused body so the fallback
	// target still has only a small program to emit.
	os.write_file(source, '#include "@DIR/native.m"\nfn C.bridge_answer() int\nfn main() { assert C.bridge_answer() == 42; println(@CCOMPILER) }\nfn padding() {\nmut n := 0\n' +
		'n += 1\n'.repeat(150_000) + 'println(n)\n}\n')!
	args := ['-u', 'VFLAGS', '-u', 'VEXE', '-u', 'CFLAGS', '-u', 'LDFLAGS',
		'V3CACHE=' + os.join_path(root, 'cache'), 'V3_CACHE_TRACE=1', 'V_MACOS_V3_NO_FALLBACK=1',
		@VEXE, '-cc', 'tcc', '-no-retry-compilation', '-o', exe, source]
	for reason in ['cold source graph is too large for scoped cache population',
		'persisted large source graph bypasses cache setup'] {
		build := cmdexec.run('env', args)
		assert build.exit_code != 0, build.output
		assert build.output.contains(reason), build.output
		assert build.output.contains('cannot build the TCC native bridge: the module cache is unavailable and -no-retry-compilation prevents switching compilers'), build.output
		assert !os.exists(exe)
	}
}
