module main

import os
import v.cmdexec

fn test_macos_native_bridge_falls_back_when_header_expansion_is_excessive() ! {
	$if !macos {
		return
	}
	if !os.is_file(os.join_path(@VEXEROOT, 'thirdparty', 'tcc', 'tcc.exe')) { return }
	root := os.join_path(os.vtmp_dir(), 'tcc bridge expansion ${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'bridge_expansion' }\n")!
	os.write_file(os.join_path(root, 'h0.h'), '#pragma once\ntypedef int BridgeNumber;\n')!
	for i in 1 .. 14 {
		previous := i - 1
		os.write_file(os.join_path(root, 'h${i}.h'), '#pragma once\n#include "h${previous}.h"\n#include "h${previous}.h"\n')!
	}
	os.write_file(os.join_path(root, 'native.m'), '#import <Foundation/Foundation.h>\n#include "h13.h"\nstatic int bridge_answer(BridgeNumber n) { return n + 1; }\n')!
	source := os.join_path(root, 'main.c.v')
	os.write_file(source, '#flag darwin -framework Foundation\n#include "@DIR/native.m"\nfn C.bridge_answer(n int) int\nfn main() { assert C.bridge_answer(41) == 42 }\n')!
	exe := os.join_path(root, 'program')
	args := ['-u', 'VFLAGS', '-u', 'VEXE', '-u', 'CFLAGS', '-u', 'LDFLAGS',
		'V3CACHE=' + os.join_path(root, 'cache'), 'V3_CACHE_TRACE=1', 'V_MACOS_V3_NO_FALLBACK=1',
		@VEXE, '-gc', 'none', '-o', exe, source]
	build := cmdexec.run('env', args)
	assert build.exit_code == 0, build.output
	assert build.output.contains('native bridge header expansion exceeds its safe limit'), build.output
	run := cmdexec.run(exe, [])
	assert run.exit_code == 0, run.output
}
