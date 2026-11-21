module fastc

import os
import v.cmdexec

// An exhaustive `match` whose arms are C constants has an untyped fallback.
fn test_fastc_exhaustive_matches_of_c_constants() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_exhaustive_matches_of_c_constants_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'main.v'), 'module main

#include <sys/socket.h>

enum How {
	recv
	send
	both
}

fn shutdown_how(h How) int {
	how := match h {
		.recv { C.SHUT_RD }
		.send { C.SHUT_WR }
		.both { C.SHUT_RDWR }
	}
	return how
}

fn main() {
	println(shutdown_how(.recv) == C.SHUT_RD)
	println(shutdown_how(.both) == C.SHUT_RDWR)
}
') or { panic(err) }
	program := os.join_path(root, 'program')
	build := cmdexec.run_in_merged_with_env(@VEXE, ['-b', 'fastc', '-nocache', '-gc', 'none', '-o',
		program, root], os.real_path(os.join_path(@DIR, '../../../..')), {
		'VEXE':                   @VEXE
		'VFLAGS':                 ''
		'VOSARGS':                ''
		'V_MACOS_V3_NO_FALLBACK': '1'
	})
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'true\ntrue', run.output
}
