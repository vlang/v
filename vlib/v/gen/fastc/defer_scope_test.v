module fastc

import os
import v.cmdexec

// A deferred block may declare a local named like one of its function (`result`).
fn test_fastc_deferred_blocks_have_their_own_scope() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_deferred_blocks_have_their_own_scope_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'main.v'), "module main

fn work(n int) !int {
	defer {
		result := n * 2
		println('deferred \${result}')
	}
	result := n + 1
	if result > 5 {
		return error('too big')
	}
	return result
}

fn main() {
	println(work(1)!)
	println(work(9) or { -1 })
}
") or { panic(err) }
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
	assert run.output.trim_space() == 'deferred 2\n2\ndeferred 18\n-1', run.output
}
