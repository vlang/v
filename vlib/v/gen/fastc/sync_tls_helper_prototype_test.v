module fastc

import os
import v.cmdexec

// sync's native TLS helpers (`v_sync_tls_*`, from thread_helper.o) get prototypes, so an
// instance of `sync.new_tls[voidptr]` compiles.
fn test_fastc_sync_tls_helpers_are_declared() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_sync_tls_helpers_are_declared_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'main.v'), "module main

import sync

fn main() {
	_ := sync.new_tls[voidptr](unsafe { nil }) or { panic(err) }
	println('created')
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
	assert run.output.trim_space() == 'created', run.output
}
