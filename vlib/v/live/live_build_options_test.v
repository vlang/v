module main

import os
import v.cmdexec

fn test_live_rebuild_keeps_compile_configuration() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'live build options_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'modules', 'liveconfig'))!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'modules', 'liveconfig', 'liveconfig.v'), 'module liveconfig\npub const value = 42\n@[live]\npub fn main() int { return value }\n')!
	source := os.join_path(root, 'main.v')
	os.write_file(source, r'module main
import liveconfig
@[live]
fn value() int {
 $if live_build_option ? { return liveconfig.main() } $else { return 0 }
}
@[live]
fn main() { assert value() == 42; println("ok") }
')!
	program := os.join_path(root, 'program')
	options := ['-cc', 'clang', '-prod', '-gc', 'none', '-d', 'live_build_option', '-path',
		os.join_path(root, 'modules') + '|@vlib|@vmodules', '-cflags', '-DLIVE_BUILD_OPTIONS=1',
		'-ldflags', '-lm', '-live']
	generated := os.join_path(root, 'program.c')
	codegen := cmdexec.run(@VEXE, [...options, '-o', generated, source])
	assert codegen.exit_code == 0, codegen.output
	c_source := os.read_file(generated)!
	for flag in ['live_build_option', '-prod', '-gc', '-path', 'LIVE_BUILD_OPTIONS=1', '-ldflags'] {
		assert c_source.contains(flag), flag
	}
	build := cmdexec.run(@VEXE, [...options, '-o', program, source])
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'ok', run.output
}
