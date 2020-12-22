module main

import os
import testing
import v.util

fn p(s string) string {
	println(s)
	return s
}

fn main() {
	args_string := os.args[1..].join(' ')
	skips := []string{}
	vexe := os.getenv('VEXE')
	vroot := os.dir(vexe)
	util.ensure_modules_for_all_tools_are_installed('-v' in os.args)
	folder := 'cmd/tools'
	main_label := 'Building $folder ...'
	finish_label := 'building $folder'
	mut session := testing.prepare_test_session(args_string.all_before('build-tools'),
		folder, skips, main_label)
	session.rm_binaries = false
	session.test()
	eprintln(session.benchmark.total_message(finish_label))
	if session.failed {
		exit(1)
	}
	//
	mut executables := os.ls(session.vtmp_dir) ?
	executables.sort()
	executables = executables.filter(it !in ['gen1m', 'gen_vc', 'fast', 'wyhash'])
	for exe in executables {
		os.mv_by_cp(os.join_path(session.vtmp_dir, exe), os.join_path(vroot, 'cmd', 'tools',
			exe))
	}
}
