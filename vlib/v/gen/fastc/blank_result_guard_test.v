module fastc

import os
import v.cmdexec

fn test_fastc_blank_void_result_guards() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_blank_result_guard_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'main.v'), 'module main
fn save(ok bool) ! {
	if !ok { return error("failed") }
}
fn main() {
	mut total := 0
	if _ := save(true) { total++ } else { panic(err) }
	if _ := save(false) { panic("unexpected success") } else {
		assert err.msg() == "failed"
		total++
	}
	total += if _ := save(true) { 40 } else { 0 }
	println(total)
}
') or { panic(err) }
	program := os.join_path(root, 'program')
	build := cmdexec.run(@VEXE, ['-b', 'fastc', '-nocache', '-o', program, root])
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '42', run.output
}
