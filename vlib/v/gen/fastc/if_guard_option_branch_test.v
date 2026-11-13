module fastc

import os
import v.cmdexec

// `x := if h := opt { h.call() } else { none }` keeps the payload type of the option
// branch, which is resolved while the guard local is in scope.
fn test_if_guard_expression_keeps_option_branch_payload() ! {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_if_guard_option_branch_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'if_guard_option_branch' }")!
	os.write_file(os.join_path(root, 'main.v'), "module main
struct Host {
mut:
	flushes int
}
fn (mut h Host) flush(mut messages []string) ?string {
	messages << 'flushed'
	h.flushes++
	return 'next.html'
}
fn navigate(parser_host ?&Host) string {
	mut messages := []string{}
	navigation := if mut host := parser_host { host.flush(mut messages) } else { none }
	if destination := navigation {
		return '\${destination} \${messages.len}'
	}
	return 'none'
}
fn main() {
	mut host := &Host{}
	println(navigate(host))
	println(navigate(none))
}
")!
	for backend in ['c', 'fastc'] {
		program := os.join_path(root, 'program_' + backend)
		build := cmdexec.run(@VEXE, ['-b', backend, '-nocache', '-o', program, root])
		assert build.exit_code == 0, '${backend}: ${build.output}'
		run := cmdexec.run(program, [])
		assert run.exit_code == 0, '${backend}: ${run.output}'
		assert run.output.trim_space() == 'next.html 1\nnone', '${backend}: ${run.output}'
	}
}
