module fastc

import os
import v.cmdexec

// `app.results.len` reads a channel promoted from an embedded struct through its
// embedded path.
fn test_promoted_channel_properties() ! {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_embedded_channel_property_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'embedded_channel_property' }")!
	os.write_file(os.join_path(root, 'main.v'), "module main
struct Result {
	n int
}
struct Navigation {
mut:
	results chan Result
}
struct App {
	Navigation
mut:
	frames int
}
fn main() {
	mut app := &App{
		Navigation: Navigation{
			results: chan Result{cap: 4}
		}
	}
	app.results <- Result{3}
	if app.results.len == 0 {
		println('empty')
		return
	}
	queued := app.results.len
	capacity := app.results.cap
	direct := app.Navigation.results.len
	println('\${queued} \${capacity} \${direct}')
}
")!
	for backend in ['c', 'fastc'] {
		program := os.join_path(root, 'program_' + backend)
		build := cmdexec.run(@VEXE, ['-b', backend, '-nocache', '-o', program, root])
		assert build.exit_code == 0, '${backend}: ${build.output}'
		run := cmdexec.run(program, [])
		assert run.exit_code == 0, '${backend}: ${run.output}'
		assert run.output.trim_space() == '1 4 1', '${backend}: ${run.output}'
	}
}
