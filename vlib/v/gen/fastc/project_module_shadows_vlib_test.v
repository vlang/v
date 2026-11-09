module fastc

import os
import v.cmdexec

// A project module named like a vlib module (Vox's `clipboard`) must win over vlib in
// every importing directory, as it does for the C backend.
fn test_project_module_shadows_vlib_module_in_submodules() ! {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_project_module_shadow_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'clipboard'))!
	os.mkdir_all(os.join_path(root, 'events'))!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'project_module_shadow' }")!
	os.write_file(os.join_path(root, 'clipboard', 'event.v'), 'module clipboard
pub struct Event {
pub mut:
	stamp f64
}
pub fn Event.make() !&Event {
	return &Event{}
}
pub fn (mut e Event) set_stamp(stamp f64) {
	e.stamp = stamp
}
')!
	os.write_file(os.join_path(root, 'events', 'events.v'), 'module events
import clipboard
pub fn stamp() f64 {
	mut event := clipboard.Event.make() or { panic(err) }
	if event.stamp == 0 {
		event.set_stamp(2.5)
	}
	return event.stamp
}
')!
	os.write_file(os.join_path(root, 'main.v'), 'module main
import events
fn main() {
	println(events.stamp())
}
')!
	for backend in ['c', 'fastc'] {
		program := os.join_path(root, 'program_' + backend)
		build := cmdexec.run(@VEXE, ['-b', backend, '-nocache', '-o', program, root])
		assert build.exit_code == 0, '${backend}: ${build.output}'
		run := cmdexec.run(program, [])
		assert run.exit_code == 0, '${backend}: ${run.output}'
		assert run.output.trim_space() == '2.5', '${backend}: ${run.output}'
	}
}
