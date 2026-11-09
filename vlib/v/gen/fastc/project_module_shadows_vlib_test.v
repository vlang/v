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

fn fastc_write_stamp_module(dir string, name string, stamp string) ! {
	os.mkdir_all(dir)!
	os.write_file(os.join_path(dir, '${name}.v'), 'module ${name}\npub fn stamp() string {\n\treturn "${stamp}"\n}\n')!
}

fn fastc_build_and_run(root string, args []string) string {
	program := os.join_path(root, 'program')
	mut build_args := ['-b', 'fastc', '-nocache']
	build_args << args
	build_args << ['-o', program, os.join_path(root, 'project')]
	build := cmdexec.run(@VEXE, build_args)
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
	return run.output.trim_space()
}

// Explicit `-path` search roots keep their order, also over a project module.
fn test_explicit_search_path_module_is_kept_over_project_module() ! {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_project_module_search_path_${os.getpid()}')
	project := os.join_path(root, 'project')
	defer { os.rmdir_all(root) or {} }
	fastc_write_stamp_module(os.join_path(project, 'clipboard'), 'clipboard', 'project')!
	fastc_write_stamp_module(os.join_path(root, 'deps', 'clipboard'), 'clipboard', 'search path')!
	os.mkdir_all(os.join_path(project, 'events'))!
	os.write_file(os.join_path(project, 'v.mod'), "Module { name: 'project' }")!
	os.write_file(os.join_path(project, 'events', 'events.v'), 'module events\nimport clipboard\npub fn stamp() string {\n\treturn clipboard.stamp()\n}\n')!
	os.write_file(os.join_path(project, 'main.v'), 'module main\nimport events\nfn main() {\n\tprintln(events.stamp())\n}\n')!
	assert fastc_build_and_run(root, ['-path', '${os.join_path(root, 'deps')}|@vlib']) == 'search path'
}

// A dependency outside the project keeps its own sibling module, even when the project
// holds a module of the same name (one program cannot load both).
fn test_dependency_keeps_its_own_sibling_module() ! {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_project_module_dependency_${os.getpid()}')
	project := os.join_path(root, 'project')
	defer { os.rmdir_all(root) or {} }
	fastc_write_stamp_module(os.join_path(project, 'helper'), 'helper', 'project')!
	fastc_write_stamp_module(os.join_path(root, 'dep', 'helper'), 'helper', 'dependency')!
	os.write_file(os.join_path(root, 'dep', 'dep.v'), 'module dep\nimport helper\npub fn stamp() string {\n\treturn helper.stamp()\n}\n')!
	os.write_file(os.join_path(project, 'v.mod'), "Module { name: 'project' }")!
	os.write_file(os.join_path(project, 'main.v'), 'module main\nimport dep\nfn main() {\n\tprintln(dep.stamp())\n}\n')!
	assert fastc_build_and_run(root, []) == 'dependency'
}
