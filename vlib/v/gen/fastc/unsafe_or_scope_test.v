module fastc

import os
import v.cmdexec
import v.pref

fn test_fastc_unsafe_map_or_return_keeps_enclosing_block_scopes() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_unsafe_or_scope_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), r'module main
import os
struct Node {
mut:
	value int
	children map[string]&Node
}
fn ensure_child(parent &Node, name string) &Node {
	if name in parent.children {
		return unsafe { parent.children[name] or { panic("missing child ${name}") } }
	}
	mut new_node := &Node{ value: 40 }
	new_node.value += 2
	return new_node
}
fn child_or_return(parent &Node, name string) &Node {
	if name.len > 0 {
		return unsafe { parent.children[name] or { return &Node{ value: -1 } } }
	}
	mut new_node := &Node{ value: 90 }
	new_node.value += 9
	return new_node
}
fn child_or_panic(parent &Node, name string) &Node {
	return unsafe { unsafe { parent.children[name] or { panic("missing child ${name}") } } }
}
fn child_or_value(parent &Node, name string) &Node {
	return unsafe { parent.children[name] or { &Node{ value: -2 } } }
}
fn main() {
	child := &Node{ value: 7 }
	parent := &Node{ children: { "present": child } }
	if os.args.len > 1 {
		child_or_panic(parent, "absent")
		return
	}
	assert ensure_child(parent, "present") == child
	created := ensure_child(parent, "absent")
	assert created.value == 42
	assert child_or_return(parent, "present") == child
	missing := child_or_return(parent, "absent")
	assert missing.value == -1
	later := child_or_return(parent, "")
	assert later.value == 99
	assert child_or_panic(parent, "present") == child
	assert child_or_value(parent, "present") == child
	fallback := child_or_value(parent, "absent")
	assert fallback.value == -2
	println("unsafe fallback scopes work")
}
') or { panic(err) }
	mut prefs := pref.new_preferences()
	prefs.vroot = @VEXEROOT
	prefs.building_v = true
	prefs.backend = 'fastc'
	prefs.ccompiler = 'clang'
	prefs.user_defines = ['gc_none', 'no_backtrace', 'v3_backend']
	source := generate_files([root], prefs) or { panic(err) }
	c_path := os.join_path(root, 'main.c')
	program := os.join_path(root, 'program')
	os.write_file(c_path, source) or { panic(err) }
	build := cmdexec.run('cc', ['-std=gnu11', '-o', program, c_path])
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'unsafe fallback scopes work', run.output
	failed := cmdexec.run(program, ['missing'])
	assert failed.exit_code != 0, failed.output
	assert failed.output.contains('missing child absent'), failed.output
}
