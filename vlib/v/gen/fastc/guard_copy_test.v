module fastc

import os
import v.cmdexec
import v.pref

fn test_fastc_copies_after_terminal_guards_keep_the_boxed_type() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_guard_copy_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), r'module main

struct Leaf { value int }
struct Link { next &Item }
type Item = Leaf | Link
type Scalar = int | string

fn follow(value Item, enabled bool) int {
	if value !is Link || !enabled { return -1 }
	original := value
	assert original is Link
	mut current := value
	mut depth := 0
	for current is Link {
		depth++
		current = *current.next
	}
	if current is Leaf { return depth + current.value }
	return -2
}

fn follow_mut(mut value Item) int {
	if value !is Link { return -1 }
	if value !is Link { return -2 }
	mut current := value
	mut depth := 0
	for current is Link {
		depth++
		current = *current.next
	}
	assert value is Link
	if current is Leaf { return depth + current.value }
	return -3
}

fn positive(value Scalar) int {
	if value is int {
		mut local := value
		local += 1
		if value !is int { return -1 }
		mut after := value
		after += 2
		return local + after
	}
	return -2
}

fn main() {
	leaf := Item(Leaf{value: 40})
	inner := Item(Link{next: &leaf})
	outer := Item(Link{next: &inner})
	assert follow(outer, true) == 42
	assert follow(inner, true) == 41
	assert follow(leaf, true) == -1
	assert follow(outer, false) == -1
	assert outer is Link
	mut mutable_outer := outer
	assert follow_mut(mut mutable_outer) == 42
	assert mutable_outer is Link
	assert positive(Scalar(10)) == 23
	assert positive(Scalar("other")) == -2
	println("guard copies work")
}
') or { panic(err) }
	mut prefs := pref.new_preferences()
	prefs.vroot = @VEXEROOT
	prefs.building_v = true
	prefs.backend = 'fastc'
	prefs.ccompiler = 'clang'
	prefs.user_defines = ['gc_none', 'no_backtrace', 'v3_backend']
	prefs.exclude = [os.join_path(@VEXEROOT, 'vlib/builtin/backtraces_nix.c.v')]
	c_source := generate_files([root], prefs) or { panic(err) }
	c_file := os.join_path(root, 'main.c')
	program := os.join_path(root, 'program')
	os.write_file(c_file, c_source) or { panic(err) }
	build := cmdexec.run('cc', ['-std=gnu11', '-w', '-pthread', '-o', program, c_file])
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'guard copies work', run.output
}
