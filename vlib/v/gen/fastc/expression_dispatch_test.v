module fastc

import os
import v.cmdexec
import v.pref

fn test_fastc_expression_dispatch_preserves_rewritten_and_mutating_operands() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_expression_dispatch_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), r'module main

struct Leaf { mut: value int }
struct Branch { mut: value int }
type Tree = Leaf | Branch
enum Mark { ready waiting }
type Alias = Mark

fn optional(found bool) ?int {
	if !found { return none }
	return 41
}

fn propagate(found bool) ?int {
	value := optional(found)?
	return value + 1
}

fn forward(found bool) ?int {
	return optional(found)?
}

fn set_common(mut tree Tree, value int) {
	tree.value = value
}

fn main() {
	leaf := Tree(Leaf{value: 7})
	branch := Tree(Branch{value: 5})
	assert (leaf is Leaf && (leaf as Leaf).value == 7) && !(branch is Leaf)
	assert branch !is Leaf || (branch as Leaf).value == 0
	assert (leaf as Leaf).value + (branch as Branch).value == 12
	mut total := 3
	total += (leaf as Leaf).value
	assert total == 10
	assert (propagate(true) or { 0 }) == 42
	assert (propagate(false) or { 9 }) == 9
	assert (forward(true) or { 0 }) == 41
	assert (forward(false) or { 8 }) == 8
	mut records := map[string]Leaf{}
	records["first"].value = 10
	records["first"].value++
	records["first"].value--
	assert records["first"].value == 10
	mut values := [1, 2, 3]
	values[0]++
	values[1]--
	values[2] += total
	assert values == [2, 1, 13]
	fixed := [4, 5, 6]!
	assert fixed.len + total == 13
	assert Alias.ready == Mark.ready
	mut common := Tree(Leaf{value: 2})
	set_common(mut common, total)
	assert common.value == 10
	common = Branch{value: 1}
	set_common(mut common, values[2])
	assert common.value == 13
}
') or { panic(err) }
	mut prefs := pref.new_preferences()
	prefs.vroot = @VEXEROOT
	prefs.building_v = true
	prefs.backend = 'fastc'
	prefs.ccompiler = 'clang'
	prefs.user_defines = ['gc_none', 'no_backtrace', 'v3_backend']
	c_source := generate_files([root], prefs) or { panic(err) }
	c_path := os.join_path(root, 'main.c')
	program := os.join_path(root, 'program')
	os.write_file(c_path, c_source) or { panic(err) }
	build := cmdexec.run('cc', ['-std=gnu11', '-O2', '-o', program, c_path])
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
}
