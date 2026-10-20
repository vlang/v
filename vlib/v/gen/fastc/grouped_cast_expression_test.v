module fastc

import os
import v.cmdexec
import v.pref

fn test_fastc_grouped_arithmetic_and_embedded_casts_preserve_evaluation() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_grouped_cast_expression_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), r'module main
struct Leaf { value int }
struct Branch { value int }
type Tree = Leaf | Branch
fn bump(mut calls int, value int) int {
	calls++
	return value
}
fn main() {
	mut calls := 0
	assert (bump(mut calls, 3) + 4) * (2 + 1) == 21
	assert calls == 1
	leaf := Tree(Leaf{ value: 7 })
	branch := Tree(Branch{ value: 5 })
	assert (leaf as Leaf).value + (branch as Branch).value == 12
	assert bump(mut calls, (leaf as Leaf).value) + ((2 + 1) * 3) == 16
	assert calls == 2
	assert !(false && bump(mut calls, (leaf as Leaf).value) > 0)
	assert calls == 2
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

fn test_fastc_same_sum_cast_preserves_imported_payloads_and_evaluation() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_same_sum_cast_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'model')) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'same_sum_cast' }") or { panic(err) }
	os.write_file(os.join_path(root, 'model', 'model.v'), r'module model
pub struct Payload { pub: value int }
pub type Type = Payload | int
pub type Alias = Type
pub struct Holder { pub: value Type }
pub struct Calls { pub mut: count int }
pub fn reference(value Type) &Type { return &value }
pub fn forward(owned Type) &Type { return reference(Type(owned)) }
pub fn copy(value Type) Type { copied := Type(value); return copied }
pub fn from_alias(value Alias) Type { return Type(value) }
pub fn next(mut calls Calls, value int) Type {
	calls.count++
	return Payload{value}
}
pub fn returned(mut calls Calls) Type { return Type(next(mut calls, 21)) }
pub fn collected(mut calls Calls) []Type { return [Type(next(mut calls, 31)), Type(4)] }
pub fn member(holder Holder) Type { return Type(holder.value) }
pub fn narrowed(value Type) Type {
	if value is Payload { return Type(value) }
	return Type(value)
}
pub fn pointer(value &Type) &Type { return &Type(value) }
') or { panic(err) }
	os.write_file(os.join_path(root, 'main.v'), r'module main
import model
fn payload(value model.Type, expected int) {
	if value is model.Payload { assert value.value == expected } else { assert false }
}
fn main() {
	mut calls := model.Calls{}
	original := model.next(mut calls, 11)
	assert calls.count == 1
	ref := model.forward(original)
	payload(*ref, 11)
	payload(model.copy(original), 11)
	payload(model.returned(mut calls), 21)
	assert calls.count == 2
	values := model.collected(mut calls)
	assert calls.count == 3
	payload(values[0], 31)
	value := values[1]
	if value is int { assert value == 4 } else { assert false }
	payload(*model.forward(model.next(mut calls, 41)), 41)
	assert calls.count == 4
	payload(model.member(model.Holder{value: original}), 11)
	payload(model.narrowed(original), 11)
	primitive := model.narrowed(model.Type(7))
	if primitive is int { assert primitive == 7 } else { assert false }
	aliased := model.Alias(original)
	payload(model.from_alias(aliased), 11)
	assert voidptr(model.pointer(ref)) == voidptr(ref)
	qualified := model.Type(original)
	payload(qualified, 11)
	println("same sum casts preserve payloads")
}
') or { panic(err) }
	program := os.join_path(root, 'program')
	// Concrete sum-tag checks use FastC's existing full runtime, as compiler builds do.
	build := cmdexec.run(@VEXE, ['-b', 'fastc', '-d', 'fastc_real_builtin', '-nocache', '-gc',
		'none', '-o', program, os.join_path(root, 'main.v')])
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'same sum casts preserve payloads', run.output
}
