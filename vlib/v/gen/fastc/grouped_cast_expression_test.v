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

fn test_fastc_smartcast_pointer_rebinding_preserves_original_storage() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_smartcast_pointer_rebind_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), r'module main
struct Atom { mut: value int }
struct Wrap { base &Ty }
struct ResultBox { base &Ty }
type Ty = Atom | Wrap | ResultBox
struct Holder { mut: value Ty }
struct Counter { mut: value int }
fn negated(start &Ty) &Ty {
	mut value := start
	if value !is Wrap { return value }
	value = value.base
	return value
}
fn other_branch(start &Ty) &Ty {
	mut value := start
	if value !is Wrap {} else { value = value.base }
	return value
}
fn matched(start &Ty) &Ty {
	mut value := start
	match value {
		Wrap { value = value.base }
		ResultBox { value = value.base }
		else {}
	}
	return value
}
fn replace(mut value Ty, replacement Ty) {
	if value is Wrap { value = replacement }
}
fn replace_match(mut value Ty, replacement Ty) {
	match value {
		ResultBox { value = replacement }
		else {}
	}
}
fn assert_atom(value Ty, expected int) {
	if value is Atom { assert value.value == expected } else { assert false }
}
fn assert_wrap(value Ty, target &Ty) {
	if value is Wrap { assert voidptr(value.base) == voidptr(target) } else { assert false }
}
fn assert_result(value Ty, target &Ty) {
	if value is ResultBox { assert voidptr(value.base) == voidptr(target) } else { assert false }
}
fn main() {
	mut base := Ty(Atom{value: 42})
	mut wrapped := Ty(Wrap{base: &base})
	mut expected := &wrapped
	if expected is Wrap { expected = expected.base }
	assert voidptr(expected) == voidptr(&base)
	assert_wrap(wrapped, &base)
	assert_atom(base, 42)
	if expected is Atom { assert expected.value == 42 } else { assert false }
	assert_wrap(wrapped, &base)
	assert voidptr(negated(&wrapped)) == voidptr(&base)
	assert voidptr(other_branch(&wrapped)) == voidptr(&base)
	assert voidptr(matched(&wrapped)) == voidptr(&base)
	assert_wrap(wrapped, &base)
	mut result := Ty(ResultBox{base: &base})
	mut result_ref := &result
	if result_ref is ResultBox { result_ref = result_ref.base }
	assert voidptr(result_ref) == voidptr(&base)
	assert_result(result, &base)
	assert_result(result, &base)
	mut holder := Holder{value: Wrap{base: &base}}
	mut field_ref := &holder.value
	if field_ref is Wrap { field_ref = field_ref.base }
	assert voidptr(field_ref) == voidptr(&base)
	assert_wrap(holder.value, &base)
	assert_wrap(holder.value, &base)
	mut nested := Ty(ResultBox{base: &wrapped})
	mut cursor := &nested
	if cursor is ResultBox {
		cursor = cursor.base
		if cursor is Wrap { cursor = cursor.base }
	}
	assert voidptr(cursor) == voidptr(&base)
	assert_result(nested, &wrapped)
	assert_wrap(wrapped, &base)
	assert_result(nested, &wrapped)
	mut matched_nested := &nested
	match matched_nested {
		ResultBox {
			matched_nested = matched_nested.base
			match matched_nested {
				Wrap { matched_nested = matched_nested.base }
				else {}
			}
		}
		else {}
	}
	assert voidptr(matched_nested) == voidptr(&base)
	assert_result(nested, &wrapped)
	assert_wrap(wrapped, &base)
	mut replaced := Ty(Wrap{base: &base})
	replace(mut replaced, Ty(Atom{value: 7}))
	if replaced is Atom { assert replaced.value == 7 } else { assert false }
	mut replaced_match := Ty(ResultBox{base: &base})
	replace_match(mut replaced_match, Ty(Atom{value: 9}))
	if replaced_match is Atom { assert replaced_match.value == 9 } else { assert false }
	mut counter := Counter{value: 1}
	mut counter_ref := &counter
	counter_ref.value = 2
	assert counter.value == 2
	assert voidptr(counter_ref) == voidptr(&counter)
	println("smartcast pointer rebinding preserves original storage")
}
') or { panic(err) }
	for backend in ['c', 'fastc'] {
		program := os.join_path(root, 'program_${backend}')
		mut flags := ['-b', backend, '-nocache', '-gc', 'none']
		if backend == 'fastc' {
			// Concrete sum-tag checks use the same full runtime as compiler builds.
			flags << ['-d', 'fastc_real_builtin']
		}
		flags << ['-o', program, os.join_path(root, 'main.v')]
		build := cmdexec.run(@VEXE, flags)
		assert build.exit_code == 0, build.output
		run := cmdexec.run(program, [])
		assert run.exit_code == 0, run.output
		assert run.output.trim_space() == 'smartcast pointer rebinding preserves original storage', run.output
	}
}
