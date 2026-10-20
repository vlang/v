module c

import os
import v.flat
import v.types

fn test_sum_fixed_array_pointer_storage_and_addressable_projection() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.sum_types.set('Recursive', ['int', '&[2]Recursive', '&int'])
	tc.type_aliases.set_id('PairPointer', flat.source_type_id('&[2]Recursive'))
	tc.sum_types.set('Aliased', ['int', 'PairPointer'])
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	g.emit_sum_type(flat.source_type_id('Recursive'))
	g.emit_sum_type(flat.source_type_id('Aliased'))
	declarations := g.sb.str()
	assert declarations.contains('void* _ptr__2Recursive;')
	assert declarations.contains('void* PairPointer;')
	assert declarations.contains('i64* _ptr__int;')
	assert !declarations.contains('Array_fixed_Recursive_2* _ptr__2Recursive;')
	slot_type := g.sum_pointer_slot_type(tc.parse_canonical_type(flat.source_type_id('&[2]Recursive')))
	assert !g.fn_ptr_nested_typedef_ready('${slot_type}*')
	// The same canonical array gets one typed slot view even through an alias.
	g.sb.clear()
	g.fixed_array_typedefs()
	g.sum_pointer_slot_typedefs()
	typedefs := g.sb.str()
	assert typedefs.contains('typedef Recursive Array_fixed_Recursive_2[2];')
	assert typedefs.count('__attribute__((may_alias))') == 1
	array_index := typedefs.index('typedef Recursive Array_fixed_Recursive_2[2];') or { panic('missing array typedef') }
	slot_index := typedefs.index('__v3_sum_fixed_pointer_') or { panic('missing slot typedef') }
	assert array_index < slot_index
	assert g.fn_ptr_nested_typedef_ready('${slot_type}*')
	indirect := tc.parse_canonical_type(flat.source_type_id('&&[2]Recursive'))
	assert g.value_c_type(indirect) == '${slot_type}*'
	assert g.callback_c_type(indirect) == '${slot_type}*'
	assert g.fn_return_type_name(indirect) == '${slot_type}*'
	assert g.cast_c_type(indirect) == '${slot_type}*'
	base := a.add_node(flat.Node{ kind: .ident, value: 'wrapped', typ: flat.source_type_id('Recursive') })
	field := flat.Node{ kind: .selector, value: '_ptr__2Recursive', op: .dot }
	g.sb.clear()
	assert g.gen_erased_sum_pointer_selector(field, base, tc.parse_canonical_type(flat.source_type_id('Recursive')), true)
	assert g.sb.str() == '(*(${slot_type}*)&((wrapped)._ptr__2Recursive))'
}

fn test_raw_recursive_fixed_array_pointer_variants_preserve_shared_mutation() {
	dir := os.join_path(os.vtmp_dir(), 'sum_fixed_pointer_${os.getpid()}')
	os.mkdir_all(os.join_path(dir, 'model'))!
	defer { os.rmdir_all(dir) or {} }
	os.write_file(os.join_path(dir, 'v.mod'), "Module { name: 'sum_fixed_pointer' }")!
	model := 'module model
pub type Node = int | &[2]Node
pub type PairPointer = &[2]Node
pub type Owner = int | PairPointer
pub fn wrap(value PairPointer) Owner { return Owner(value) }
pub fn pointer(value Owner) PairPointer { return value as PairPointer }
pub fn replace(mut value PairPointer, other PairPointer) { value = other }
pub fn rebind(mut value Owner, other PairPointer) {
 if mut value is PairPointer { replace(mut value, other) } else { assert false }
}
'
	os.write_file(os.join_path(dir, 'model', 'model.v'), model)!
	source := 'import model
type Node = int | &[2]Node
type Nested = int | &[2][2]Node
fn pointer(value Node) &[2]Node { return value as &[2]Node }
fn pair_first(value &[2]Node) int { return (*value)[0] as int }
fn change_first(mut value &[2]Node) { unsafe { (*value)[0] = Node(11) } }
fn replace(mut value &[2]Node, other &[2]Node) { value = other }
type SlotCallback = fn (&&[2]Node) &&[2]Node
struct SlotHolder { slot &&[2]Node
 callback SlotCallback }
fn identity_slot(value &&[2]Node) &&[2]Node { return value }
fn store_slot(value &&[2]Node, other &[2]Node) { unsafe { *value = other } }
fn rebind(mut value Node, other &[2]Node) {
 if mut value is &[2]Node {
  slot := unsafe { &value }
  callback := identity_slot
  returned := callback(slot)
  holder := SlotHolder{slot: returned, callback: SlotCallback(identity_slot)}
  assert voidptr(slot) == voidptr(holder.slot)
  callback_slot := holder.callback(holder.slot)
  assert voidptr(callback_slot) == voidptr(slot)
  store_slot(callback_slot, other)
  replace(mut *slot, other)
 } else { assert false }
}
fn main() {
 mut first := [Node(1), Node(2)]!
 mut second := [Node(7), Node(8)]!
 first_ptr := unsafe { &first }
 second_ptr := unsafe { &second }
 mut wrapped := Node(first_ptr)
 assert voidptr(pointer(wrapped)) == voidptr(first_ptr)
 if mut wrapped is &[2]Node {
  slot := unsafe { &wrapped }
  change_first(mut *slot)
  assert pair_first(wrapped) == 11
 } else { assert false }
 assert (first[0] as int) == 11
 copies := [wrapped].clone()
 assert voidptr(pointer(copies[0])) == voidptr(first_ptr)
 assert copies[0].str() == "Node(&[<circular>, <circular>])"
 rebind(mut wrapped, second_ptr)
 assert voidptr(pointer(wrapped)) == voidptr(second_ptr)
 assert pair_first(pointer(wrapped)) == 7
 assert pair_first(pointer(copies[0])) == 11
 mut nested := [[Node(21), Node(22)]!, [Node(23), Node(24)]!]!
 nested_ptr := unsafe { &nested }
 container := Nested(nested_ptr)
 nested_view := container as &[2][2]Node
 assert voidptr(nested_view) == voidptr(nested_ptr)
 unsafe { (*nested_view)[1][0] = Node(25) }
 assert (nested[1][0] as int) == 25
 assert container.str().contains("25")
 mut qualified := [model.Node(31), model.Node(32)]!
 mut other := [model.Node(41), model.Node(42)]!
 qualified_ptr := model.PairPointer(unsafe { &qualified })
 other_ptr := model.PairPointer(unsafe { &other })
 mut alias_owner := model.wrap(qualified_ptr)
 assert voidptr(model.pointer(alias_owner)) == voidptr(qualified_ptr)
 model.rebind(mut alias_owner, other_ptr)
 assert voidptr(model.pointer(alias_owner)) == voidptr(other_ptr)
 unsafe { (*model.pointer(alias_owner))[0] = model.Node(43) }
 assert (other[0] as int) == 43
 println("fixed-array pointer sums ok")
}'
	path := os.join_path(dir, 'main.v')
	os.write_file(path, source)!
	for jobs in ['1', '3'] {
		output := os.join_path(dir, 'main_${jobs}.c')
		// Configure only the child; the surrounding test session keeps its own job count.
		mut command := os.new_process(@VEXE)
		command.set_args(['-new-compiler', '-nocache', '-o', output, path])
		mut environment := os.environ()
		environment['VJOBS'] = jobs
		command.set_environment(environment)
		command.set_redirect_stdio_merged()
		command.run()
		generated_output := command.stdout_slurp()
		command.wait()
		code := command.code
		command.close()
		assert code == 0, generated_output
		generated := os.read_file(output)!
		assert generated.contains('void* _ptr__2Node;')
		assert generated.contains('__attribute__((may_alias))')
		for autofree in [false, true] {
			mut args := [@VEXE, '-new-compiler', '-nocache', '-prod', '-cc', 'clang']
			if autofree { args << ['-gc', 'none', '-autofree'] }
			args << ['run', path]
			result := os.exec(args)
			assert result.exit_code == 0, 'autofree=${autofree}: ${result.output}'
			assert result.output.contains('fixed-array pointer sums ok'), result.output
		}
	}
}

fn test_owned_aligned_fixed_array_sum_drops_elements_once() {
	cc := os.find_abs_path_of_executable('clang') or { return }
	dir := os.join_path(os.vtmp_dir(), 'sum_fixed_pointer_owner_${os.getpid()}')
	os.mkdir_all(dir)!
	defer { os.rmdir_all(dir) or {} }
	compiler := os.join_path(dir, 'v-ownership')
	vlib := os.join_path(@VEXEROOT, 'vlib')
	build := os.exec([@VEXE, '-new-compiler', '-b', 'c', '-cc', cc, '-gc', 'none', '-d', 'ownership',
		'-path', vlib, '-o', compiler, os.join_path(vlib, 'v', 'v.v')])
	assert build.exit_code == 0, build.output
	source := os.join_path(@VMODROOT, 'vlib', 'v', 'gen', 'c', 'testdata', 'sumtype_fixed_array_pointer', 'aligned_owner.c.v')
	for prealloc in [false, true] {
		mut args := ['-new-compiler', '-nocache', '-no-closures', '-gc', 'none', '-cc', cc, '-autofree']
		if prealloc { args << '-prealloc' }
		args << ['run', source]
		result := os.exec([compiler, ...args])
		assert result.exit_code == 0, 'prealloc=${prealloc}: ${result.output}'
	}
}
