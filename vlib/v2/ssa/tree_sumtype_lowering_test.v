module ssa

import os
import v2.ast
import v2.parser
import v2.pref
import v2.token
import v2.transformer
import v2.types

fn tree_sumtype_source() string {
	source := r'module main

type Tree = Empty | Node

struct Empty {}

struct Node {
	value int
	left  Tree
	right Tree
}

fn size(tree Tree) int {
	return match tree {
		Empty { int(0) }
		Node { 1 + size(tree.left) + size(tree.right) }
	}
}

fn println(s string) {
	_ = s
}

fn main() {
	node1 := Node{30, Empty{}, Empty{}}
	node2 := Node{20, Empty{}, Empty{}}
	tree := Node{10, node1, node2}
	println(@tree_structure_inter@)
	println(@tree_size_inter@)
}
'
	return source.replace('@tree_structure_inter@', "'tree structure:\\n \${tree}'").replace('@tree_size_inter@',
		"'tree size: \${size(tree)}'")
}

fn tree_sumtype_example_source() string {
	source := r'type Tree = Empty | Node

struct Empty {}

struct Node {
	value int
	left  Tree
	right Tree
}

fn size(tree Tree) int {
	return match tree {
		Empty { int(0) }
		Node { 1 + size(tree.left) + size(tree.right) }
	}
}

fn println(s string) {
	_ = s
}

fn main() {
	node1 := Node{30, Empty{}, Empty{}}
	node2 := Node{20, Empty{}, Empty{}}
	tree := Node{10, node1, node2}
	println(@tree_structure_inter@)
	println(@tree_size_inter@)
}
'
	return source.replace('@tree_structure_inter@', "'tree structure:\\n \${tree}'").replace('@tree_size_inter@',
		"'tree size: \${size(tree)}'")
}

fn generic_tree_sumtype_direct_wrap_source() string {
	return '
module main

struct Empty {}

struct Node[T] {
	value T
	left  Tree[T]
	right Tree[T]
}

type Tree[T] = Empty | Node[T]

fn tag(tree Tree[f64]) int {
	return match tree {
		Empty { 0 }
		Node[f64] { 1 }
	}
}

fn main() {
	empty := Tree[f64](Empty{})
	tree := Tree[f64](Node[f64]{1.0, empty, empty})
	_ = tag(tree)
}
'
}

fn generic_tree_sumtype_receiver_size_source() string {
	return '
module main

struct Empty {}

struct Node[T] {
	value T
	left  Tree[T]
	right Tree[T]
}

type Tree[T] = Empty | Node[T]

fn (tree Tree[T]) size[T]() int {
	return match tree {
		Empty { 0 }
		Node[T] { 1 }
	}
}

fn main() {
	empty := Tree[f64](Empty{})
	tree := Tree[f64](Node[f64]{1.0, empty, empty})
	_ = tree.size()
}
'
}

fn generic_tree_sumtype_insert_size_source() string {
	return '
module main

struct Empty {}

struct Node[T] {
	value T
	left  Tree[T]
	right Tree[T]
}

type Tree[T] = Empty | Node[T]

fn (tree Tree[T]) size[T]() int {
	return match tree {
		Empty { 0 }
		Node[T] { 1 + tree.left.size() + tree.right.size() }
	}
}

fn (tree Tree[T]) insert[T](x T) Tree[T] {
	return match tree {
		Empty { Node[T]{x, tree, tree} }
		Node[T] {
			if x == tree.value {
				tree
			} else if x < tree.value {
				Node[T]{
					...tree
					left: tree.left.insert(x)
				}
			} else {
				Node[T]{
					...tree
					right: tree.right.insert(x)
				}
			}
		}
	}
}

fn main() {
	mut tree := Tree[f64](Empty{})
	tree = tree.insert(0.2)
	tree = tree.insert(0.5)
	_ = tree.size()
}
'
}

fn generic_tree_nested_type_args_source() string {
	return '
module main

struct Empty {}

struct Node[T] {
	value T
	left  Tree[T]
	right Tree[T]
}

type Tree[T] = Empty | Node[T]

struct Queue[T] {
	value T
}

struct Holder[T] {
	value T
}

fn wrap_array(values []int) Tree[[]int] {
	empty := Tree[[]int](Empty{})
	return Tree[[]int](Node[[]int]{values, empty, empty})
}

fn wrap_string_array(values []string) Tree[[]string] {
	empty := Tree[[]string](Empty{})
	return Tree[[]string](Node[[]string]{values, empty, empty})
}

fn wrap_map(values map[string]int) Tree[map[string]int] {
	empty := Tree[map[string]int](Empty{})
	return Tree[map[string]int](Node[map[string]int]{values, empty, empty})
}

fn wrap_pointer(value &int) Tree[&int] {
	empty := Tree[&int](Empty{})
	return Tree[&int](Node[&int]{value, empty, empty})
}

fn make_queue(values []string) Queue[[]string] {
	return Queue[[]string]{values}
}

fn hold_option(value ?int) Holder[?int] {
	return Holder[?int]{value}
}

fn hold_result(value !int) Holder[!int] {
	return Holder[!int]{value}
}

fn main() {
	mut value := 7
	_ = wrap_array([1, 2])
	_ = wrap_string_array(["a", "b"])
	_ = wrap_map({
		"a": 1
	})
	_ = wrap_pointer(&value)
	_ = make_queue(["x", "y"])
}
'
}

fn nested_module_generic_tree_source() string {
	return '
module bar

pub struct Empty {}

pub struct Node[T] {
pub:
	value T
	left  Tree[T]
	right Tree[T]
}

pub struct Leaf {}

pub type Tree[T] = Empty | Node[T] | Leaf

pub fn wrap(values []int) Tree[[]int] {
	empty := Tree[[]int](Empty{})
	return Tree[[]int](Node[[]int]{values, empty, empty})
}

pub fn tag(tree Tree[[]int]) int {
	return match tree {
		Empty { 0 }
		Node[[]int] { 1 }
		Leaf { 2 }
	}
}
'
}

fn nested_module_generic_tree_main_source() string {
	return '
module main

import foo.bar as fb

fn hold_imported(value fb.Tree[[]int]) fb.Tree[[]int] {
	return value
}

fn main() {
	tree := fb.wrap([1, 2])
	_ = fb.tag(tree)
	_ = hold_imported(tree)
}
	'
}

fn nested_module_generic_tree_alias_foo_bar_main_source() string {
	return '
module main

import foo.bar as foo_bar

fn hold_imported(value foo_bar.Tree[[]int]) foo_bar.Tree[[]int] {
	return value
}

fn main() {
	tree := foo_bar.wrap([1, 2])
	_ = foo_bar.tag(tree)
	_ = hold_imported(tree)
}
	'
}

fn selective_import_generic_arg_foo_source() string {
	return '
module foo

pub struct Bar {
pub:
	value int
}

pub fn make_bar() Bar {
	return Bar{7}
}
'
}

fn selective_import_generic_arg_baz_source() string {
	return '
module baz

import foo { Bar }

pub struct Wrapper[T] {
pub:
	value T
}

pub struct NoneType {}

pub type Maybe[T] = Wrapper[T] | NoneType

pub fn wrap_bar(value Bar) Wrapper[Bar] {
	return Wrapper[Bar]{value}
}

pub fn hold_bar(value Wrapper[Bar]) Wrapper[Bar] {
	return value
}

pub fn make_maybe(value Bar) Maybe[Bar] {
	return Wrapper[Bar]{value}
}

pub fn is_wrapped(value Maybe[Bar]) int {
	return match value {
		Wrapper[Bar] { 1 }
		NoneType { 0 }
	}
}
'
}

fn selective_import_generic_arg_main_source() string {
	return '
module main

import baz
import foo

fn main() {
	value := foo.make_bar()
	wrapped := baz.wrap_bar(value)
	maybe := baz.make_maybe(value)
	_ = baz.hold_bar(wrapped)
	_ = baz.is_wrapped(maybe)
}
	'
}

fn string_param_interpolation_source() string {
	source := r'module main

fn render(n int, src string, dst string) string {
	return @move_inter@
}

fn main() {
	_ = render(1, @src_arg@, @dst_arg@)
}
'
	return source.replace('@move_inter@', "'Disc \${n} from \${src} to \${dst}...'").replace('@src_arg@',
		"'a'").replace('@dst_arg@', "'b'")
}

fn hanoi_string_param_interpolation_source() string {
	source := r'module main

fn println(s string) {
	_ = s
}

fn move(n int, a string, b string) {
	println(@move_inter@)
}

fn hanoi(n int, a string, b string, c string) {
	if n == 1 {
		move(n, a, c)
	} else {
		hanoi(n - 1, a, c, b)
		move(n, a, c)
		hanoi(n - 1, b, a, c)
	}
}

fn main() {
	hanoi(3, "A", "B", "C")
}
'
	return source.replace('@move_inter@', "'Disc \${n} from \${a} to \${b}...'")
}

fn tree_ssa_module_for_source(label string, source string) &Module {
	tmp_dir := os.join_path(os.temp_dir(), 'v2_tree_sumtype_${label}_${os.getpid()}')
	os.mkdir_all(tmp_dir) or { panic('cannot create ${tmp_dir}') }
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	binary_dir := os.join_path(tmp_dir, 'encoding', 'binary')
	os.mkdir_all(binary_dir) or { panic('cannot create ${binary_dir}') }
	binary_path := os.join_path(binary_dir, 'binary.v')
	os.write_file(binary_path, 'module binary

pub fn size[T](x T) int {
	_ = x
	return -1
}
') or {
		panic('cannot write ${binary_path}')
	}
	path := os.join_path(tmp_dir, 'main.v')
	os.write_file(path, source) or { panic('cannot write ${path}') }
	prefs := &pref.Preferences{
		backend: .x64
		arch:    .x64
	}
	mut file_set := token.FileSet.new()
	mut par := parser.Parser.new(prefs)
	files := par.parse_files([binary_path, path], mut file_set)
	env := types.Environment.new()
	mut checker := types.Checker.new(prefs, file_set, env)
	checker.check_files(files)
	mut trans := transformer.Transformer.new_with_pref(env, prefs)
	trans.set_file_set(file_set)
	transformed_files := trans.transform_files(files)
	mut mod := Module.new('tree_sumtype_${label}')
	mut builder := Builder.new_with_env(mod, env)
	builder.build_all(transformed_files)
	return mod
}

fn tree_ssa_module_for_source_flat(label string, source string) &Module {
	tmp_dir := os.join_path(os.temp_dir(), 'v2_tree_sumtype_flat_${label}_${os.getpid()}')
	os.mkdir_all(tmp_dir) or { panic('cannot create ${tmp_dir}') }
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	binary_dir := os.join_path(tmp_dir, 'encoding', 'binary')
	os.mkdir_all(binary_dir) or { panic('cannot create ${binary_dir}') }
	binary_path := os.join_path(binary_dir, 'binary.v')
	os.write_file(binary_path, 'module binary

pub fn size[T](x T) int {
	_ = x
	return -1
}
') or {
		panic('cannot write ${binary_path}')
	}
	path := os.join_path(tmp_dir, 'main.v')
	os.write_file(path, source) or { panic('cannot write ${path}') }
	prefs := &pref.Preferences{
		backend: .x64
		arch:    .x64
	}
	mut file_set := token.FileSet.new()
	mut par := parser.Parser.new(prefs)
	files := par.parse_files([binary_path, path], mut file_set)
	env := types.Environment.new()
	mut checker := types.Checker.new(prefs, file_set, env)
	checker.check_files(files)
	mut trans := transformer.Transformer.new_with_pref(env, prefs)
	trans.set_file_set(file_set)
	flat := ast.flatten_files(files)
	transformed_flat, _ := trans.transform_files_to_flat(&flat, files)
	mut mod := Module.new('tree_sumtype_flat_${label}')
	mut builder := Builder.new_with_env(mod, env)
	builder.build_all_from_flat(&transformed_flat)
	return mod
}

fn tree_ssa_module_for_nested_tree_source(label string, use_flat bool) &Module {
	return tree_ssa_module_for_nested_tree_main_source(label, use_flat,
		nested_module_generic_tree_main_source())
}

fn tree_ssa_module_for_nested_tree_main_source(label string, use_flat bool, main_source string) &Module {
	tmp_dir := os.join_path(os.temp_dir(), 'v2_tree_sumtype_nested_${label}_${os.getpid()}')
	os.mkdir_all(tmp_dir) or { panic('cannot create ${tmp_dir}') }
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	nested_dir := os.join_path(tmp_dir, 'foo', 'bar')
	os.mkdir_all(nested_dir) or { panic('cannot create ${nested_dir}') }
	nested_path := os.join_path(nested_dir, 'bar.v')
	os.write_file(nested_path, nested_module_generic_tree_source()) or {
		panic('cannot write ${nested_path}')
	}
	main_path := os.join_path(tmp_dir, 'main.v')
	os.write_file(main_path, main_source) or { panic('cannot write ${main_path}') }
	prefs := &pref.Preferences{
		backend: .x64
		arch:    .x64
	}
	mut file_set := token.FileSet.new()
	mut par := parser.Parser.new(prefs)
	files := par.parse_files([nested_path, main_path], mut file_set)
	env := types.Environment.new()
	mut checker := types.Checker.new(prefs, file_set, env)
	checker.check_files(files)
	mut trans := transformer.Transformer.new_with_pref(env, prefs)
	trans.set_file_set(file_set)
	mut mod := Module.new('tree_sumtype_nested_${label}')
	mut builder := Builder.new_with_env(mod, env)
	if use_flat {
		flat := ast.flatten_files(files)
		transformed_flat, _ := trans.transform_files_to_flat(&flat, files)
		builder.build_all_from_flat(&transformed_flat)
	} else {
		transformed_files := trans.transform_files(files)
		builder.build_all(transformed_files)
	}
	return mod
}

fn tree_ssa_module_for_selective_import_generic_arg(label string, use_flat bool) &Module {
	tmp_dir := os.join_path(os.temp_dir(), 'v2_tree_sumtype_selective_${label}_${os.getpid()}')
	os.mkdir_all(tmp_dir) or { panic('cannot create ${tmp_dir}') }
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	foo_dir := os.join_path(tmp_dir, 'foo')
	baz_dir := os.join_path(tmp_dir, 'baz')
	os.mkdir_all(foo_dir) or { panic('cannot create ${foo_dir}') }
	os.mkdir_all(baz_dir) or { panic('cannot create ${baz_dir}') }
	foo_path := os.join_path(foo_dir, 'foo.v')
	baz_path := os.join_path(baz_dir, 'baz.v')
	main_path := os.join_path(tmp_dir, 'main.v')
	os.write_file(foo_path, selective_import_generic_arg_foo_source()) or {
		panic('cannot write ${foo_path}')
	}
	os.write_file(baz_path, selective_import_generic_arg_baz_source()) or {
		panic('cannot write ${baz_path}')
	}
	os.write_file(main_path, selective_import_generic_arg_main_source()) or {
		panic('cannot write ${main_path}')
	}
	prefs := &pref.Preferences{
		backend: .x64
		arch:    .x64
	}
	mut file_set := token.FileSet.new()
	mut par := parser.Parser.new(prefs)
	files := par.parse_files([foo_path, baz_path, main_path], mut file_set)
	env := types.Environment.new()
	mut checker := types.Checker.new(prefs, file_set, env)
	checker.check_files(files)
	mut trans := transformer.Transformer.new_with_pref(env, prefs)
	trans.set_file_set(file_set)
	mut mod := Module.new('tree_sumtype_selective_${label}')
	mut builder := Builder.new_with_env(mod, env)
	if use_flat {
		flat := ast.flatten_files(files)
		transformed_flat, _ := trans.transform_files_to_flat(&flat, files)
		builder.build_all_from_flat(&transformed_flat)
	} else {
		transformed_files := trans.transform_files(files)
		builder.build_all(transformed_files)
	}
	return mod
}

fn tree_ssa_func(m &Module, name string) ?Function {
	for func in m.funcs {
		if func.name == name {
			return func
		}
	}
	return none
}

fn tree_ssa_call_callees(m &Module, func Function) []string {
	mut callees := []string{}
	for blk_id in func.blocks {
		for val_id in m.blocks[blk_id].instrs {
			value := m.values[val_id]
			if value.kind != .instruction {
				continue
			}
			instr := m.instrs[value.index]
			if instr.op != .call || instr.operands.len == 0 {
				continue
			}
			callee_id := instr.operands[0]
			if callee_id <= 0 || callee_id >= m.values.len {
				continue
			}
			callee := m.values[callee_id]
			if callee.kind == .func_ref {
				callees << callee.name
			}
		}
	}
	return callees
}

fn tree_ssa_type_name(m &Module, typ TypeID) string {
	if typ <= 0 || int(typ) >= m.type_store.types.len {
		return ''
	}
	return m.c_struct_names[int(typ)] or { '' }
}

fn tree_ssa_type_id_by_name(m &Module, type_name string) ?TypeID {
	for type_id, name in m.c_struct_names {
		if ssa_type_name_matches(name, type_name) {
			return TypeID(type_id)
		}
	}
	return none
}

fn tree_ssa_type_id_by_exact_name(m &Module, type_name string) ?TypeID {
	for type_id, name in m.c_struct_names {
		if name == type_name {
			return TypeID(type_id)
		}
	}
	return none
}

fn tree_ssa_type_is_i64(m &Module, typ TypeID) bool {
	if typ <= 0 || int(typ) >= m.type_store.types.len {
		return false
	}
	info := m.type_store.types[typ]
	return info.kind == .int_t && info.width == 64 && !info.is_unsigned
}

fn tree_ssa_is_const_zero(m &Module, val_id ValueID) bool {
	if val_id <= 0 || val_id >= m.values.len {
		return false
	}
	return m.values[val_id].kind == .constant && m.values[val_id].name == '0'
}

fn tree_ssa_const_name(m &Module, val_id ValueID) string {
	if val_id <= 0 || val_id >= m.values.len || m.values[val_id].kind != .constant {
		return ''
	}
	return m.values[val_id].name
}

fn tree_ssa_value_points_to_type(m &Module, val_id ValueID, type_name string) bool {
	if val_id <= 0 || val_id >= m.values.len {
		return false
	}
	typ := m.values[val_id].typ
	if typ <= 0 || int(typ) >= m.type_store.types.len {
		return false
	}
	info := m.type_store.types[typ]
	if info.kind != .ptr_t {
		return false
	}
	return ssa_type_name_matches(tree_ssa_type_name(m, info.elem_type), type_name)
}

fn tree_ssa_has_sumtype_tag_extract(m &Module, func Function, type_name string) bool {
	return tree_ssa_sumtype_tag_value_ids(m, func, type_name).len > 0
}

fn tree_ssa_sumtype_tag_value_ids(m &Module, func Function, type_name string) []ValueID {
	mut sumtype_storage := []ValueID{}
	for blk_id in func.blocks {
		for val_id in m.blocks[blk_id].instrs {
			value := m.values[val_id]
			if value.kind != .instruction {
				continue
			}
			instr := m.instrs[value.index]
			if instr.op != .store || instr.operands.len < 2 {
				continue
			}
			src_id := instr.operands[0]
			dst_id := instr.operands[1]
			if src_id <= 0 || src_id >= m.values.len || dst_id <= 0 || dst_id >= m.values.len {
				continue
			}
			src_type_name := tree_ssa_type_name(m, m.values[src_id].typ)
			if ssa_type_name_matches(src_type_name, type_name) && dst_id !in sumtype_storage {
				sumtype_storage << dst_id
			}
		}
	}
	mut tag_ptrs := []ValueID{}
	mut tag_vals := []ValueID{}
	for blk_id in func.blocks {
		for val_id in m.blocks[blk_id].instrs {
			value := m.values[val_id]
			if value.kind != .instruction {
				continue
			}
			instr := m.instrs[value.index]
			if instr.op == .extractvalue && instr.operands.len >= 2 {
				base_id := instr.operands[0]
				index_id := instr.operands[1]
				if base_id <= 0 || base_id >= m.values.len || !tree_ssa_is_const_zero(m, index_id) {
					continue
				}
				base_type_name := tree_ssa_type_name(m, m.values[base_id].typ)
				if ssa_type_name_matches(base_type_name, type_name) {
					tag_vals << val_id
				}
			}
			if instr.op == .get_element_ptr && instr.operands.len >= 2 {
				base_id := instr.operands[0]
				index_id := instr.operands[1]
				if !tree_ssa_is_const_zero(m, index_id) {
					continue
				}
				if base_id in sumtype_storage
					|| tree_ssa_value_points_to_type(m, base_id, type_name) {
					tag_ptrs << val_id
				}
			}
			if instr.op == .load && instr.operands.len > 0 {
				ptr_id := instr.operands[0]
				if ptr_id in tag_ptrs {
					tag_vals << val_id
				}
			}
		}
	}
	return tag_vals
}

fn tree_ssa_sumtype_tag_compare_const_names(m &Module, func Function, type_name string) []string {
	tag_vals := tree_ssa_sumtype_tag_value_ids(m, func, type_name)
	mut names := []string{}
	for blk_id in func.blocks {
		for val_id in m.blocks[blk_id].instrs {
			value := m.values[val_id]
			if value.kind != .instruction {
				continue
			}
			instr := m.instrs[value.index]
			if instr.op !in [.eq, .ne] || instr.operands.len < 2 {
				continue
			}
			lhs := instr.operands[0]
			rhs := instr.operands[1]
			if lhs in tag_vals && rhs > 0 && rhs < m.values.len && m.values[rhs].kind == .constant {
				names << m.values[rhs].name
			}
			if rhs in tag_vals && lhs > 0 && lhs < m.values.len && m.values[lhs].kind == .constant {
				names << m.values[lhs].name
			}
		}
	}
	return names
}

fn tree_ssa_has_eq_on_type(m &Module, func Function, type_name string) bool {
	for blk_id in func.blocks {
		for val_id in m.blocks[blk_id].instrs {
			value := m.values[val_id]
			if value.kind != .instruction {
				continue
			}
			instr := m.instrs[value.index]
			if instr.op != .eq {
				continue
			}
			for operand in instr.operands {
				if operand <= 0 || operand >= m.values.len {
					continue
				}
				operand_type_name := tree_ssa_type_name(m, m.values[operand].typ)
				if ssa_type_name_matches(operand_type_name, type_name) {
					return true
				}
			}
		}
	}
	return false
}

fn tree_ssa_assert_sumtype_match_uses_tag(m &Module, func Function, type_name string) {
	assert tree_ssa_has_sumtype_tag_extract(m, func, type_name)
	assert !tree_ssa_has_eq_on_type(m, func, type_name)
	tag_consts := tree_ssa_sumtype_tag_compare_const_names(m, func, type_name)
	assert '0' in tag_consts
	assert '1' in tag_consts
}

fn tree_ssa_assert_no_raw_tree_string_concat_arg(m &Module, func Function) {
	string_concat_callees := ['builtin__string__+', 'builtin__string__plus_two']
	for blk_id in func.blocks {
		for val_id in m.blocks[blk_id].instrs {
			value := m.values[val_id]
			if value.kind != .instruction {
				continue
			}
			instr := m.instrs[value.index]
			if instr.op != .call || instr.operands.len < 2 {
				continue
			}
			callee_id := instr.operands[0]
			if callee_id <= 0 || callee_id >= m.values.len {
				continue
			}
			callee := m.values[callee_id]
			if callee.kind != .func_ref || callee.name !in string_concat_callees {
				continue
			}
			for operand in instr.operands[1..] {
				if operand <= 0 || operand >= m.values.len {
					continue
				}
				operand_type := m.values[operand].typ
				operand_type_name := tree_ssa_type_name(m, operand_type)
				assert !ssa_type_name_matches(operand_type_name, 'Node'), '${func.name} passes raw Node v${operand} to ${callee.name}'

				assert !ssa_type_name_matches(operand_type_name, 'Tree'), '${func.name} passes raw Tree v${operand} to ${callee.name}'
			}
		}
	}
}

fn tree_ssa_assert_string_param_interpolation_has_no_string_str(m &Module) {
	tree_ssa_assert_func_has_no_string_str(m, 'render')
}

fn tree_ssa_assert_func_has_no_string_str(m &Module, name string) {
	func := tree_ssa_func(m, name) or { panic('missing ${name}') }
	callees := tree_ssa_call_callees(m, func)
	assert 'builtin__string__str' !in callees
	assert 'string__str' !in callees
	assert 'builtin__string__+' in callees || 'builtin__string__plus_two' in callees
}

fn tree_ssa_call_arg_type_names(m &Module, func Function, callee_name string) []string {
	mut names := []string{}
	for blk_id in func.blocks {
		for val_id in m.blocks[blk_id].instrs {
			value := m.values[val_id]
			if value.kind != .instruction {
				continue
			}
			instr := m.instrs[value.index]
			if instr.op != .call || instr.operands.len < 2 {
				continue
			}
			callee_id := instr.operands[0]
			if callee_id <= 0 || callee_id >= m.values.len {
				continue
			}
			callee := m.values[callee_id]
			if callee.kind != .func_ref || callee.name != callee_name {
				continue
			}
			arg_id := instr.operands[1]
			if arg_id <= 0 || arg_id >= m.values.len {
				continue
			}
			names << tree_ssa_type_name(m, m.values[arg_id].typ)
		}
	}
	return names
}

fn test_tree_sumtype_main_calls_local_size_not_generic_size() {
	m := tree_ssa_module_for_source('local_size', tree_sumtype_source())
	main_func := tree_ssa_func(m, 'main') or { panic('missing main') }
	size_func := tree_ssa_func(m, 'size') or { panic('missing size') }
	main_callees := tree_ssa_call_callees(m, main_func)
	size_callees := tree_ssa_call_callees(m, size_func)
	assert 'size' in main_callees
	assert 'size_T_Type' !in main_callees
	assert 'binary__size_value_T_Type' !in main_callees
	assert 'size' in size_callees
	assert 'size_T_Type' !in size_callees
	tree_ssa_assert_sumtype_match_uses_tag(m, size_func, 'Tree')
	size_arg_types := tree_ssa_call_arg_type_names(m, main_func, 'size')
	assert 'Tree' in size_arg_types
	assert 'Node' !in size_arg_types
}

fn test_tree_sumtype_variant_struct_fields_keep_sumtype_storage_type() {
	m := tree_ssa_module_for_source('variant_field_storage', tree_sumtype_source())
	node_type := tree_ssa_type_id_by_name(m, 'Node') or { panic('missing Node type') }
	node_info := m.type_store.types[node_type]
	left_idx := node_info.field_names.index('left')
	right_idx := node_info.field_names.index('right')
	assert left_idx >= 0
	assert right_idx >= 0
	assert ssa_type_name_matches(tree_ssa_type_name(m, node_info.fields[left_idx]), 'Tree')
	assert ssa_type_name_matches(tree_ssa_type_name(m, node_info.fields[right_idx]), 'Tree')
}

fn tree_ssa_assert_variant_struct_init_wraps_sumtype_fields(m &Module) {
	main_func := tree_ssa_func(m, 'main') or { panic('missing main') }
	mut saw_root_node_init := false
	for blk_id in main_func.blocks {
		for val_id in m.blocks[blk_id].instrs {
			value := m.values[val_id]
			if value.kind != .instruction {
				continue
			}
			instr := m.instrs[value.index]
			if instr.op != .struct_init || instr.operands.len < 3 {
				continue
			}
			if !ssa_type_name_matches(tree_ssa_type_name(m, value.typ), 'Node') {
				continue
			}
			if tree_ssa_const_name(m, instr.operands[0]) != '10' {
				continue
			}
			saw_root_node_init = true
			assert ssa_type_name_matches(tree_ssa_type_name(m, m.values[instr.operands[1]].typ),
				'Tree')
			assert ssa_type_name_matches(tree_ssa_type_name(m, m.values[instr.operands[2]].typ),
				'Tree')
		}
	}
	assert saw_root_node_init
}

fn tree_ssa_value_mentions_type(m &Module, val_id ValueID, type_name string, depth int) bool {
	if depth <= 0 || val_id <= 0 || val_id >= m.values.len {
		return false
	}
	val := m.values[val_id]
	if ssa_type_name_matches(tree_ssa_type_name(m, val.typ), type_name)
		|| tree_ssa_value_points_to_type(m, val_id, type_name) {
		return true
	}
	if val.kind != .instruction {
		return false
	}
	instr := m.instrs[val.index]
	for operand in instr.operands {
		if tree_ssa_value_mentions_type(m, operand, type_name, depth - 1) {
			return true
		}
	}
	return false
}

fn tree_ssa_assert_generic_sumtype_direct_wrap(m &Module) {
	tree_type := tree_ssa_type_id_by_name(m, 'Tree_T_f64') or {
		panic('missing Tree_T_f64 SSA type')
	}
	node_type := tree_ssa_type_id_by_name(m, 'Node_T_f64') or {
		panic('missing Node_T_f64 SSA type')
	}
	tree_info := m.type_store.types[tree_type]
	assert tree_info.kind == .struct_t
	assert tree_info.field_names == ['_tag', '_data']
	node_info := m.type_store.types[node_type]
	left_idx := node_info.field_names.index('left')
	right_idx := node_info.field_names.index('right')
	assert left_idx >= 0
	assert right_idx >= 0
	left_type := node_info.fields[left_idx]
	right_type := node_info.fields[right_idx]
	assert ssa_type_name_matches(tree_ssa_type_name(m, left_type), 'Tree_T_f64')
	assert ssa_type_name_matches(tree_ssa_type_name(m, right_type), 'Tree_T_f64')
	main_func := tree_ssa_func(m, 'main') or { panic('missing main') }
	mut saw_empty_tag := false
	mut saw_node_tag := false
	mut saw_node_payload := false
	for blk_id in main_func.blocks {
		for val_id in m.blocks[blk_id].instrs {
			value := m.values[val_id]
			if value.kind != .instruction || value.typ != tree_type {
				continue
			}
			instr := m.instrs[value.index]
			if instr.op != .struct_init || instr.operands.len < 2 {
				continue
			}
			tag_name := tree_ssa_const_name(m, instr.operands[0])
			if tag_name == '0' {
				saw_empty_tag = true
			}
			if tag_name == '1' {
				saw_node_tag = true
				data_id := instr.operands[1]
				saw_node_payload = data_id > 0 && data_id < m.values.len
					&& tree_ssa_const_name(m, data_id) != '0'
					&& tree_ssa_value_mentions_type(m, data_id, 'Node_T_f64', 8)
			}
		}
	}
	assert saw_empty_tag, 'main missing Tree_T_f64 Empty wrapper with _tag 0'
	assert saw_node_tag, 'main missing Tree_T_f64 Node_T_f64 wrapper with _tag 1'
	assert saw_node_payload, 'main missing Tree_T_f64 Node_T_f64 payload'
	tag_func := tree_ssa_func(m, 'tag') or { panic('missing tag') }
	tree_ssa_assert_sumtype_match_uses_tag(m, tag_func, 'Tree_T_f64')
}

fn tree_ssa_assert_generic_receiver_size_match(m &Module) {
	size_func := tree_ssa_func(m, 'Tree_T_f64__size_T_f64') or {
		panic('missing Tree_T_f64__size_T_f64')
	}
	tree_ssa_assert_sumtype_match_uses_tag(m, size_func, 'Tree_T_f64')
	tag_consts := tree_ssa_sumtype_tag_compare_const_names(m, size_func, 'Tree_T_f64')
	assert '0' in tag_consts
	assert '1' in tag_consts
}

fn tree_ssa_assert_generic_insert_wraps_node_return(m &Module) {
	tree_type := tree_ssa_type_id_by_name(m, 'Tree_T_f64') or {
		panic('missing Tree_T_f64 SSA type')
	}
	insert_func := tree_ssa_func(m, 'Tree_T_f64__insert_T_f64') or {
		panic('missing Tree_T_f64__insert_T_f64')
	}
	mut node_wrap_count := 0
	mut saw_node_tag := false
	mut saw_node_payload := false
	for blk_id in insert_func.blocks {
		for val_id in m.blocks[blk_id].instrs {
			value := m.values[val_id]
			if value.kind != .instruction || value.typ != tree_type {
				continue
			}
			instr := m.instrs[value.index]
			if instr.op != .struct_init || instr.operands.len < 2 {
				continue
			}
			if tree_ssa_const_name(m, instr.operands[0]) != '1' {
				continue
			}
			node_wrap_count++
			saw_node_tag = true
			data_id := instr.operands[1]
			if data_id > 0 && data_id < m.values.len
				&& tree_ssa_value_mentions_type(m, data_id, 'Node_T_f64', 8) {
				saw_node_payload = true
			}
		}
	}
	assert saw_node_tag, 'insert missing Tree_T_f64 Node_T_f64 return wrapper with _tag 1'
	assert saw_node_payload, 'insert missing Tree_T_f64 Node_T_f64 return payload'
	assert node_wrap_count >= 3, 'insert should wrap Empty plus both recursive Node branches with tag 1; got ${node_wrap_count}'
}

fn tree_ssa_assert_func_return_type(m &Module, func_name string, type_name string) {
	func := tree_ssa_func(m, func_name) or { panic('missing ${func_name}') }
	actual_name := tree_ssa_type_name(m, func.typ)
	assert ssa_type_name_matches(actual_name, type_name), '${func_name} return type: expected ${type_name}, got ${actual_name}'
	assert !tree_ssa_type_is_i64(m, func.typ), '${func_name} fell back to i64 instead of ${type_name}'
}

fn tree_ssa_assert_func_return_type_exact(m &Module, func_name string, type_name string) {
	func := tree_ssa_func(m, func_name) or { panic('missing ${func_name}') }
	actual_name := tree_ssa_type_name(m, func.typ)
	assert actual_name == type_name, '${func_name} return type: expected exact ${type_name}, got ${actual_name}'
	assert !tree_ssa_type_is_i64(m, func.typ), '${func_name} fell back to i64 instead of ${type_name}'
}

fn tree_ssa_assert_generic_nested_type_args(m &Module) {
	expected_types := [
		'Tree_T_Array_int',
		'Node_T_Array_int',
		'Tree_T_Array_string',
		'Node_T_Array_string',
		'Tree_T_Map_string_int',
		'Node_T_Map_string_int',
		'Tree_T_intptr',
		'Node_T_intptr',
		'Queue_T_Array_string',
		'Holder_T_Option_int',
		'Holder_T_Result_int',
	]
	for type_name in expected_types {
		_ = tree_ssa_type_id_by_name(m, type_name) or { panic('missing ${type_name}') }
	}
	tree_ssa_assert_func_return_type(m, 'wrap_array', 'Tree_T_Array_int')
	tree_ssa_assert_func_return_type(m, 'wrap_string_array', 'Tree_T_Array_string')
	tree_ssa_assert_func_return_type(m, 'wrap_map', 'Tree_T_Map_string_int')
	tree_ssa_assert_func_return_type(m, 'wrap_pointer', 'Tree_T_intptr')
	tree_ssa_assert_func_return_type(m, 'make_queue', 'Queue_T_Array_string')
	tree_ssa_assert_func_return_type(m, 'hold_option', 'Holder_T_Option_int')
	tree_ssa_assert_func_return_type(m, 'hold_result', 'Holder_T_Result_int')
}

fn tree_ssa_assert_nested_module_generic_tree(m &Module) {
	expected_types := [
		'bar__Tree_T_Array_int',
		'bar__Node_T_Array_int',
	]
	for type_name in expected_types {
		_ = tree_ssa_type_id_by_exact_name(m, type_name) or {
			panic('missing exact ${type_name} in ${m.c_struct_names}')
		}
	}
	assert tree_ssa_type_id_by_exact_name(m, 'bar__int') == none
	assert tree_ssa_type_id_by_exact_name(m, 'bar__Array_int') == none
	tree_ssa_assert_func_return_type_exact(m, 'bar__wrap', 'bar__Tree_T_Array_int')
	tree_ssa_assert_func_return_type_exact(m, 'hold_imported', 'bar__Tree_T_Array_int')
	tag_func := tree_ssa_func(m, 'bar__tag') or { panic('missing bar__tag') }
	tree_ssa_assert_sumtype_match_uses_tag(m, tag_func, 'bar__Tree_T_Array_int')
	assert tree_ssa_type_id_by_exact_name(m, 'nested_tree__Tree_T_Array_int') == none
	assert tree_ssa_type_id_by_exact_name(m, 'nested_tree__Node_T_Array_int') == none
	assert tree_ssa_type_id_by_exact_name(m, 'foo_bar__Tree_T_Array_int') == none
	assert tree_ssa_type_id_by_exact_name(m, 'foo_bar__Node_T_Array_int') == none
	assert tree_ssa_func(m, 'nested_tree__wrap') == none
	assert tree_ssa_func(m, 'nested_tree__tag') == none
	assert tree_ssa_func(m, 'foo_bar__wrap') == none
	assert tree_ssa_func(m, 'foo_bar__tag') == none
}

fn tree_ssa_assert_selective_import_generic_arg(m &Module) {
	_ = tree_ssa_type_id_by_exact_name(m, 'foo__Bar') or { panic('missing exact foo__Bar') }
	_ = tree_ssa_type_id_by_exact_name(m, 'baz__Wrapper_T_foo_Bar') or {
		panic('missing exact baz__Wrapper_T_foo_Bar in ${m.c_struct_names}')
	}
	_ = tree_ssa_type_id_by_exact_name(m, 'baz__Maybe_T_foo_Bar') or {
		panic('missing exact baz__Maybe_T_foo_Bar in ${m.c_struct_names}')
	}
	assert tree_ssa_type_id_by_exact_name(m, 'baz__Wrapper_T_baz__Bar') == none
	assert tree_ssa_type_id_by_exact_name(m, 'baz__Wrapper_T_baz_Bar') == none
	assert tree_ssa_type_id_by_exact_name(m, 'baz__Maybe_T_baz__Bar') == none
	assert tree_ssa_type_id_by_exact_name(m, 'baz__Maybe_T_baz_Bar') == none
	assert tree_ssa_type_id_by_exact_name(m, 'baz__Bar') == none
	tree_ssa_assert_func_return_type_exact(m, 'baz__wrap_bar', 'baz__Wrapper_T_foo_Bar')
	tree_ssa_assert_func_return_type_exact(m, 'baz__hold_bar', 'baz__Wrapper_T_foo_Bar')
	tree_ssa_assert_func_return_type_exact(m, 'baz__make_maybe', 'baz__Maybe_T_foo_Bar')
	is_wrapped_func := tree_ssa_func(m, 'baz__is_wrapped') or { panic('missing baz__is_wrapped') }
	tree_ssa_assert_sumtype_match_uses_tag(m, is_wrapped_func, 'baz__Maybe_T_foo_Bar')
}

fn test_tree_sumtype_variant_struct_init_wraps_sumtype_fields() {
	m := tree_ssa_module_for_source('variant_field_init', tree_sumtype_source())
	tree_ssa_assert_variant_struct_init_wraps_sumtype_fields(m)
}

fn test_tree_sumtype_flat_variant_struct_init_wraps_sumtype_fields() {
	m := tree_ssa_module_for_source_flat('flat_variant_field_init', tree_sumtype_source())
	tree_ssa_assert_variant_struct_init_wraps_sumtype_fields(m)
}

fn test_generic_tree_sumtype_direct_wrap_uses_node_tag_on_legacy_ast() {
	m := tree_ssa_module_for_source('generic_direct_wrap',
		generic_tree_sumtype_direct_wrap_source())
	tree_ssa_assert_generic_sumtype_direct_wrap(m)
}

fn test_generic_tree_sumtype_flat_direct_wrap_uses_node_tag() {
	m := tree_ssa_module_for_source_flat('generic_direct_wrap',
		generic_tree_sumtype_direct_wrap_source())
	tree_ssa_assert_generic_sumtype_direct_wrap(m)
}

fn test_generic_tree_sumtype_receiver_size_uses_node_tag_on_legacy_ast() {
	m := tree_ssa_module_for_source('generic_receiver_size',
		generic_tree_sumtype_receiver_size_source())
	tree_ssa_assert_generic_receiver_size_match(m)
}

fn test_generic_tree_sumtype_flat_receiver_size_uses_node_tag() {
	m := tree_ssa_module_for_source_flat('generic_receiver_size',
		generic_tree_sumtype_receiver_size_source())
	tree_ssa_assert_generic_receiver_size_match(m)
}

fn test_generic_tree_sumtype_insert_return_wraps_node_on_legacy_ast() {
	m := tree_ssa_module_for_source('generic_insert_size',
		generic_tree_sumtype_insert_size_source())
	tree_ssa_assert_generic_insert_wraps_node_return(m)
}

fn test_generic_tree_sumtype_flat_insert_return_wraps_node() {
	m := tree_ssa_module_for_source_flat('generic_insert_size',
		generic_tree_sumtype_insert_size_source())
	tree_ssa_assert_generic_insert_wraps_node_return(m)
}

fn test_generic_tree_sumtype_nested_type_args_do_not_fall_back_to_i64_on_legacy_ast() {
	m := tree_ssa_module_for_source('generic_nested_type_args',
		generic_tree_nested_type_args_source())
	tree_ssa_assert_generic_nested_type_args(m)
}

fn test_generic_tree_sumtype_flat_nested_type_args_do_not_fall_back_to_i64() {
	m := tree_ssa_module_for_source_flat('generic_nested_type_args',
		generic_tree_nested_type_args_source())
	tree_ssa_assert_generic_nested_type_args(m)
}

fn test_nested_module_generic_tree_sumtype_uses_prefixed_names_on_legacy_ast() {
	m := tree_ssa_module_for_nested_tree_source('nested_generic_tree', false)
	tree_ssa_assert_nested_module_generic_tree(m)
}

fn test_nested_module_generic_tree_sumtype_flat_uses_prefixed_names() {
	m := tree_ssa_module_for_nested_tree_source('nested_generic_tree', true)
	tree_ssa_assert_nested_module_generic_tree(m)
}

fn test_nested_module_generic_tree_sumtype_alias_foo_bar_not_ambiguous_on_legacy_ast() {
	m := tree_ssa_module_for_nested_tree_main_source('nested_generic_tree_alias_foo_bar', false,
		nested_module_generic_tree_alias_foo_bar_main_source())
	tree_ssa_assert_nested_module_generic_tree(m)
}

fn test_nested_module_generic_tree_sumtype_alias_foo_bar_flat_not_ambiguous() {
	m := tree_ssa_module_for_nested_tree_main_source('nested_generic_tree_alias_foo_bar', true,
		nested_module_generic_tree_alias_foo_bar_main_source())
	tree_ssa_assert_nested_module_generic_tree(m)
}

fn test_selective_import_generic_arg_uses_resolved_type_module_on_legacy_ast() {
	m := tree_ssa_module_for_selective_import_generic_arg('selective_generic_arg', false)
	tree_ssa_assert_selective_import_generic_arg(m)
}

fn test_selective_import_generic_arg_flat_uses_resolved_type_module() {
	m := tree_ssa_module_for_selective_import_generic_arg('selective_generic_arg', true)
	tree_ssa_assert_selective_import_generic_arg(m)
}

fn test_foo_bar_imported_generic_sumtype_resolver_uses_dotted_checker_scope() {
	mut imported_scope := types.new_scope(unsafe { nil })
	imported_scope.insert_type('Tree', types.Type(types.SumType{
		name:           'foo.bar__Tree'
		generic_params: ['T']
	}))
	mut flat_scope := types.new_scope(unsafe { nil })
	flat_scope.insert_type('Tree', types.Type(types.SumType{
		name:           'foo_bar__Tree'
		generic_params: ['T']
	}))
	mut env := types.Environment.new()
	lock env.scopes {
		env.scopes['foo.bar'] = imported_scope
		env.scopes['foo_bar'] = flat_scope
	}
	mut mod := Module.new('foo_bar_imported_generic_sumtype')
	mut builder := Builder.new_with_env(mod, env)
	builder.cur_module = 'main'
	builder.module_import_aliases = {
		'bar': 'foo.bar'
	}

	base_name := builder.generic_selector_type_part_name_from_flat('bar', 'Tree')
	assert base_name == 'foo_bar__Tree'
	checked_type := builder.lookup_checked_type_by_name(base_name) or {
		panic('missing checker type for ${base_name}')
	}
	assert types.type_name(checked_type) == 'foo.bar__Tree'
	type_id := builder.generic_sumtype_storage_to_ssa(base_name, 'foo_bar__Tree_T_Array_int') or {
		panic('missing foo_bar__Tree_T_Array_int storage')
	}
	assert type_id != builder.mod.type_store.get_int(64)
	assert builder.mod.c_struct_names[type_id] == 'foo_bar__Tree_T_Array_int'
}

fn test_dotted_checker_type_lookup_rejects_ambiguous_explicit_imports() {
	mut dotted_scope := types.new_scope(unsafe { nil })
	dotted_scope.insert_type('Tree', types.Type(types.SumType{
		name:           'foo.bar__Tree'
		generic_params: ['T']
	}))
	mut flat_scope := types.new_scope(unsafe { nil })
	flat_scope.insert_type('Tree', types.Type(types.SumType{
		name:           'foo_bar__Tree'
		generic_params: ['T']
	}))
	mut env := types.Environment.new()
	lock env.scopes {
		env.scopes['foo.bar'] = dotted_scope
		env.scopes['foo_bar'] = flat_scope
	}
	mut mod := Module.new('ambiguous_import_type_lookup')
	mut builder := Builder.new_with_env(mod, env)
	builder.module_import_aliases = {
		'bar':  'foo.bar'
		'flat': 'foo_bar'
	}

	_ := builder.lookup_checked_type_by_name('foo_bar__Tree') or { return }
	assert false, 'ambiguous explicit imports should not pick foo.bar or foo_bar'
}

fn test_selectively_imported_type_generic_arg_uses_resolved_type_module() {
	mut current_scope := types.new_scope(unsafe { nil })
	current_scope.insert_type('Bar', types.Type(types.Struct{
		name: 'foo__Bar'
	}))
	current_scope.insert_type('Wrapper', types.Type(types.Struct{
		name: 'baz__Wrapper'
	}))
	mut env := types.Environment.new()
	lock env.scopes {
		env.scopes['baz'] = current_scope
	}
	mut mod := Module.new('selective_import_generic_arg')
	mut builder := Builder.new_with_env(mod, env)
	builder.cur_module = 'baz'

	base_part := builder.generic_ident_type_part_name('Wrapper')
	arg_part := builder.generic_type_arg_part_name(ast.Expr(ast.Ident{
		name: 'Bar'
	}))
	assert base_part == 'baz__Wrapper'
	assert arg_part == 'foo_Bar'
	assert generic_type_name_from_parts(base_part, [arg_part]) == 'baz__Wrapper_T_foo_Bar'
	assert generic_type_name_from_parts(base_part, [arg_part]) != 'baz__Wrapper_T_baz__Bar'
	assert generic_type_name_from_parts(base_part, [arg_part]) != 'baz__Wrapper_T_baz_Bar'
}

fn test_generic_suffix_fallback_rejects_ambiguous_registered_types() {
	mut mod := Module.new('generic_suffix_collision')
	mut builder := Builder.new_with_env(mod, types.Environment.new())
	first := builder.mod.type_store.register(Type{
		kind: .struct_t
	})
	second := builder.mod.type_store.register(Type{
		kind: .struct_t
	})
	builder.struct_types['first__Tree_T_Array_int'] = first
	builder.struct_types['second__Tree_T_Array_int'] = second
	_ := builder.registered_type_by_unique_concrete_suffix('nested_tree__Tree_T_Array_int') or {
		return
	}
	assert false, 'ambiguous generic suffix fallback should not choose an arbitrary type'
}

fn test_tree_sumtype_interpolation_uses_autostr_not_raw_struct_string() {
	m := tree_ssa_module_for_source('node_str', tree_sumtype_source())
	main_func := tree_ssa_func(m, 'main') or { panic('missing main') }
	main_callees := tree_ssa_call_callees(m, main_func)
	main_uses_autostr := 'Tree__str' in main_callees || 'Node__str' in main_callees
	assert main_uses_autostr
	if 'Tree__str' in main_callees {
		tree_str_callees := tree_ssa_call_callees(m, tree_ssa_func(m, 'Tree__str') or {
			panic('missing Tree__str')
		})
		assert 'Empty__str' in tree_str_callees
		assert 'Node__str' in tree_str_callees
	}
	assert 'Tree__str' in tree_ssa_call_callees(m, tree_ssa_func(m, 'Node__str') or {
		panic('missing Node__str')
	})
	tree_ssa_assert_no_raw_tree_string_concat_arg(m, main_func)
}

fn test_tree_sumtype_example_interpolation_uses_node_str_and_tree_str_helpers() {
	m := tree_ssa_module_for_source('example_node_str', tree_sumtype_example_source())
	main_func := tree_ssa_func(m, 'main') or { panic('missing main') }
	main_callees := tree_ssa_call_callees(m, main_func)
	assert 'Node__str' in main_callees
	node_str_callees := tree_ssa_call_callees(m, tree_ssa_func(m, 'Node__str') or {
		panic('missing Node__str')
	})
	assert 'Tree__str' in node_str_callees
	tree_str_callees := tree_ssa_call_callees(m, tree_ssa_func(m, 'Tree__str') or {
		panic('missing Tree__str')
	})
	assert 'Empty__str' in tree_str_callees
	assert 'Node__str' in tree_str_callees
	tree_ssa_assert_no_raw_tree_string_concat_arg(m, main_func)
}

fn test_tree_sumtype_flat_match_uses_tag_and_autostr_helpers() {
	m := tree_ssa_module_for_source_flat('tree_pipeline', tree_sumtype_source())
	main_func := tree_ssa_func(m, 'main') or { panic('missing main') }
	size_func := tree_ssa_func(m, 'size') or { panic('missing size') }
	tree_ssa_assert_sumtype_match_uses_tag(m, size_func, 'Tree')
	main_callees := tree_ssa_call_callees(m, main_func)
	main_uses_autostr := 'Tree__str' in main_callees || 'Node__str' in main_callees
	assert main_uses_autostr
	assert 'Tree__str' in tree_ssa_call_callees(m, tree_ssa_func(m, 'Node__str') or {
		panic('missing Node__str')
	})
	tree_ssa_assert_no_raw_tree_string_concat_arg(m, main_func)
}

fn test_string_parameter_interpolation_does_not_call_string_str() {
	m := tree_ssa_module_for_source('string_param_interpolation',
		string_param_interpolation_source())
	tree_ssa_assert_string_param_interpolation_has_no_string_str(m)
}

fn test_flat_string_parameter_interpolation_does_not_call_string_str() {
	m := tree_ssa_module_for_source_flat('flat_string_param_interpolation',
		string_param_interpolation_source())
	tree_ssa_assert_string_param_interpolation_has_no_string_str(m)
}

fn test_hanoi_string_parameter_interpolation_does_not_call_string_str() {
	m := tree_ssa_module_for_source('hanoi_string_param_interpolation',
		hanoi_string_param_interpolation_source())
	tree_ssa_assert_func_has_no_string_str(m, 'move')
}

fn test_flat_hanoi_string_parameter_interpolation_does_not_call_string_str() {
	m := tree_ssa_module_for_source_flat('flat_hanoi_string_param_interpolation',
		hanoi_string_param_interpolation_source())
	tree_ssa_assert_func_has_no_string_str(m, 'move')
}

fn test_tree_sumtype_convert_to_string_uses_registered_autostr_helper() {
	mut mod := Module.new('tree_three_field_struct')
	mut builder := Builder.new_with_env(mod, types.Environment.new())
	i8_type := builder.mod.type_store.get_int(8)
	ptr_i8_type := builder.mod.type_store.get_ptr(i8_type)
	int_type := builder.mod.type_store.get_int(32)
	string_type := builder.mod.type_store.register(Type{
		kind:        .struct_t
		fields:      [ptr_i8_type, int_type, int_type]
		field_names: ['str', 'len', 'is_lit']
	})
	builder.struct_types['string'] = string_type
	builder.mod.c_struct_names[int(string_type)] = 'string'
	node_type := builder.mod.type_store.register(Type{
		kind:        .struct_t
		fields:      [int_type, string_type, string_type]
		field_names: ['value', 'left', 'right']
	})
	builder.mod.c_struct_names[int(node_type)] = 'Node'
	node_val := builder.mod.add_value_node(.argument, node_type, 'node', 0)
	node_str_fn := builder.mod.new_function('Node__str', string_type, []TypeID{})
	builder.fn_index['Node__str'] = node_str_fn
	test_fn := builder.mod.new_function('test_convert_to_string', string_type, []TypeID{})
	builder.cur_func = test_fn
	builder.cur_block = builder.mod.add_block(test_fn, 'entry')
	converted := builder.convert_to_string(node_val, node_type)
	assert converted != node_val
	assert builder.mod.values[converted].typ == string_type
	assert builder.mod.values[converted].kind == .instruction
	instr := builder.mod.instrs[builder.mod.values[converted].index]
	assert instr.op == .call
	assert instr.operands.len > 0
	callee := builder.mod.values[instr.operands[0]]
	assert callee.kind == .func_ref
	assert callee.name == 'Node__str'
}

fn test_convert_to_string_treats_string_struct_names_as_already_string() {
	for type_name in ['string', 'builtin__string'] {
		mut mod := Module.new('string_alias_${type_name}')
		mut builder := Builder.new_with_env(mod, types.Environment.new())
		i8_type := builder.mod.type_store.get_int(8)
		ptr_i8_type := builder.mod.type_store.get_ptr(i8_type)
		int_type := builder.mod.type_store.get_int(32)
		string_type := builder.mod.type_store.register(Type{
			kind:        .struct_t
			fields:      [ptr_i8_type, int_type, int_type]
			field_names: ['str', 'len', 'is_lit']
		})
		builder.mod.c_struct_names[int(string_type)] = type_name
		string_val := builder.mod.add_value_node(.argument, string_type, 's', 0)
		str_fn := builder.mod.new_function('${type_name}__str', string_type, [
			string_type,
		])
		builder.fn_index['${type_name}__str'] = str_fn
		test_fn := builder.mod.new_function('test_convert_to_string_${type_name}', string_type,
			[]TypeID{})
		builder.cur_func = test_fn
		builder.cur_block = builder.mod.add_block(test_fn, 'entry')

		assert builder.is_string_struct_type(string_type)
		str_fn_name := builder.str_fn_name_for_ssa_type(string_type) or { '' }
		assert str_fn_name == ''
		converted := builder.convert_to_string(string_val, string_type)
		assert converted == string_val
	}
}
