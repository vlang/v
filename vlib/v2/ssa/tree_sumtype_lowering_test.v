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

fn test_tree_sumtype_variant_struct_init_wraps_sumtype_fields() {
	m := tree_ssa_module_for_source('variant_field_init', tree_sumtype_source())
	tree_ssa_assert_variant_struct_init_wraps_sumtype_fields(m)
}

fn test_tree_sumtype_flat_variant_struct_init_wraps_sumtype_fields() {
	m := tree_ssa_module_for_source_flat('flat_variant_field_init', tree_sumtype_source())
	tree_ssa_assert_variant_struct_init_wraps_sumtype_fields(m)
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
