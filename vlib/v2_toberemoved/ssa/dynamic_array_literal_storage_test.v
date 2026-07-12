module ssa

import v2.ast
import v2.token
import v2.types

fn dal_ident(name string) ast.Expr {
	return ast.Expr(ast.Ident{
		name: name
	})
}

fn dal_num(value string) ast.Expr {
	return ast.Expr(ast.BasicLiteral{
		kind:  .number
		value: value
	})
}

fn dal_array_type(elem ast.Expr) ast.Expr {
	return ast.Expr(ast.Type(ast.ArrayType{
		elem_type: elem
	}))
}

fn dal_sizeof(expr ast.Expr) ast.Expr {
	return ast.Expr(ast.KeywordOperator{
		op:    token.Token.key_sizeof
		exprs: [expr]
	})
}

fn dal_field(name string, typ string) ast.FieldDecl {
	return ast.FieldDecl{
		name: name
		typ:  dal_ident(typ)
	}
}

fn dal_param(name string, typ ast.Expr) ast.Parameter {
	return ast.Parameter{
		name: name
		typ:  typ
	}
}

fn dal_builtin_file() ast.File {
	return ast.File{
		name:  'builtin.v'
		mod:   'builtin'
		stmts: [
			ast.Stmt(ast.ModuleStmt{
				name: 'builtin'
			}),
			ast.Stmt(ast.StructDecl{
				name:   'array'
				fields: [
					dal_field('data', 'voidptr'),
					dal_field('offset', 'int'),
					dal_field('len', 'int'),
					dal_field('cap', 'int'),
					dal_field('flags', 'int'),
					dal_field('element_size', 'int'),
				]
			}),
			ast.Stmt(ast.FnDecl{
				name: 'new_array_from_c_array_noscan'
				typ:  ast.FnType{
					params:      [
						dal_param('len', dal_ident('int')),
						dal_param('cap', dal_ident('int')),
						dal_param('elem_size', dal_ident('int')),
						dal_param('data', dal_ident('voidptr')),
					]
					return_type: dal_ident('array')
				}
			}),
		]
	}
}

fn dal_dynamic_literal_files() []ast.File {
	return [
		dal_builtin_file(),
		ast.File{
			name:  'main.v'
			mod:   'main'
			stmts: [
				ast.Stmt(ast.ModuleStmt{
					name: 'main'
				}),
				ast.Stmt(ast.FnDecl{
					name:  'make_arr'
					typ:   ast.FnType{
						return_type: dal_array_type(dal_ident('int'))
					}
					stmts: [
						ast.Stmt(ast.AssignStmt{
							op:  .decl_assign
							lhs: [dal_ident('a')]
							rhs: [
								ast.Expr(ast.ArrayInitExpr{
									exprs: [dal_num('1'), dal_num('2'),
										dal_num('3')]
								}),
							]
						}),
						ast.Stmt(ast.ReturnStmt{
							exprs: [
								dal_ident('a'),
							]
						}),
					]
				}),
				ast.Stmt(ast.FnDecl{
					name:  'make_arr_of_arrs'
					typ:   ast.FnType{
						params:      [
							dal_param('a', dal_array_type(dal_ident('int'))),
							dal_param('b', dal_array_type(dal_ident('int'))),
							dal_param('c', dal_array_type(dal_ident('int'))),
						]
						return_type: dal_array_type(dal_array_type(dal_ident('int')))
					}
					stmts: [
						ast.Stmt(ast.ReturnStmt{
							exprs: [
								ast.Expr(ast.CallExpr{
									lhs:  dal_ident('new_array_from_c_array_noscan')
									args: [dal_num('3'), dal_num('3'),
										dal_sizeof(dal_array_type(dal_ident('int'))),
										ast.Expr(ast.ArrayInitExpr{
											exprs: [dal_ident('a'),
												dal_ident('b'),
												dal_ident('c')]
										})]
								}),
							]
						}),
					]
				}),
			]
		},
	]
}

fn dal_build_legacy() &Builder {
	files := dal_dynamic_literal_files()
	env := types.Environment.new()
	mut mod := Module.new('dynamic_array_literal_storage_legacy')
	mut b := Builder.new_with_env(mod, env)
	b.minimal_runtime_roots = true
	b.build_all(files)
	return b
}

fn dal_build_flat() &Builder {
	files := dal_dynamic_literal_files()
	flat := ast.flatten_files(files)
	env := types.Environment.new()
	mut mod := Module.new('dynamic_array_literal_storage_flat')
	mut b := Builder.new_with_env(mod, env)
	b.minimal_runtime_roots = true
	b.build_all_from_flat(&flat)
	return b
}

fn dal_func_value_ids(b &Builder, name string) []ValueID {
	idx := b.fn_index[name] or {
		assert false, 'missing SSA function ${name}'
		return []ValueID{}
	}
	mut ids := []ValueID{}
	for block_id in b.mod.funcs[idx].blocks {
		for value_id in b.mod.blocks[block_id].instrs {
			ids << value_id
		}
	}
	return ids
}

fn dal_new_array_call_ids(b &Builder, fn_name string) []ValueID {
	mut call_ids := []ValueID{}
	for value_id in dal_func_value_ids(b, fn_name) {
		value := b.mod.values[value_id]
		if value.kind != .instruction {
			continue
		}
		instr := b.mod.instrs[value.index]
		if instr.op != .call || instr.operands.len == 0 {
			continue
		}
		callee_id := instr.operands[0]
		if callee_id > 0 && callee_id < b.mod.values.len
			&& b.mod.values[callee_id].name == 'builtin__new_array_from_c_array_noscan' {
			call_ids << value_id
		}
	}
	return call_ids
}

fn dal_new_array_call_in_fn(b &Builder, fn_name string) (ValueID, []ValueID) {
	call_ids := dal_new_array_call_ids(b, fn_name)
	assert call_ids.len == 1
	call_id := call_ids[0]
	instr := b.mod.instrs[b.mod.values[call_id].index]
	return call_id, instr.operands
}

fn dal_new_array_call(b &Builder) (ValueID, []ValueID) {
	return dal_new_array_call_in_fn(b, 'make_arr')
}

fn dal_fixed_array_alloca_type_id(b &Builder, value_id ValueID) TypeID {
	if value_id <= 0 || value_id >= b.mod.values.len {
		return 0
	}
	value := b.mod.values[value_id]
	if value.kind != .instruction {
		return 0
	}
	instr := b.mod.instrs[value.index]
	if instr.op != .alloca {
		return 0
	}
	typ := b.mod.type_store.types[value.typ]
	if typ.kind != .ptr_t || typ.elem_type <= 0 || typ.elem_type >= b.mod.type_store.types.len {
		return 0
	}
	fixed_type := typ.elem_type
	if b.mod.type_store.types[fixed_type].kind != .array_t {
		return 0
	}
	return fixed_type
}

fn dal_new_array_data_buffer_alloca(b &Builder, fn_name string) ValueID {
	_, operands := dal_new_array_call_in_fn(b, fn_name)
	assert operands.len == 5
	data_arg := operands[4]
	assert data_arg > 0 && data_arg < b.mod.values.len
	data_value := b.mod.values[data_arg]
	assert data_value.kind == .instruction
	data_instr := b.mod.instrs[data_value.index]
	if data_instr.op == .bitcast {
		assert data_instr.operands.len == 1
		return data_instr.operands[0]
	}
	return data_arg
}

fn dal_assert_single_new_array_call_in_fn(b &Builder, fn_name string) {
	call_ids := dal_new_array_call_ids(b, fn_name)
	assert call_ids.len == 1
}

fn dal_assert_array_of_arrays_literal_is_raw_element_buffer(b &Builder) {
	array_type := b.struct_types['array'] or { TypeID(0) }
	assert array_type != 0
	dal_assert_single_new_array_call_in_fn(b, 'make_arr_of_arrs')
	buffer_alloca := dal_new_array_data_buffer_alloca(b, 'make_arr_of_arrs')
	fixed_type := dal_fixed_array_alloca_type_id(b, buffer_alloca)
	assert fixed_type != 0
	fixed_info := b.mod.type_store.types[fixed_type]
	assert fixed_info.len == 3
	assert fixed_info.elem_type == array_type
	_, operands := dal_new_array_call_in_fn(b, 'make_arr_of_arrs')
	assert b.mod.values[operands[1]].name == '3'
	assert b.mod.values[operands[2]].name == '3'
	assert b.mod.values[operands[3]].name == '32'
}

fn dal_is_fixed_array_alloca_ptr(b &Builder, value_id ValueID) bool {
	return dal_fixed_array_alloca_type_id(b, value_id) != 0
}

fn dal_assert_no_nested_dynamic_array_for_arg(b &Builder) {
	dal_assert_single_new_array_call_in_fn(b, 'make_arr_of_arrs')
}

fn dal_assert_dijkstra_array_literal_regression(b &Builder) {
	dal_assert_array_of_arrays_literal_is_raw_element_buffer(b)
	dal_assert_no_nested_dynamic_array_for_arg(b)
}

fn dal_assert_no_raw_fixed_array_local_store(b &Builder) {
	for value_id in dal_func_value_ids(b, 'make_arr') {
		value := b.mod.values[value_id]
		if value.kind != .instruction {
			continue
		}
		instr := b.mod.instrs[value.index]
		if instr.op != .store || instr.operands.len < 2 {
			continue
		}
		assert !dal_is_fixed_array_alloca_ptr(b, instr.operands[0])
	}
}

fn dal_assert_dynamic_literal_storage_invariant(b &Builder) {
	array_type := b.struct_types['array'] or { TypeID(0) }
	assert array_type != 0
	call_id, operands := dal_new_array_call(b)
	assert b.mod.values[call_id].typ == array_type
	assert operands.len == 5
	assert b.mod.values[operands[1]].name == '3'
	assert b.mod.values[operands[2]].name == '3'
	assert b.mod.values[operands[3]].name == '4'

	data_arg := operands[4]
	assert data_arg > 0 && data_arg < b.mod.values.len
	data_value := b.mod.values[data_arg]
	assert data_value.kind == .instruction
	data_instr := b.mod.instrs[data_value.index]
	assert data_instr.op == .bitcast
	assert data_instr.operands.len == 1
	assert dal_is_fixed_array_alloca_ptr(b, data_instr.operands[0])

	mut stored_call_to_array_local := false
	for value_id in dal_func_value_ids(b, 'make_arr') {
		value := b.mod.values[value_id]
		if value.kind != .instruction {
			continue
		}
		instr := b.mod.instrs[value.index]
		if instr.op != .store || instr.operands.len < 2 || instr.operands[0] != call_id {
			continue
		}
		dst_type := b.mod.type_store.types[b.mod.values[instr.operands[1]].typ]
		assert dst_type.kind == .ptr_t
		assert dst_type.elem_type == array_type
		stored_call_to_array_local = true
	}
	assert stored_call_to_array_local
	dal_assert_no_raw_fixed_array_local_store(b)
}

fn test_dynamic_array_literal_storage_invariant_matches_legacy_and_flat() {
	legacy := dal_build_legacy()
	dal_assert_dynamic_literal_storage_invariant(legacy)
	dal_assert_dijkstra_array_literal_regression(legacy)
	flat := dal_build_flat()
	dal_assert_dynamic_literal_storage_invariant(flat)
	dal_assert_dijkstra_array_literal_regression(flat)
}
