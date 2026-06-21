module ssa

import v2.ast
import v2.token
import v2.types

fn core_reg_ident(name string) ast.Expr {
	return ast.Expr(ast.Ident{
		name: name
	})
}

fn core_reg_field(name string, typ string) ast.FieldDecl {
	return ast.FieldDecl{
		name: name
		typ:  core_reg_ident(typ)
	}
}

fn core_reg_builtin_file() ast.File {
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
					core_reg_field('data', 'voidptr'),
					core_reg_field('offset', 'int'),
					core_reg_field('len', 'int'),
					core_reg_field('cap', 'int'),
					core_reg_field('flags', 'int'),
					core_reg_field('element_size', 'int'),
				]
			}),
			ast.Stmt(ast.StructDecl{
				name:   'string'
				fields: [
					core_reg_field('str', 'voidptr'),
					core_reg_field('len', 'int'),
					core_reg_field('is_lit', 'int'),
				]
			}),
		]
	}
}

fn core_reg_array_type(elem ast.Expr) ast.Expr {
	return ast.Expr(ast.Type(ast.ArrayType{
		elem_type: elem
	}))
}

fn core_reg_selector(lhs ast.Expr, rhs string) ast.Expr {
	return ast.Expr(ast.SelectorExpr{
		lhs: lhs
		rhs: ast.Ident{
			name: rhs
		}
	})
}

fn core_reg_call(lhs ast.Expr, args []ast.Expr) ast.Expr {
	return ast.Expr(ast.CallExpr{
		lhs:  lhs
		args: args
	})
}

fn core_reg_num(value string) ast.Expr {
	return ast.Expr(ast.BasicLiteral{
		kind:  .number
		value: value
	})
}

fn core_reg_bool(value bool) ast.Expr {
	return ast.Expr(ast.BasicLiteral{
		kind: if value { token.Token.key_true } else { token.Token.key_false }
	})
}

fn core_reg_string(value string) ast.Expr {
	return ast.Expr(ast.StringLiteral{
		kind:  .v
		value: value
	})
}

fn core_reg_infix(lhs ast.Expr, op token.Token, rhs ast.Expr) ast.Expr {
	return ast.Expr(ast.InfixExpr{
		op:  op
		lhs: lhs
		rhs: rhs
	})
}

fn core_reg_decl(name string, rhs ast.Expr) ast.Stmt {
	return ast.Stmt(ast.AssignStmt{
		op:  .decl_assign
		lhs: [core_reg_ident(name)]
		rhs: [rhs]
	})
}

fn core_reg_return(expr ast.Expr) ast.Stmt {
	return ast.Stmt(ast.ReturnStmt{
		exprs: [expr]
	})
}

fn core_reg_expr_stmt(expr ast.Expr) ast.Stmt {
	return ast.Stmt(ast.ExprStmt{
		expr: expr
	})
}

fn core_reg_empty_dynamic_array(elem string, cap ast.Expr) ast.Expr {
	return ast.Expr(ast.ArrayInitExpr{
		typ: core_reg_array_type(core_reg_ident(elem))
		cap: cap
	})
}

fn core_reg_pointer_type(name string) ast.Expr {
	return ast.Expr(ast.PrefixExpr{
		op:   .amp
		expr: core_reg_ident(name)
	})
}

fn core_reg_call_or_cast(lhs ast.Expr, expr ast.Expr) ast.Expr {
	return ast.Expr(ast.CallOrCastExpr{
		lhs:  lhs
		expr: expr
	})
}

fn core_reg_unsafe_expr(expr ast.Expr) ast.Expr {
	return ast.Expr(ast.UnsafeExpr{
		stmts: [
			ast.Stmt(ast.ExprStmt{
				expr: expr
			}),
		]
	})
}

fn core_reg_string_runes_decl() ast.Stmt {
	s_ident := core_reg_ident('s')
	runes_ident := core_reg_ident('runes')
	return ast.Stmt(ast.FnDecl{
		is_method: true
		receiver:  ast.Parameter{
			name: 's'
			typ:  core_reg_ident('string')
		}
		name:      'runes'
		typ:       ast.FnType{
			return_type: core_reg_array_type(core_reg_ident('rune'))
		}
		stmts:     [
			core_reg_decl('runes', core_reg_empty_dynamic_array('rune', core_reg_selector(s_ident,
				'len'))),
			core_reg_return(runes_ident),
		]
	})
}

fn core_reg_string_graphemes_impl_decl() ast.Stmt {
	s_ident := core_reg_ident('s')
	runes_ident := core_reg_ident('runes')
	res_ident := core_reg_ident('res')
	return ast.Stmt(ast.FnDecl{
		name:  'string_graphemes_impl'
		typ:   ast.FnType{
			params:      [
				ast.Parameter{
					name: 's'
					typ:  core_reg_ident('string')
				},
			]
			return_type: core_reg_array_type(core_reg_ident('string'))
		}
		stmts: [
			core_reg_decl('runes', core_reg_call(core_reg_selector(s_ident, 'runes'), []ast.Expr{})),
			core_reg_decl('res', core_reg_empty_dynamic_array('string', core_reg_selector(runes_ident,
				'len'))),
			core_reg_decl('cluster', core_reg_empty_dynamic_array('rune', ast.Expr(ast.BasicLiteral{
				kind:  .number
				value: '4'
			}))),
			core_reg_return(res_ident),
		]
	})
}

fn core_reg_tos2_decl() ast.Stmt {
	return ast.Stmt(ast.FnDecl{
		name: 'tos2'
		typ:  ast.FnType{
			params:      [
				ast.Parameter{
					name: 's'
					typ:  core_reg_pointer_type('u8')
				},
			]
			return_type: core_reg_ident('string')
		}
	})
}

fn core_reg_string_clone_decl() ast.Stmt {
	return ast.Stmt(ast.FnDecl{
		is_method: true
		receiver:  ast.Parameter{
			name: 's'
			typ:  core_reg_ident('string')
		}
		name:      'clone'
		typ:       ast.FnType{
			return_type: core_reg_ident('string')
		}
	})
}

fn core_reg_array_clone_decl() ast.Stmt {
	return ast.Stmt(ast.FnDecl{
		is_method: true
		receiver:  ast.Parameter{
			name: 'a'
			typ:  core_reg_ident('array')
		}
		name:      'clone'
		typ:       ast.FnType{
			return_type: core_reg_ident('array')
		}
	})
}

fn core_reg_array_alloc_like_decl() ast.Stmt {
	return ast.Stmt(ast.FnDecl{
		is_method: true
		receiver:  ast.Parameter{
			name: 'a'
			typ:  core_reg_ident('array')
		}
		name:      'alloc_array_data_like_uninit'
		typ:       ast.FnType{
			params:      [
				ast.Parameter{
					name: 'new_size'
					typ:  core_reg_ident('u64')
				},
			]
			return_type: core_reg_ident('voidptr')
		}
	})
}

fn core_reg_int_str_decl() ast.Stmt {
	return ast.Stmt(ast.FnDecl{
		is_method: true
		receiver:  ast.Parameter{
			name: 'n'
			typ:  core_reg_ident('int')
		}
		name:      'str'
		typ:       ast.FnType{
			return_type: core_reg_ident('string')
		}
	})
}

fn core_reg_tos_clone_decl() ast.Stmt {
	const_s := core_reg_ident('const_s')
	ptr_cast := ast.Expr(ast.PrefixExpr{
		op:   .amp
		expr: core_reg_call_or_cast(core_reg_ident('u8'), const_s)
	})
	tos2_call := core_reg_call_or_cast(core_reg_ident('tos2'), ptr_cast)
	return ast.Stmt(ast.FnDecl{
		name:  'tos_clone'
		typ:   ast.FnType{
			params:      [
				ast.Parameter{
					name: 'const_s'
					typ:  core_reg_pointer_type('u8')
				},
			]
			return_type: core_reg_ident('string')
		}
		stmts: [
			core_reg_decl('s', core_reg_unsafe_expr(tos2_call)),
			core_reg_return(core_reg_call(core_reg_selector(core_reg_ident('s'), 'clone'),
				[]ast.Expr{})),
		]
	})
}

fn core_reg_param(name string, typ ast.Expr) ast.Parameter {
	return ast.Parameter{
		name: name
		typ:  typ
	}
}

fn core_reg_fn_decl(name string, params []ast.Parameter, return_type ast.Expr, stmts []ast.Stmt) ast.Stmt {
	return ast.Stmt(ast.FnDecl{
		name:  name
		typ:   ast.FnType{
			params:      params
			return_type: return_type
		}
		stmts: stmts
	})
}

fn core_reg_c_puts_decl() ast.Stmt {
	return ast.Stmt(ast.FnDecl{
		name:     'puts'
		language: .c
		typ:      ast.FnType{
			params:      [
				core_reg_param('s', core_reg_ident('voidptr')),
			]
			return_type: core_reg_ident('int')
		}
	})
}

fn core_reg_dep_file() ast.File {
	x_ident := core_reg_ident('x')
	return ast.File{
		name:  'dep.v'
		mod:   'dep'
		stmts: [
			ast.Stmt(ast.ModuleStmt{
				name: 'dep'
			}),
			ast.Stmt(ast.StructDecl{
				name:   'Type'
				fields: [
					core_reg_field('value', 'int'),
				]
			}),
			core_reg_fn_decl('value', [
				core_reg_param('x', core_reg_ident('int')),
			], core_reg_ident('int'), [
				core_reg_return(x_ident),
			]),
			core_reg_fn_decl('cast_c_type', [
				core_reg_param('x', core_reg_ident('int')),
			], core_reg_ident('Type'), [
				core_reg_return(core_reg_call_or_cast(core_reg_selector(core_reg_ident('C'), 'Type'),
					x_ident)),
			]),
		]
	}
}

fn core_reg_call_or_cast_matrix_main_file() ast.File {
	p_ident := core_reg_ident('p')
	x_ident := core_reg_ident('x')
	s_ident := core_reg_ident('s')
	a_ident := core_reg_ident('a')
	n_ident := core_reg_ident('n')
	return ast.File{
		name:  'main.v'
		mod:   'main'
		stmts: [
			ast.Stmt(ast.ModuleStmt{
				name: 'main'
			}),
			core_reg_c_puts_decl(),
			core_reg_fn_decl('call_tos2', [
				core_reg_param('p', core_reg_pointer_type('u8')),
			], core_reg_ident('string'), [
				core_reg_return(core_reg_call_or_cast(core_reg_ident('tos2'), p_ident)),
			]),
			core_reg_fn_decl('cast_int', [
				core_reg_param('x', core_reg_ident('i64')),
			], core_reg_ident('int'), [
				core_reg_return(core_reg_call_or_cast(core_reg_ident('int'), x_ident)),
			]),
			core_reg_fn_decl('cast_voidptr', [
				core_reg_param('x', core_reg_ident('i64')),
			], core_reg_ident('voidptr'), [
				core_reg_return(core_reg_call_or_cast(core_reg_ident('voidptr'), x_ident)),
			]),
			core_reg_fn_decl('cast_u8_ptr', [
				core_reg_param('x', core_reg_ident('voidptr')),
			], core_reg_pointer_type('u8'), [
				core_reg_return(ast.Expr(ast.PrefixExpr{
					op:   .amp
					expr: core_reg_call_or_cast(core_reg_ident('u8'), x_ident)
				})),
			]),
			core_reg_fn_decl('call_c_puts', [
				core_reg_param('p', core_reg_ident('voidptr')),
			], core_reg_ident('int'), [
				core_reg_return(core_reg_call_or_cast(core_reg_selector(core_reg_ident('C'), 'puts'),
					p_ident)),
			]),
			core_reg_fn_decl('call_dep_value', [
				core_reg_param('x', core_reg_ident('int')),
			], core_reg_ident('int'), [
				core_reg_return(core_reg_call_or_cast(core_reg_selector(core_reg_ident('dep'),
					'value'), x_ident)),
			]),
			core_reg_fn_decl('cast_dep_type', [
				core_reg_param('x', core_reg_ident('int')),
			], core_reg_selector(core_reg_ident('dep'), 'Type'), [
				core_reg_return(core_reg_call_or_cast(core_reg_selector(core_reg_ident('dep'),
					'Type'), x_ident)),
			]),
			core_reg_fn_decl('clone_string', [
				core_reg_param('s', core_reg_ident('string')),
			], core_reg_ident('string'), [
				core_reg_return(core_reg_call(core_reg_selector(s_ident, 'clone'), []ast.Expr{})),
			]),
			core_reg_fn_decl('clone_array', [
				core_reg_param('a', core_reg_ident('array')),
			], core_reg_ident('array'), [
				core_reg_return(core_reg_call(core_reg_selector(a_ident, 'clone'), []ast.Expr{})),
			]),
			core_reg_fn_decl('call_array_alloc_like', [
				core_reg_param('a', core_reg_ident('array')),
				core_reg_param('n', core_reg_ident('u64')),
			], core_reg_ident('voidptr'), [
				core_reg_return(core_reg_call_or_cast(core_reg_selector(a_ident,
					'alloc_array_data_like_uninit'), n_ident)),
			]),
		]
	}
}

fn core_reg_call_or_cast_matrix_files() []ast.File {
	base_builtin := core_reg_builtin_file()
	mut builtin_stmts := base_builtin.stmts.clone()
	builtin_stmts << core_reg_tos2_decl()
	builtin_stmts << core_reg_string_clone_decl()
	builtin_stmts << core_reg_array_clone_decl()
	builtin_stmts << core_reg_array_alloc_like_decl()
	builtin := ast.File{
		name:  base_builtin.name
		mod:   base_builtin.mod
		stmts: builtin_stmts
	}
	return [builtin, core_reg_dep_file(), core_reg_call_or_cast_matrix_main_file()]
}

fn core_reg_mod_eq_zero(lhs ast.Expr, divisor string) ast.Expr {
	return core_reg_infix(core_reg_infix(lhs, .mod, core_reg_num(divisor)), .eq, core_reg_num('0'))
}

fn core_reg_match_branch(conds []ast.Expr, value ast.Expr) ast.MatchBranch {
	return ast.MatchBranch{
		cond:  conds
		stmts: [
			core_reg_expr_stmt(value),
		]
	}
}

fn core_reg_match_true_value_main_file() ast.File {
	n_ident := core_reg_ident('n')
	match_expr := ast.Expr(ast.MatchExpr{
		expr:     core_reg_bool(true)
		branches: [
			core_reg_match_branch([core_reg_mod_eq_zero(n_ident, '15')],
				core_reg_string("'FizzBuzz'")),
			core_reg_match_branch([core_reg_mod_eq_zero(n_ident, '5')], core_reg_string("'Buzz'")),
			core_reg_match_branch([core_reg_mod_eq_zero(n_ident, '3')], core_reg_string("'Fizz'")),
			core_reg_match_branch([]ast.Expr{}, core_reg_call(core_reg_selector(n_ident, 'str'),
				[]ast.Expr{})),
		]
	})
	return ast.File{
		name:  'main.v'
		mod:   'main'
		stmts: [
			ast.Stmt(ast.ModuleStmt{
				name: 'main'
			}),
			core_reg_fn_decl('fizz_value', [
				core_reg_param('n', core_reg_ident('int')),
			], core_reg_ident('string'), [
				core_reg_decl('value', match_expr),
				core_reg_return(core_reg_ident('value')),
			]),
		]
	}
}

fn core_reg_match_true_value_files() []ast.File {
	base_builtin := core_reg_builtin_file()
	mut builtin_stmts := base_builtin.stmts.clone()
	builtin_stmts << core_reg_int_str_decl()
	builtin := ast.File{
		name:  base_builtin.name
		mod:   base_builtin.mod
		stmts: builtin_stmts
	}
	return [builtin, core_reg_match_true_value_main_file()]
}

fn core_reg_literal_decl() ast.Stmt {
	return ast.Stmt(ast.FnDecl{
		name:  'literal_value'
		typ:   ast.FnType{
			return_type: core_reg_ident('string')
		}
		stmts: [
			core_reg_return(ast.Expr(ast.StringLiteral{
				kind:  .v
				value: "'hi'"
			})),
		]
	})
}

fn core_reg_call_runes_decl() ast.Stmt {
	s_ident := core_reg_ident('s')
	return ast.Stmt(ast.FnDecl{
		name:  'call_runes'
		typ:   ast.FnType{
			params:      [
				ast.Parameter{
					name: 's'
					typ:  core_reg_ident('string')
				},
			]
			return_type: core_reg_array_type(core_reg_ident('rune'))
		}
		stmts: [
			core_reg_return(core_reg_call(core_reg_selector(s_ident, 'runes'), []ast.Expr{})),
		]
	})
}

fn core_reg_contract_files() []ast.File {
	base_builtin := core_reg_builtin_file()
	mut builtin_stmts := base_builtin.stmts.clone()
	builtin_stmts << core_reg_string_runes_decl()
	builtin_stmts << core_reg_string_graphemes_impl_decl()
	builtin := ast.File{
		name:  base_builtin.name
		mod:   base_builtin.mod
		stmts: builtin_stmts
	}
	main_file := ast.File{
		name:  'main.v'
		mod:   'main'
		stmts: [
			ast.Stmt(ast.ModuleStmt{
				name: 'main'
			}),
			core_reg_literal_decl(),
			core_reg_call_runes_decl(),
		]
	}
	return [builtin, main_file]
}

fn core_reg_build_contract() &Builder {
	files := core_reg_contract_files()
	env := types.Environment.new()
	mut mod := Module.new('core_builtin_type_contract')
	mut b := Builder.new_with_env(mod, env)
	b.minimal_runtime_roots = true
	b.build_all(files)
	return b
}

fn core_reg_build_call_or_cast_matrix_legacy() &Builder {
	files := core_reg_call_or_cast_matrix_files()
	env := types.Environment.new()
	mut mod := Module.new('call_or_cast_matrix_legacy')
	mut b := Builder.new_with_env(mod, env)
	b.minimal_runtime_roots = true
	b.build_all(files)
	return b
}

fn core_reg_build_call_or_cast_matrix_flat() &Builder {
	files := core_reg_call_or_cast_matrix_files()
	flat := ast.flatten_files(files)
	env := types.Environment.new()
	mut mod := Module.new('call_or_cast_matrix_flat')
	mut b := Builder.new_with_env(mod, env)
	b.minimal_runtime_roots = true
	b.build_all_from_flat(&flat)
	return b
}

fn core_reg_build_match_true_value_legacy() &Builder {
	files := core_reg_match_true_value_files()
	env := types.Environment.new()
	mut mod := Module.new('match_true_value_legacy')
	mut b := Builder.new_with_env(mod, env)
	b.minimal_runtime_roots = true
	b.build_all(files)
	return b
}

fn core_reg_build_match_true_value_flat() &Builder {
	files := core_reg_match_true_value_files()
	flat := ast.flatten_files(files)
	env := types.Environment.new()
	mut mod := Module.new('match_true_value_flat')
	mut b := Builder.new_with_env(mod, env)
	b.minimal_runtime_roots = true
	b.build_all_from_flat(&flat)
	return b
}

fn core_reg_func_idx(b &Builder, name string) int {
	return b.fn_index[name] or {
		assert false, 'missing SSA function ${name}'
		return -1
	}
}

fn core_reg_func_value_ids(b &Builder, name string) []ValueID {
	idx := core_reg_func_idx(b, name)
	func := b.mod.funcs[idx]
	mut ids := []ValueID{}
	for block_id in func.blocks {
		for value_id in b.mod.blocks[block_id].instrs {
			ids << value_id
		}
	}
	return ids
}

fn core_reg_call_callees(b &Builder, name string) []string {
	mut callees := []string{}
	for value_id in core_reg_func_value_ids(b, name) {
		value := b.mod.values[value_id]
		if value.kind != .instruction {
			continue
		}
		instr := b.mod.instrs[value.index]
		if instr.op != .call || instr.operands.len == 0 {
			continue
		}
		callee_id := instr.operands[0]
		if callee_id > 0 && callee_id < b.mod.values.len {
			callees << b.mod.values[callee_id].name
		}
	}
	return callees
}

fn core_reg_assert_callees(b &Builder, name string, expected []string) {
	callees := core_reg_call_callees(b, name)
	assert callees == expected, '${name} callees: ${callees}, expected ${expected}'
}

fn core_reg_assert_no_zero_loads(b &Builder, name string) {
	for value_id in core_reg_func_value_ids(b, name) {
		value := b.mod.values[value_id]
		if value.kind != .instruction {
			continue
		}
		instr := b.mod.instrs[value.index]
		if instr.op == .load {
			assert value.typ != 0, '${name} has load v${value_id} with type 0'
		}
	}
}

fn core_reg_find_call_type(b &Builder, func_name string, callee_name string) TypeID {
	for value_id in core_reg_func_value_ids(b, func_name) {
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
			&& b.mod.values[callee_id].name == callee_name {
			return value.typ
		}
	}
	assert false, 'missing call to ${callee_name} in ${func_name}'
	return 0
}

fn core_reg_assert_return_type(b &Builder, func_name string, expected TypeID) {
	idx := core_reg_func_idx(b, func_name)
	assert b.mod.funcs[idx].typ == expected
}

fn core_reg_has_phi_type(b &Builder, func_name string, expected TypeID) bool {
	for value_id in core_reg_func_value_ids(b, func_name) {
		value := b.mod.values[value_id]
		if value.kind != .instruction {
			continue
		}
		instr := b.mod.instrs[value.index]
		if instr.op == .phi && value.typ == expected {
			return true
		}
	}
	return false
}

fn core_reg_assert_call_or_cast_matrix(mut b Builder) {
	i8_t := b.mod.type_store.get_int(8)
	u8_t := b.mod.type_store.get_uint(8)
	i32_t := b.mod.type_store.get_int(32)
	ptr_i8_t := b.mod.type_store.get_ptr(i8_t)
	ptr_u8_t := b.mod.type_store.get_ptr(u8_t)
	dep_type := b.struct_types['dep__Type'] or {
		assert false, 'missing dep__Type'
		return
	}
	assert 'dep__value' in b.fn_index
	dep_value_selector := ast.SelectorExpr{
		lhs: core_reg_ident('dep')
		rhs: ast.Ident{
			name: 'value'
		}
	}
	assert b.selector_module_name(dep_value_selector) or { '' } == 'dep'
	string_type := b.get_string_type()
	array_type := b.get_array_type()

	core_reg_assert_callees(b, 'call_tos2', ['builtin__tos2'])
	assert core_reg_find_call_type(b, 'call_tos2', 'builtin__tos2') == string_type

	core_reg_assert_callees(b, 'cast_int', [])
	core_reg_assert_return_type(b, 'cast_int', i32_t)
	core_reg_assert_callees(b, 'cast_voidptr', [])
	core_reg_assert_return_type(b, 'cast_voidptr', ptr_i8_t)
	core_reg_assert_callees(b, 'cast_u8_ptr', [])
	core_reg_assert_return_type(b, 'cast_u8_ptr', ptr_u8_t)

	core_reg_assert_callees(b, 'call_c_puts', ['puts'])
	assert core_reg_find_call_type(b, 'call_c_puts', 'puts') == i32_t
	core_reg_assert_callees(b, 'dep__cast_c_type', [])
	core_reg_assert_return_type(b, 'dep__cast_c_type', dep_type)

	core_reg_assert_callees(b, 'call_dep_value', ['dep__value'])
	assert core_reg_find_call_type(b, 'call_dep_value', 'dep__value') == i32_t
	core_reg_assert_callees(b, 'cast_dep_type', [])
	core_reg_assert_return_type(b, 'cast_dep_type', dep_type)

	core_reg_assert_callees(b, 'clone_string', ['builtin__string__clone'])
	assert core_reg_find_call_type(b, 'clone_string', 'builtin__string__clone') == string_type
	core_reg_assert_callees(b, 'clone_array', ['builtin__array__clone'])
	assert core_reg_find_call_type(b, 'clone_array', 'builtin__array__clone') == array_type
	core_reg_assert_callees(b, 'call_array_alloc_like', [
		'builtin__array__alloc_array_data_like_uninit',
	])
	assert core_reg_find_call_type(b, 'call_array_alloc_like',
		'builtin__array__alloc_array_data_like_uninit') == ptr_i8_t
}

fn core_reg_assert_match_true_value(mut b Builder) {
	string_type := b.get_string_type()
	assert string_type != 0
	core_reg_assert_return_type(b, 'fizz_value', string_type)
	core_reg_assert_callees(b, 'fizz_value', ['builtin__int__str'])
	assert core_reg_find_call_type(b, 'fizz_value', 'builtin__int__str') == string_type
	assert core_reg_has_phi_type(b, 'fizz_value', string_type)
}

fn assert_core_builtin_types_registered(mut b Builder) {
	assert ssa_string_ok('array')
	assert ssa_string_ok('string')
	assert b.get_array_type() != 0
	assert b.get_string_type() != 0
	assert b.struct_types['array'] == b.get_array_type()
	assert b.struct_types['string'] == b.get_string_type()
}

fn test_build_all_preregisters_core_builtin_types() {
	files := [core_reg_builtin_file()]
	env := types.Environment.new()
	mut mod := Module.new('core_builtin_type_registration')
	mut b := Builder.new_with_env(mod, env)
	b.skip_fn_bodies = true
	b.minimal_runtime_roots = true
	b.build_all(files)
	assert_core_builtin_types_registered(mut b)
}

fn test_build_all_from_flat_preregisters_core_builtin_types() {
	files := [core_reg_builtin_file()]
	flat := ast.flatten_files(files)
	env := types.Environment.new()
	mut mod := Module.new('core_builtin_type_registration_flat')
	mut b := Builder.new_with_env(mod, env)
	b.skip_fn_bodies = true
	b.minimal_runtime_roots = true
	b.build_all_from_flat(&flat)
	assert_core_builtin_types_registered(mut b)
}

fn test_string_literal_uses_canonical_string_struct_type() {
	mut b := core_reg_build_contract()
	string_type := b.get_string_type()
	assert string_type != 0
	assert b.mod.type_store.types[string_type].kind == .struct_t

	mut found := false
	for value in b.mod.values {
		if value.kind == .string_literal {
			assert value.typ == string_type
			found = true
		}
	}
	assert found
}

fn test_builtin_string_runes_signature_and_call_return_canonical_array_type() {
	mut b := core_reg_build_contract()
	array_type := b.get_array_type()
	assert array_type != 0
	assert b.mod.type_store.types[array_type].kind == .struct_t

	runes_idx := core_reg_func_idx(b, 'builtin__string__runes')
	assert b.mod.funcs[runes_idx].typ == array_type
	assert core_reg_find_call_type(b, 'call_runes', 'builtin__string__runes') == array_type
}

fn test_string_graphemes_impl_has_no_zero_typed_loads() {
	b := core_reg_build_contract()
	core_reg_assert_no_zero_loads(b, 'builtin__string_graphemes_impl')
}

fn test_empty_dynamic_rune_array_local_load_is_canonical_array_type() {
	mut b := core_reg_build_contract()
	array_type := b.get_array_type()
	mut found_array_load := false
	for value_id in core_reg_func_value_ids(b, 'builtin__string__runes') {
		value := b.mod.values[value_id]
		if value.kind != .instruction {
			continue
		}
		instr := b.mod.instrs[value.index]
		if instr.op == .load {
			assert value.typ != 0, 'builtin__string__runes has load v${value_id} with type 0'
			if value.typ == array_type {
				found_array_load = true
			}
		}
	}
	assert found_array_load
}

fn test_call_or_cast_dispatch_matrix_matches_legacy_and_flat() {
	mut legacy := core_reg_build_call_or_cast_matrix_legacy()
	core_reg_assert_call_or_cast_matrix(mut legacy)
	mut flat := core_reg_build_call_or_cast_matrix_flat()
	core_reg_assert_call_or_cast_matrix(mut flat)
}

fn test_match_true_expression_lowers_to_string_value_legacy_and_flat() {
	mut legacy := core_reg_build_match_true_value_legacy()
	core_reg_assert_match_true_value(mut legacy)
	mut flat := core_reg_build_match_true_value_flat()
	core_reg_assert_match_true_value(mut flat)
}

fn test_call_or_cast_known_builtin_fn_stays_call_before_string_clone_resolution() {
	base_builtin := core_reg_builtin_file()
	mut builtin_stmts := base_builtin.stmts.clone()
	builtin_stmts << core_reg_tos2_decl()
	builtin_stmts << core_reg_string_clone_decl()
	builtin_stmts << core_reg_array_clone_decl()
	builtin_stmts << core_reg_tos_clone_decl()
	files := [
		ast.File{
			name:  base_builtin.name
			mod:   base_builtin.mod
			stmts: builtin_stmts
		},
	]
	flat := ast.flatten_files(files)
	env := types.Environment.new()
	mut mod := Module.new('core_builtin_call_or_cast_fn')
	mut b := Builder.new_with_env(mod, env)
	b.minimal_runtime_roots = true
	b.build_all_from_flat(&flat)

	callees := core_reg_call_callees(b, 'builtin__tos_clone')
	assert 'builtin__tos2' in callees
	assert 'builtin__string__clone' in callees
	assert 'builtin__array__clone' !in callees
	assert core_reg_find_call_type(b, 'builtin__tos_clone', 'builtin__tos2') == b.get_string_type()
	assert core_reg_find_call_type(b, 'builtin__tos_clone', 'builtin__string__clone') == b.get_string_type()
}
