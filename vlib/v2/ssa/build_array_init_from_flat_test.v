// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
// vtest build: macos
//
// Bit-equality pin for the cursor-native rewrite of `build_array_init_from_flat`.
// The previous helper rehydrated every ArrayInitExpr field via `decode_expr`
// (typ, init, cap, len, update_expr, and each element) and dispatched to
// legacy `build_array_init_expr`. The cursor-native rewrite mirrors
// `build_array_init_expr` directly: elements walk `c.edge(5)..edge_count()` via
// `build_expr_from_flat`; the declared element type is resolved from `typ_c`
// using a `.typ_array` / `.typ_array_fixed` kind match (`edge(0)` / `edge(1)`
// for elem_type) plus an `ast_type_to_ssa_from_flat` call; the fixed-array
// marker check inspects `len_c.kind() == .expr_postfix` with `aux == .not`
// (the `!` suffix) or `typ_c.kind() == .typ_array_fixed`. Empty dynamic
// arrays with explicit `len`/`cap` now read those edges and lower to the same
// runtime allocation call as the legacy builder. Pin asserts module-count
// parity for the common literal `[1, 2, 3]` path and the dynamic
// `[]int{len: 3, cap: 5}` path.
module ssa

import v2.ast
import v2.types

fn ai_ident(name string) ast.Expr {
	return ast.Expr(ast.Ident{
		name: name
	})
}

fn ai_num(value string) ast.Expr {
	return ast.Expr(ast.BasicLiteral{
		kind:  .number
		value: value
	})
}

fn ai_array_type(elem ast.Expr) ast.Expr {
	return ast.Expr(ast.Type(ast.ArrayType{
		elem_type: elem
	}))
}

fn ai_field(name string, typ string) ast.FieldDecl {
	return ast.FieldDecl{
		name: name
		typ:  ai_ident(typ)
	}
}

fn make_array_init_fixture() []ast.File {
	mut stmts := [
		ast.Stmt(ast.ModuleStmt{
			name: 'main'
		}),
	]
	// fn make_arr() { a := [1, 2, 3] _ = a }
	stmts << ast.Stmt(ast.FnDecl{
		name:  'make_arr'
		typ:   ast.FnType{
			return_type: ast.empty_expr
		}
		stmts: [
			ast.Stmt(ast.AssignStmt{
				op:  .decl_assign
				lhs: [ai_ident('a')]
				rhs: [
					ast.Expr(ast.ArrayInitExpr{
						exprs: [ai_num('1'), ai_num('2'), ai_num('3')]
					}),
				]
			}),
		]
	})
	// fn make_dynamic_arr() { a := []int{len: 3, cap: 5} _ = a }
	stmts << ast.Stmt(ast.FnDecl{
		name:  'make_dynamic_arr'
		typ:   ast.FnType{
			return_type: ast.empty_expr
		}
		stmts: [
			ast.Stmt(ast.AssignStmt{
				op:  .decl_assign
				lhs: [ai_ident('a')]
				rhs: [
					ast.Expr(ast.ArrayInitExpr{
						typ: ai_array_type(ai_ident('int'))
						len: ai_num('3')
						cap: ai_num('5')
					}),
				]
			}),
		]
	})
	return [
		ast.File{
			name:  'main.v'
			mod:   'main'
			stmts: stmts
		},
	]
}

fn make_dynamic_array_init_fixture() []ast.File {
	builtin_file := ast.File{
		name:  'builtin.v'
		mod:   'builtin'
		stmts: [
			ast.Stmt(ast.ModuleStmt{
				name: 'builtin'
			}),
			ast.Stmt(ast.StructDecl{
				name:   'array'
				fields: [
					ai_field('data', 'voidptr'),
					ai_field('offset', 'int'),
					ai_field('len', 'int'),
					ai_field('cap', 'int'),
					ai_field('flags', 'int'),
					ai_field('element_size', 'int'),
				]
			}),
			ast.Stmt(ast.FnDecl{
				name:     'builtin____new_array_noscan'
				language: .c
				typ:      ast.FnType{
					params:      [
						ast.Parameter{
							name: 'len'
							typ:  ai_ident('int')
						},
						ast.Parameter{
							name: 'cap'
							typ:  ai_ident('int')
						},
						ast.Parameter{
							name: 'elem_size'
							typ:  ai_ident('int')
						},
					]
					return_type: ai_ident('array')
				}
			}),
		]
	}
	main_file := ast.File{
		name:  'main.v'
		mod:   'main'
		stmts: [
			ast.Stmt(ast.ModuleStmt{
				name: 'main'
			}),
			ast.Stmt(ast.FnDecl{
				name:  'make_dynamic'
				typ:   ast.FnType{
					return_type: ai_array_type(ai_ident('int'))
				}
				stmts: [
					ast.Stmt(ast.ReturnStmt{
						exprs: [
							ast.Expr(ast.ArrayInitExpr{
								typ: ai_array_type(ai_ident('int'))
								len: ai_num('3')
								cap: ai_num('5')
							}),
						]
					}),
				]
			}),
		]
	}
	return [builtin_file, main_file]
}

fn build_via_legacy_array_init(files []ast.File, env &types.Environment, name string) &Module {
	mut mod := Module.new(name)
	mut b := Builder.new_with_env(mod, env)
	b.register_fn_signatures(files[0])
	b.build_fn_bodies(files[0])
	return mod
}

fn build_via_flat_array_init(files []ast.File, env &types.Environment, name string) &Module {
	flat := ast.flatten_files(files)
	mut mod := Module.new(name)
	mut b := Builder.new_with_env(mod, env)
	b.register_fn_signatures_from_flat(flat.file_cursor(0))
	b.build_fn_bodies_from_flat(flat.file_cursor(0))
	return mod
}

fn build_all_via_legacy_array_init(files []ast.File, env &types.Environment, name string) &Module {
	mut mod := Module.new(name)
	mut b := Builder.new_with_env(mod, env)
	b.minimal_runtime_roots = true
	b.build_all(files)
	return mod
}

fn build_all_via_flat_array_init(files []ast.File, env &types.Environment, name string) &Module {
	flat := ast.flatten_files(files)
	mut mod := Module.new(name)
	mut b := Builder.new_with_env(mod, env)
	b.minimal_runtime_roots = true
	b.build_all_from_flat(&flat)
	return mod
}

fn ai_call_operands(m &Module, func_name string, callee_name string) []ValueID {
	for func in m.funcs {
		if func.name != func_name {
			continue
		}
		for block_id in func.blocks {
			for value_id in m.blocks[block_id].instrs {
				value := m.values[value_id]
				if value.kind != .instruction {
					continue
				}
				instr := m.instrs[value.index]
				if instr.op != .call || instr.operands.len == 0 {
					continue
				}
				callee_id := instr.operands[0]
				if callee_id > 0 && callee_id < m.values.len
					&& m.values[callee_id].name == callee_name {
					return instr.operands
				}
			}
		}
	}
	assert false, 'missing call to ${callee_name} in ${func_name}'
	return []ValueID{}
}

fn ai_assert_dynamic_array_operands(m &Module) {
	operands := ai_call_operands(m, 'make_dynamic', 'builtin____new_array_noscan')
	assert operands.len == 4
	assert m.values[operands[1]].name == '3'
	assert m.values[operands[2]].name == '5'
	assert m.values[operands[3]].name == '4'
}

fn test_build_array_init_from_flat_matches_legacy() {
	files := make_array_init_fixture()
	env := types.Environment.new()
	mod_legacy := build_via_legacy_array_init(files, env, 'array_init_legacy')
	mod_flat := build_via_flat_array_init(files, env, 'array_init_flat')

	assert mod_legacy.funcs.len == mod_flat.funcs.len
	assert mod_legacy.blocks.len == mod_flat.blocks.len
	assert mod_legacy.instrs.len == mod_flat.instrs.len
	assert mod_legacy.values.len == mod_flat.values.len
}

fn test_build_array_init_from_flat_dynamic_len_cap_operands_match_legacy() {
	files := make_dynamic_array_init_fixture()
	env := types.Environment.new()
	mod_legacy := build_all_via_legacy_array_init(files, env, 'array_init_dynamic_legacy')
	mod_flat := build_all_via_flat_array_init(files, env, 'array_init_dynamic_flat')

	ai_assert_dynamic_array_operands(mod_legacy)
	ai_assert_dynamic_array_operands(mod_flat)
}
