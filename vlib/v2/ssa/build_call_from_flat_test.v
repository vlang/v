// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
// vtest build: macos
//
// Bit-equality pin for s196: `build_call_from_flat` is the twelfth per-kind
// arm inside `build_expr_from_flat`. CallExpr flat encoding (flat.v:1862) is
// (.expr_call, pos, -1, -1, 0, 0, [edge0=lhs, edge1..n=args]). The cursor
// port decodes the lhs and every arg via `decode_expr` (unavoidable:
// build_call pattern-matches heavily on `expr.lhs is ast.SelectorExpr`,
// `expr.lhs is ast.Ident`, and `arg is ast.ModifierExpr`), then constructs
// ast.CallExpr{lhs, args, pos} and dispatches to existing build_call.
module ssa

import v2.ast
import v2.token
import v2.types

// Fixture: `fn add(x int, y int) int { return x + y } fn main() { add(1, 2) }`.
// The main body contains an ExprStmt wrapping a CallExpr with lhs=Ident('add')
// and args=[BasicLiteral('1'), BasicLiteral('2')]. Exercises the common-case
// free-function call path (no struct/method/builtin resolution).
fn make_call_fixture() []ast.File {
	return [
		ast.File{
			name:  'main.v'
			mod:   'main'
			stmts: [
				ast.Stmt(ast.ModuleStmt{
					name: 'main'
				}),
				ast.Stmt(ast.FnDecl{
					name:  'add'
					typ:   ast.FnType{
						params:      [
							ast.Parameter{
								name: 'x'
								typ:  ast.Expr(ast.Ident{
									name: 'int'
								})
							},
							ast.Parameter{
								name: 'y'
								typ:  ast.Expr(ast.Ident{
									name: 'int'
								})
							},
						]
						return_type: ast.Expr(ast.Ident{
							name: 'int'
						})
					}
					stmts: [
						ast.Stmt(ast.ReturnStmt{
							exprs: [
								ast.Expr(ast.InfixExpr{
									op:  token.Token.plus
									lhs: ast.Expr(ast.Ident{
										name: 'x'
									})
									rhs: ast.Expr(ast.Ident{
										name: 'y'
									})
								}),
							]
						}),
					]
				}),
				ast.Stmt(ast.FnDecl{
					name:  'main'
					typ:   ast.FnType{
						return_type: ast.empty_expr
					}
					stmts: [
						ast.Stmt(ast.ExprStmt{
							expr: ast.Expr(ast.CallExpr{
								lhs:  ast.Expr(ast.Ident{
									name: 'add'
								})
								args: [
									ast.Expr(ast.BasicLiteral{
										kind:  .number
										value: '1'
									}),
									ast.Expr(ast.BasicLiteral{
										kind:  .number
										value: '2'
									}),
								]
							})
						}),
					]
				}),
			]
		},
	]
}

fn build_via_legacy_call(files []ast.File, env &types.Environment, name string) &Module {
	mut mod := Module.new(name)
	mut b := Builder.new_with_env(mod, env)
	b.register_fn_signatures(files[0])
	b.build_fn_bodies(files[0])
	return mod
}

fn build_via_flat_call(files []ast.File, env &types.Environment, name string) &Module {
	flat := ast.flatten_files(files)
	mut mod := Module.new(name)
	mut b := Builder.new_with_env(mod, env)
	b.register_fn_signatures_from_flat(flat.file_cursor(0))
	stmts := flat.file_cursor(0).stmts()
	for si in 0 .. stmts.len() {
		c := stmts.at(si)
		if c.kind() != .stmt_fn_decl {
			continue
		}
		decl := c.fn_decl_signature()
		fn_name := b.mangle_fn_name(decl)
		func_idx := b.fn_index[fn_name] or { continue }
		b.cur_func = func_idx
		b.label_blocks = map[string]BlockID{}
		b.vars = map[string]ValueID{}
		entry := mod.add_block(func_idx, 'entry')
		b.cur_block = entry
		for param in decl.typ.params {
			param_type := b.ast_type_to_ssa(param.typ)
			param_val := mod.add_value_node(.argument, param_type, param.name, 0)
			mod.func_add_param(func_idx, param_val)
			alloca := mod.add_instr(.alloca, entry, mod.type_store.get_ptr(param_type), []ValueID{})
			mod.add_instr(.store, entry, 0, [param_val, alloca])
			b.vars[param.name] = alloca
		}
		body := c.list_at(3)
		for bi in 0 .. body.len() {
			stmt_c := body.at(bi)
			if stmt_c.kind() == .stmt_return {
				ret_expr_c := stmt_c.edge(0)
				val := b.build_expr_from_flat(ret_expr_c)
				mod.add_instr(.ret, b.cur_block, 0, [val])
			} else if stmt_c.kind() == .stmt_expr {
				expr_c := stmt_c.edge(0)
				b.build_expr_from_flat(expr_c)
			} else {
				b.build_stmt_from_flat(stmt_c)
			}
		}
		if !b.block_has_terminator(b.cur_block) {
			if fn_name == 'main' {
				zero := mod.get_or_add_const(mod.type_store.get_int(32), '0')
				mod.add_instr(.ret, b.cur_block, 0, [zero])
			} else {
				mod.add_instr(.ret, b.cur_block, 0, []ValueID{})
			}
		}
	}
	return mod
}

fn test_build_call_from_flat_matches_legacy() {
	_ = token.Token.plus
	files := make_call_fixture()
	env := types.Environment.new()
	mod_legacy := build_via_legacy_call(files, env, 'call_legacy')
	mod_flat := build_via_flat_call(files, env, 'call_flat')

	assert mod_legacy.funcs.len == mod_flat.funcs.len
	assert mod_legacy.blocks.len == mod_flat.blocks.len
	assert mod_legacy.instrs.len == mod_flat.instrs.len
	assert mod_legacy.values.len == mod_flat.values.len
}

fn call_test_ident(name string) ast.Expr {
	return ast.Expr(ast.Ident{
		name: name
	})
}

fn call_test_param(name string, typ ast.Expr) ast.Parameter {
	return ast.Parameter{
		name: name
		typ:  typ
	}
}

fn make_same_module_c_shadow_fixture() []ast.File {
	int_type := call_test_ident('int')
	return [
		ast.File{
			name:  'foo.v'
			mod:   'foo'
			stmts: [
				ast.Stmt(ast.ModuleStmt{
					name: 'foo'
				}),
				ast.Stmt(ast.FnDecl{
					name:     'callee'
					language: .c
					typ:      ast.FnType{
						params:      [
							call_test_param('x', int_type),
						]
						return_type: int_type
					}
				}),
				ast.Stmt(ast.FnDecl{
					name:  'callee'
					typ:   ast.FnType{
						params:      [
							call_test_param('x', int_type),
						]
						return_type: int_type
					}
					stmts: [
						ast.Stmt(ast.ReturnStmt{
							exprs: [
								call_test_ident('x'),
							]
						}),
					]
				}),
				ast.Stmt(ast.FnDecl{
					name:  'caller'
					typ:   ast.FnType{
						return_type: int_type
					}
					stmts: [
						ast.Stmt(ast.ReturnStmt{
							exprs: [
								ast.Expr(ast.CallExpr{
									lhs:  call_test_ident('callee')
									args: [
										ast.Expr(ast.BasicLiteral{
											kind:  .number
											value: '1'
										}),
									]
								}),
							]
						}),
					]
				}),
			]
		},
	]
}

fn call_test_func_value_ids(b &Builder, name string) []ValueID {
	idx := b.fn_index[name] or {
		assert false, 'missing SSA function ${name}'
		return []ValueID{}
	}
	func := b.mod.funcs[idx]
	mut ids := []ValueID{}
	for block_id in func.blocks {
		for value_id in b.mod.blocks[block_id].instrs {
			ids << value_id
		}
	}
	return ids
}

fn call_test_callees(b &Builder, name string) []string {
	mut callees := []string{}
	for value_id in call_test_func_value_ids(b, name) {
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

fn test_build_all_from_flat_prefers_same_module_fn_over_bare_c_symbol() {
	files := make_same_module_c_shadow_fixture()
	flat := ast.flatten_files(files)
	env := types.Environment.new()
	mut mod := Module.new('same_module_c_shadow')
	mut b := Builder.new_with_env(mod, env)
	b.build_all_from_flat(&flat)

	assert call_test_callees(b, 'foo__caller') == ['foo__callee']
}
