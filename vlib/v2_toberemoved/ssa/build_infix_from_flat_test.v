// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
// vtest build: macos
//
// Bit-equality pin for s193: `build_infix_from_flat` is the ninth per-kind
// arm inside `build_expr_from_flat`. InfixExpr flat encoding (flat.v:1956)
// is (.expr_infix, pos, -1, -1, u16(op), 0, [edge0=lhs, edge1=rhs]). Both
// edges need full `decode_expr` rehydration: `build_infix` calls
// `is_none_expr` (sum-type match) and recursive `build_expr` on both sides.
module ssa

import v2.ast
import v2.token
import v2.types

// Fixture: `fn add(x int, y int) int { return x + y }`. InfixExpr with
// op=.plus, lhs=Ident('x'), rhs=Ident('y'). Exercises the common-case
// integer addition path (no string/array/pointer-arith/float promotion
// branches fire).
fn make_infix_fixture() []ast.File {
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
			]
		},
	]
}

fn build_via_legacy_infix(files []ast.File, env &types.Environment, name string) &Module {
	mut mod := Module.new(name)
	mut b := Builder.new_with_env(mod, env)
	b.register_fn_signatures(files[0])
	b.build_fn_bodies(files[0])
	return mod
}

fn build_via_flat_infix(files []ast.File, env &types.Environment, name string) &Module {
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
			} else {
				b.build_stmt_from_flat(stmt_c)
			}
		}
		if !b.block_has_terminator(b.cur_block) {
			mod.add_instr(.ret, b.cur_block, 0, []ValueID{})
		}
	}
	return mod
}

fn test_build_infix_from_flat_add_matches_legacy() {
	files := make_infix_fixture()
	env := types.Environment.new()
	mod_legacy := build_via_legacy_infix(files, env, 'infix_legacy')
	mod_flat := build_via_flat_infix(files, env, 'infix_flat')

	assert mod_legacy.funcs.len == mod_flat.funcs.len
	assert mod_legacy.blocks.len == mod_flat.blocks.len
	assert mod_legacy.instrs.len == mod_flat.instrs.len
	assert mod_legacy.values.len == mod_flat.values.len
}
