// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
// vtest build: macos
//
// Bit-equality pin for s195: `build_if_from_flat` is the eleventh per-kind
// arm inside `build_expr_from_flat`. IfExpr flat encoding (flat.v:1930) is
// (.expr_if, pos, -1, -1, 0, 0, [edge0=cond, edge1=else_expr,
// edge2..n=stmts]). The cursor port decodes the full IfExpr subtree via
// `decode_expr` and dispatches to existing `build_if_expr` — the inner
// walk is unchanged because `build_if_expr` pattern-matches heavily on
// `node.else_expr` (is ast.EmptyExpr / is ast.IfExpr) and `node.stmts[i]`
// (is ast.ExprStmt) plus calls `infer_if_expr_type` which recurses.
module ssa

import v2.ast
import v2.token
import v2.types

// Fixture: `fn pick(c bool) int { return if c { 1 } else { 2 } }`. IfExpr
// with cond=Ident('c'), then-branch BasicLiteral('1'), else_expr=IfExpr
// (V2 parses pure-else as nested IfExpr with EmptyExpr cond) containing
// BasicLiteral('2'). Common-case if-as-expression path.
fn make_if_fixture() []ast.File {
	return [
		ast.File{
			name:  'main.v'
			mod:   'main'
			stmts: [
				ast.Stmt(ast.ModuleStmt{
					name: 'main'
				}),
				ast.Stmt(ast.FnDecl{
					name:  'pick'
					typ:   ast.FnType{
						params:      [
							ast.Parameter{
								name: 'c'
								typ:  ast.Expr(ast.Ident{
									name: 'bool'
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
								ast.Expr(ast.IfExpr{
									cond:      ast.Expr(ast.Ident{
										name: 'c'
									})
									stmts:     [
										ast.Stmt(ast.ExprStmt{
											expr: ast.Expr(ast.BasicLiteral{
												kind:  .number
												value: '1'
											})
										}),
									]
									else_expr: ast.Expr(ast.IfExpr{
										cond:  ast.empty_expr
										stmts: [
											ast.Stmt(ast.ExprStmt{
												expr: ast.Expr(ast.BasicLiteral{
													kind:  .number
													value: '2'
												})
											}),
										]
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

fn build_via_legacy_if(files []ast.File, env &types.Environment, name string) &Module {
	mut mod := Module.new(name)
	mut b := Builder.new_with_env(mod, env)
	b.register_fn_signatures(files[0])
	b.build_fn_bodies(files[0])
	return mod
}

fn build_via_flat_if(files []ast.File, env &types.Environment, name string) &Module {
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

fn test_build_if_from_flat_matches_legacy() {
	_ = token.Token.amp
	files := make_if_fixture()
	env := types.Environment.new()
	mod_legacy := build_via_legacy_if(files, env, 'if_legacy')
	mod_flat := build_via_flat_if(files, env, 'if_flat')

	assert mod_legacy.funcs.len == mod_flat.funcs.len
	assert mod_legacy.blocks.len == mod_flat.blocks.len
	assert mod_legacy.instrs.len == mod_flat.instrs.len
	assert mod_legacy.values.len == mod_flat.values.len
}
