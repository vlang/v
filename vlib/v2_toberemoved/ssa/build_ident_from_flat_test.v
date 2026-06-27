// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
// vtest build: macos
//
// Bit-equality pin for s187: `build_ident_from_flat` is the second per-kind
// arm inside `build_expr_from_flat` (after BasicLiteral). The cursor port
// reads `name` from `c.name()` and `pos` from `c.pos()` — Ident has no aux,
// no extra, no edges. Pos preservation keeps the type-checker hit hot.
module ssa

import v2.ast
import v2.token
import v2.types

// Fixture: `mut c := 7; return c`. The ReturnStmt's expr is an Ident
// pointing at the local var. Routing the Ident through build_expr_from_flat
// must build the same SSA as the legacy path.
fn make_ident_fixture() []ast.File {
	return [
		ast.File{
			name:  'main.v'
			mod:   'main'
			stmts: [
				ast.Stmt(ast.ModuleStmt{
					name: 'main'
				}),
				ast.Stmt(ast.FnDecl{
					name:  'get_c'
					typ:   ast.FnType{
						return_type: ast.Expr(ast.Ident{
							name: 'int'
						})
					}
					stmts: [
						ast.Stmt(ast.AssignStmt{
							lhs: [
								ast.Expr(ast.Ident{
									name: 'c'
								}),
							]
							rhs: [
								ast.Expr(ast.BasicLiteral{
									kind:  .number
									value: '7'
								}),
							]
							op:  token.Token.decl_assign
						}),
						ast.Stmt(ast.ReturnStmt{
							exprs: [
								ast.Expr(ast.Ident{
									name: 'c'
								}),
							]
						}),
					]
				}),
			]
		},
	]
}

fn build_via_legacy_ident(files []ast.File, env &types.Environment, name string) &Module {
	mut mod := Module.new(name)
	mut b := Builder.new_with_env(mod, env)
	b.register_fn_signatures(files[0])
	b.build_fn_bodies(files[0])
	return mod
}

// Flat-side helper: walks fn bodies; for ReturnStmt, dispatches the 0th edge
// (the Ident) through build_expr_from_flat instead of decode_expr +
// build_expr. Everything else goes through build_stmt_from_flat.
fn build_via_flat_ident(files []ast.File, env &types.Environment, name string) &Module {
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

fn test_build_ident_from_flat_local_var_matches_legacy() {
	files := make_ident_fixture()
	env := types.Environment.new()
	mod_legacy := build_via_legacy_ident(files, env, 'id_legacy')
	mod_flat := build_via_flat_ident(files, env, 'id_flat')

	assert mod_legacy.funcs.len == mod_flat.funcs.len
	assert mod_legacy.funcs.len == 1
	assert mod_legacy.blocks.len == mod_flat.blocks.len
	assert mod_legacy.instrs.len == mod_flat.instrs.len
	assert mod_legacy.values.len == mod_flat.values.len
}
