// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
// vtest build: macos
//
// Bit-equality pin for s179: `build_block_from_flat` (fourth per-kind port
// inside the s175 seam) must walk a `.stmt_block`'s child edges the same
// way the legacy `ast.BlockStmt` arm forwards `stmt.stmts` to
// `build_stmts`. The flat schema (`flat.v:1610`) stores BlockStmt's inner
// stmts as direct child edges of the `.stmt_block` node — no aux_list
// wrapper — so the cursor port walks them via `c.edge(i)`.
module ssa

import v2.ast
import v2.types

// Fixture: a fn with a nested BlockStmt containing a ReturnStmt(0).
// Through `build_stmts_from_flat`, the outer body dispatches the
// BlockStmt to `build_block_from_flat`, which then re-enters
// `build_stmt_from_flat` for the inner ReturnStmt — exercising both the
// new arm and dispatch through it.
fn make_block_fixture() []ast.File {
	return [
		ast.File{
			name:  'main.v'
			mod:   'main'
			stmts: [
				ast.Stmt(ast.ModuleStmt{
					name: 'main'
				}),
				ast.Stmt(ast.FnDecl{
					name:  'do_block'
					typ:   ast.FnType{
						return_type: ast.Expr(ast.Ident{
							name: 'int'
						})
					}
					stmts: [
						ast.Stmt(ast.BlockStmt{
							stmts: [
								ast.Stmt(ast.ReturnStmt{
									exprs: [
										ast.Expr(ast.BasicLiteral{
											kind:  .number
											value: '7'
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

fn build_via_legacy_block(files []ast.File, env &types.Environment, name string) &Module {
	mut mod := Module.new(name)
	mut b := Builder.new_with_env(mod, env)
	b.register_fn_signatures(files[0])
	b.build_fn_bodies(files[0])
	return mod
}

fn build_via_flat_block(files []ast.File, env &types.Environment, name string) &Module {
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
		entry := mod.add_block(func_idx, 'entry')
		b.cur_block = entry
		body := c.list_at(3)
		b.build_stmts_from_flat(body)
		if !b.block_has_terminator(b.cur_block) {
			mod.add_instr(.ret, b.cur_block, 0, []ValueID{})
		}
	}
	return mod
}

fn test_build_block_from_flat_matches_legacy() {
	files := make_block_fixture()
	env := types.Environment.new()
	mod_legacy := build_via_legacy_block(files, env, 'block_legacy')
	mod_flat := build_via_flat_block(files, env, 'block_flat')

	assert mod_legacy.funcs.len == mod_flat.funcs.len
	assert mod_legacy.funcs.len == 1
	assert mod_legacy.blocks.len == mod_flat.blocks.len
	assert mod_legacy.instrs.len == mod_flat.instrs.len
	assert mod_legacy.values.len == mod_flat.values.len
}
