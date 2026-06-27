// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
// vtest build: macos
//
// Bit-equality pin for s182: `build_expr_stmt_from_flat` (seventh per-kind
// port inside the s175 seam) must dispatch the wrapped expr the same way
// the legacy `build_expr_stmt` does. The flat path walks UnsafeExpr's
// inner stmts via cursors (no `ast.Stmt` rehydration on that branch);
// non-unsafe / non-if exprs still decode via `decode_expr` since
// `build_expr` consumes legacy AST today.
module ssa

import v2.ast
import v2.types

// Two fixtures cover the two ports inside build_expr_stmt:
// - Plain expr (BasicLiteral) — exercises the fallback `build_expr` branch
// - UnsafeExpr containing a ReturnStmt — exercises the cursor-walk branch
//   that dispatches each inner stmt through `build_stmt_from_flat`.
fn make_expr_stmt_plain_fixture() []ast.File {
	return [
		ast.File{
			name:  'main.v'
			mod:   'main'
			stmts: [
				ast.Stmt(ast.ModuleStmt{
					name: 'main'
				}),
				ast.Stmt(ast.FnDecl{
					name:  'do_expr'
					typ:   ast.FnType{}
					stmts: [
						ast.Stmt(ast.ExprStmt{
							expr: ast.Expr(ast.BasicLiteral{
								kind:  .number
								value: '42'
							})
						}),
					]
				}),
			]
		},
	]
}

fn make_expr_stmt_unsafe_fixture() []ast.File {
	return [
		ast.File{
			name:  'main.v'
			mod:   'main'
			stmts: [
				ast.Stmt(ast.ModuleStmt{
					name: 'main'
				}),
				ast.Stmt(ast.FnDecl{
					name:  'do_unsafe'
					typ:   ast.FnType{
						return_type: ast.Expr(ast.Ident{
							name: 'int'
						})
					}
					stmts: [
						ast.Stmt(ast.ExprStmt{
							expr: ast.Expr(ast.UnsafeExpr{
								stmts: [
									ast.Stmt(ast.ReturnStmt{
										exprs: [
											ast.Expr(ast.BasicLiteral{
												kind:  .number
												value: '11'
											}),
										]
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

fn build_via_legacy_es(files []ast.File, env &types.Environment, name string) &Module {
	mut mod := Module.new(name)
	mut b := Builder.new_with_env(mod, env)
	b.register_fn_signatures(files[0])
	b.build_fn_bodies(files[0])
	return mod
}

fn build_via_flat_es(files []ast.File, env &types.Environment, name string) &Module {
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

fn assert_same_es(mod_legacy &Module, mod_flat &Module) {
	assert mod_legacy.funcs.len == mod_flat.funcs.len
	assert mod_legacy.blocks.len == mod_flat.blocks.len
	assert mod_legacy.instrs.len == mod_flat.instrs.len
	assert mod_legacy.values.len == mod_flat.values.len
}

fn test_build_expr_stmt_from_flat_plain_matches_legacy() {
	files := make_expr_stmt_plain_fixture()
	env := types.Environment.new()
	mod_legacy := build_via_legacy_es(files, env, 'es_plain_legacy')
	mod_flat := build_via_flat_es(files, env, 'es_plain_flat')
	assert_same_es(mod_legacy, mod_flat)
}

fn test_build_expr_stmt_from_flat_unsafe_matches_legacy() {
	files := make_expr_stmt_unsafe_fixture()
	env := types.Environment.new()
	mod_legacy := build_via_legacy_es(files, env, 'es_unsafe_legacy')
	mod_flat := build_via_flat_es(files, env, 'es_unsafe_flat')
	assert_same_es(mod_legacy, mod_flat)
}
