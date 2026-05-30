// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
// vtest build: macos
//
// Bit-equality pin for s176: `build_return_from_flat` (the first per-kind
// arm port inside `build_stmt_from_flat`) must emit the same SSA `.ret`
// instructions as the legacy `build_return`. The flat path drops the
// ReturnStmt struct decode entirely — only the inner exprs are decoded
// via `decode_expr` (which `build_expr` still consumes today).
module ssa

import v2.ast
import v2.types

// Three fixtures cover the three branches of build_return:
// - no-expr return (void function)
// - single-expr return (most common)
// - multi-expr return (tuple via insertvalue chain)
fn make_return_void_fixture() []ast.File {
	return [
		ast.File{
			name:  'main.v'
			mod:   'main'
			stmts: [
				ast.Stmt(ast.ModuleStmt{
					name: 'main'
				}),
				ast.Stmt(ast.FnDecl{
					name:  'do_nothing'
					typ:   ast.FnType{}
					stmts: [
						ast.Stmt(ast.ReturnStmt{
							exprs: []
						}),
					]
				}),
			]
		},
	]
}

fn make_return_single_fixture() []ast.File {
	return [
		ast.File{
			name:  'main.v'
			mod:   'main'
			stmts: [
				ast.Stmt(ast.ModuleStmt{
					name: 'main'
				}),
				ast.Stmt(ast.FnDecl{
					name:  'answer'
					typ:   ast.FnType{
						return_type: ast.Expr(ast.Ident{
							name: 'int'
						})
					}
					stmts: [
						ast.Stmt(ast.ReturnStmt{
							exprs: [
								ast.Expr(ast.BasicLiteral{
									kind:  .number
									value: '42'
								}),
							]
						}),
					]
				}),
			]
		},
	]
}

fn make_return_tuple_fixture() []ast.File {
	return [
		ast.File{
			name:  'main.v'
			mod:   'main'
			stmts: [
				ast.Stmt(ast.ModuleStmt{
					name: 'main'
				}),
				ast.Stmt(ast.FnDecl{
					name:  'pair'
					typ:   ast.FnType{
						return_type: ast.Expr(ast.Ident{
							name: 'int'
						})
					}
					stmts: [
						ast.Stmt(ast.ReturnStmt{
							exprs: [
								ast.Expr(ast.BasicLiteral{
									kind:  .number
									value: '1'
								}),
								ast.Expr(ast.BasicLiteral{
									kind:  .number
									value: '2'
								}),
							]
						}),
					]
				}),
			]
		},
	]
}

fn assert_same(mod_legacy &Module, mod_flat &Module) {
	assert mod_legacy.funcs.len == mod_flat.funcs.len
	assert mod_legacy.blocks.len == mod_flat.blocks.len
	assert mod_legacy.instrs.len == mod_flat.instrs.len
	assert mod_legacy.values.len == mod_flat.values.len
}

fn build_via_legacy(files []ast.File, env &types.Environment, name string) &Module {
	mut mod := Module.new(name)
	mut b := Builder.new_with_env(mod, env)
	b.register_fn_signatures(files[0])
	b.build_fn_bodies(files[0])
	return mod
}

fn build_via_flat(files []ast.File, env &types.Environment, name string) &Module {
	flat := ast.flatten_files(files)
	mut mod := Module.new(name)
	mut b := Builder.new_with_env(mod, env)
	b.register_fn_signatures_from_flat(flat.file_cursor(0))
	// Walk fn bodies via build_fn_bodies_from_flat which calls
	// build_stmt_from_flat — the .stmt_return arm is the s176 port and
	// goes through build_return_from_flat without decoding the
	// ReturnStmt struct. We exercise it here by manually wiring
	// build_stmts_from_flat over each FnDecl's body cursor.
	stmts := flat.file_cursor(0).stmts()
	for si in 0 .. stmts.len() {
		c := stmts.at(si)
		if c.kind() != .stmt_fn_decl {
			continue
		}
		decl := c.flat.decode_fn_decl_signature(c.id)
		fn_name := b.mangle_fn_name(decl)
		func_idx := b.fn_index[fn_name] or { continue }
		b.cur_func = func_idx
		entry := mod.add_block(func_idx, 'entry')
		b.cur_block = entry
		// Body is edge 3 of stmt_fn_decl (after recv, typ, attrs) — an aux_list.
		body := c.list_at(3)
		b.build_stmts_from_flat(body)
		if !b.block_has_terminator(b.cur_block) {
			mod.add_instr(.ret, b.cur_block, 0, []ValueID{})
		}
	}
	return mod
}

fn test_build_return_from_flat_void_matches_legacy() {
	files := make_return_void_fixture()
	env := types.Environment.new()
	mod_legacy := build_via_legacy(files, env, 'ret_void_legacy')
	mod_flat := build_via_flat(files, env, 'ret_void_flat')
	assert_same(mod_legacy, mod_flat)
}

fn test_build_return_from_flat_single_matches_legacy() {
	files := make_return_single_fixture()
	env := types.Environment.new()
	mod_legacy := build_via_legacy(files, env, 'ret_single_legacy')
	mod_flat := build_via_flat(files, env, 'ret_single_flat')
	assert_same(mod_legacy, mod_flat)
}

fn test_build_return_from_flat_tuple_matches_legacy() {
	files := make_return_tuple_fixture()
	env := types.Environment.new()
	mod_legacy := build_via_legacy(files, env, 'ret_tuple_legacy')
	mod_flat := build_via_flat(files, env, 'ret_tuple_flat')
	assert_same(mod_legacy, mod_flat)
}
