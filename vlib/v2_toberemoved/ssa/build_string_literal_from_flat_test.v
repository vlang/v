// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
// vtest build: macos
//
// Bit-equality pin for s188: `build_string_literal_from_flat` is the third
// per-kind arm inside `build_expr_from_flat`. StringLiteral flat encoding
// packs `kind` into `aux` (StringLiteralKind enum) and `value` into
// `name_id` — same shape as BasicLiteral, no edges.
module ssa

import v2.ast
import v2.types

// Fixture: a fn that returns a V string literal — `fn greet() string { return "hi" }`.
// ReturnStmt's 0th edge is the StringLiteral cursor.
fn make_string_literal_fixture() []ast.File {
	return [
		ast.File{
			name:  'main.v'
			mod:   'main'
			stmts: [
				ast.Stmt(ast.ModuleStmt{
					name: 'main'
				}),
				ast.Stmt(ast.FnDecl{
					name:  'greet'
					typ:   ast.FnType{
						return_type: ast.Expr(ast.Ident{
							name: 'string'
						})
					}
					stmts: [
						ast.Stmt(ast.ReturnStmt{
							exprs: [
								ast.Expr(ast.StringLiteral{
									kind:  .v
									value: "'hi'"
								}),
							]
						}),
					]
				}),
			]
		},
	]
}

fn build_via_legacy_string_lit(files []ast.File, env &types.Environment, name string) &Module {
	mut mod := Module.new(name)
	mut b := Builder.new_with_env(mod, env)
	b.register_fn_signatures(files[0])
	b.build_fn_bodies(files[0])
	return mod
}

// Flat-side helper routes ReturnStmt's 0th edge through build_expr_from_flat
// which dispatches the StringLiteral cursor to build_string_literal_from_flat.
fn build_via_flat_string_lit(files []ast.File, env &types.Environment, name string) &Module {
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

fn test_build_string_literal_from_flat_v_matches_legacy() {
	files := make_string_literal_fixture()
	env := types.Environment.new()
	mod_legacy := build_via_legacy_string_lit(files, env, 'sl_v_legacy')
	mod_flat := build_via_flat_string_lit(files, env, 'sl_v_flat')

	assert mod_legacy.funcs.len == mod_flat.funcs.len
	assert mod_legacy.funcs.len == 1
	assert mod_legacy.blocks.len == mod_flat.blocks.len
	assert mod_legacy.instrs.len == mod_flat.instrs.len
	assert mod_legacy.values.len == mod_flat.values.len
}

// Pin for kind=.c (C string literal). build_string_literal returns an i8*
// pointer via c_string_literal — exercises a different SSA path than .v.
fn test_build_string_literal_from_flat_c_matches_legacy() {
	files := [
		ast.File{
			name:  'main.v'
			mod:   'main'
			stmts: [
				ast.Stmt(ast.ModuleStmt{
					name: 'main'
				}),
				ast.Stmt(ast.FnDecl{
					name:  'cstr'
					typ:   ast.FnType{
						return_type: ast.Expr(ast.Ident{
							name: 'charptr'
						})
					}
					stmts: [
						ast.Stmt(ast.ReturnStmt{
							exprs: [
								ast.Expr(ast.StringLiteral{
									kind:  .c
									value: "'hi'"
								}),
							]
						}),
					]
				}),
			]
		},
	]
	env := types.Environment.new()
	mod_legacy := build_via_legacy_string_lit(files, env, 'sl_c_legacy')
	mod_flat := build_via_flat_string_lit(files, env, 'sl_c_flat')

	assert mod_legacy.funcs.len == mod_flat.funcs.len
	assert mod_legacy.blocks.len == mod_flat.blocks.len
	assert mod_legacy.instrs.len == mod_flat.instrs.len
	assert mod_legacy.values.len == mod_flat.values.len
}
