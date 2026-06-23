// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
// vtest build: macos
//
// Bit-equality pin for s181: `build_assert_from_flat` (sixth per-kind port
// inside the s175 seam) must emit the same SSA `.br` + assert_fail
// (call exit(1) + .unreachable) shape as the legacy `build_assert`. The
// flat path decodes only the assert condition (edge 0) — the AssertStmt
// struct itself is never rehydrated, and the optional `extra` message
// (edge 1) is ignored on both paths since the legacy build_assert only
// reads stmt.expr.
module ssa

import v2.ast
import v2.types

// Fixture: a fn `do_assert` containing `assert true`. The condition is a
// BasicLiteral so build_expr stays trivial. The shape of the emitted SSA
// (br + assert_pass + assert_fail + call exit(1) + unreachable) is what we
// pin — both paths must produce the same blocks/instrs/values counts.
fn make_assert_fixture() []ast.File {
	return [
		ast.File{
			name:  'main.v'
			mod:   'main'
			stmts: [
				ast.Stmt(ast.ModuleStmt{
					name: 'main'
				}),
				ast.Stmt(ast.FnDecl{
					name:  'do_assert'
					typ:   ast.FnType{}
					stmts: [
						ast.Stmt(ast.AssertStmt{
							expr: ast.Expr(ast.BasicLiteral{
								kind:  .key_true
								value: 'true'
							})
						}),
					]
				}),
			]
		},
	]
}

fn build_via_legacy_assert(files []ast.File, env &types.Environment, name string) &Module {
	mut mod := Module.new(name)
	mut b := Builder.new_with_env(mod, env)
	b.register_fn_signatures(files[0])
	b.build_fn_bodies(files[0])
	return mod
}

fn build_via_flat_assert(files []ast.File, env &types.Environment, name string) &Module {
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

fn test_build_assert_from_flat_matches_legacy() {
	files := make_assert_fixture()
	env := types.Environment.new()
	mod_legacy := build_via_legacy_assert(files, env, 'assert_legacy')
	mod_flat := build_via_flat_assert(files, env, 'assert_flat')

	assert mod_legacy.funcs.len == mod_flat.funcs.len
	assert mod_legacy.funcs.len == 1
	assert mod_legacy.blocks.len == mod_flat.blocks.len
	assert mod_legacy.instrs.len == mod_flat.instrs.len
	assert mod_legacy.values.len == mod_flat.values.len
}
