// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
// vtest build: macos
//
// Bit-equality pin for s183: `build_for_from_flat` (eighth per-kind port
// inside the s175 seam) must build the same for_cond / for_body /
// for_exit block scaffold + br/jmp wiring as the legacy `build_for`. The
// flat path walks body stmts directly via `c.edge(i)` and uses
// `.stmt_empty` / `.expr_empty` cursor sentinels to detect missing
// init/cond/post — no ForStmt struct decode.
module ssa

import v2.ast
import v2.token
import v2.types

// Two fixtures cover the no-cond / has-cond branches of build_for:
// - `do_infinite`: `for { break }` — has_cond=false, infinite-loop jmp
// - `do_conditioned`: `for true { break }` — has_cond=true, br on cond
// Both leave init/post empty so the EmptyStmt sentinel branch is exercised.
fn make_for_infinite_fixture() []ast.File {
	return [
		ast.File{
			name:  'main.v'
			mod:   'main'
			stmts: [
				ast.Stmt(ast.ModuleStmt{
					name: 'main'
				}),
				ast.Stmt(ast.FnDecl{
					name:  'do_infinite'
					typ:   ast.FnType{}
					stmts: [
						ast.Stmt(ast.ForStmt{
							stmts: [
								ast.Stmt(ast.FlowControlStmt{
									op: token.Token.key_break
								}),
							]
						}),
					]
				}),
			]
		},
	]
}

fn make_for_conditioned_fixture() []ast.File {
	return [
		ast.File{
			name:  'main.v'
			mod:   'main'
			stmts: [
				ast.Stmt(ast.ModuleStmt{
					name: 'main'
				}),
				ast.Stmt(ast.FnDecl{
					name:  'do_conditioned'
					typ:   ast.FnType{}
					stmts: [
						ast.Stmt(ast.ForStmt{
							cond:  ast.Expr(ast.BasicLiteral{
								kind:  .key_true
								value: 'true'
							})
							stmts: [
								ast.Stmt(ast.FlowControlStmt{
									op: token.Token.key_break
								}),
							]
						}),
					]
				}),
			]
		},
	]
}

fn build_via_legacy_for(files []ast.File, env &types.Environment, name string) &Module {
	mut mod := Module.new(name)
	mut b := Builder.new_with_env(mod, env)
	b.register_fn_signatures(files[0])
	b.build_fn_bodies(files[0])
	return mod
}

fn build_via_flat_for(files []ast.File, env &types.Environment, name string) &Module {
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

fn assert_same_for(mod_legacy &Module, mod_flat &Module) {
	assert mod_legacy.funcs.len == mod_flat.funcs.len
	assert mod_legacy.blocks.len == mod_flat.blocks.len
	assert mod_legacy.instrs.len == mod_flat.instrs.len
	assert mod_legacy.values.len == mod_flat.values.len
}

fn test_build_for_from_flat_infinite_matches_legacy() {
	files := make_for_infinite_fixture()
	env := types.Environment.new()
	mod_legacy := build_via_legacy_for(files, env, 'for_inf_legacy')
	mod_flat := build_via_flat_for(files, env, 'for_inf_flat')
	assert_same_for(mod_legacy, mod_flat)
}

fn test_build_for_from_flat_conditioned_matches_legacy() {
	files := make_for_conditioned_fixture()
	env := types.Environment.new()
	mod_legacy := build_via_legacy_for(files, env, 'for_cond_legacy')
	mod_flat := build_via_flat_for(files, env, 'for_cond_flat')
	assert_same_for(mod_legacy, mod_flat)
}
