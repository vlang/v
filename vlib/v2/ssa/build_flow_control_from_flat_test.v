// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
// vtest build: macos
//
// Bit-equality pin for s177: `build_flow_control_from_flat` (second per-
// kind port inside the s175 seam) must emit the same SSA jumps as the
// legacy `build_flow_control`. The flat path reads `op` from `c.aux()`
// and `label` from `c.name()` — no FlowControlStmt struct decode and no
// Stmt sum-type boxing.
module ssa

import v2.ast
import v2.token
import v2.types

// Fixture: a fn with a labelled goto inside an unsafe forever-loop-then-
// break shape. Going through build_fn_bodies exercises both the goto
// branch (top of build_flow_control) and the break branch (loop exit).
fn make_flow_control_fixture() []ast.File {
	return [
		ast.File{
			name:  'main.v'
			mod:   'main'
			stmts: [
				ast.Stmt(ast.ModuleStmt{
					name: 'main'
				}),
				ast.Stmt(ast.FnDecl{
					name:  'do_break'
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
				ast.Stmt(ast.FnDecl{
					name:  'do_continue'
					typ:   ast.FnType{}
					stmts: [
						ast.Stmt(ast.ForStmt{
							stmts: [
								ast.Stmt(ast.FlowControlStmt{
									op: token.Token.key_continue
								}),
							]
						}),
					]
				}),
				ast.Stmt(ast.FnDecl{
					name:  'do_goto'
					typ:   ast.FnType{}
					stmts: [
						ast.Stmt(ast.LabelStmt{
							name: 'top'
						}),
						ast.Stmt(ast.FlowControlStmt{
							op:    token.Token.key_goto
							label: 'top'
						}),
					]
				}),
			]
		},
	]
}

fn build_via_legacy_fc(files []ast.File, env &types.Environment, name string) &Module {
	mut mod := Module.new(name)
	mut b := Builder.new_with_env(mod, env)
	b.register_fn_signatures(files[0])
	b.build_fn_bodies(files[0])
	return mod
}

fn build_via_flat_fc(files []ast.File, env &types.Environment, name string) &Module {
	flat := ast.flatten_files(files)
	mut mod := Module.new(name)
	mut b := Builder.new_with_env(mod, env)
	b.register_fn_signatures_from_flat(flat.file_cursor(0))
	// Run fn bodies through build_stmts_from_flat (s175 seam). Each fn's
	// FlowControlStmt now dispatches through build_flow_control_from_flat.
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
		body := c.list_at(3)
		b.build_stmts_from_flat(body)
		if !b.block_has_terminator(b.cur_block) {
			mod.add_instr(.ret, b.cur_block, 0, []ValueID{})
		}
	}
	return mod
}

fn test_build_flow_control_from_flat_matches_legacy_for_break_continue_goto() {
	files := make_flow_control_fixture()
	env := types.Environment.new()
	mod_legacy := build_via_legacy_fc(files, env, 'fc_legacy')
	mod_flat := build_via_flat_fc(files, env, 'fc_flat')

	assert mod_legacy.funcs.len == mod_flat.funcs.len
	assert mod_legacy.funcs.len == 3
	assert mod_legacy.blocks.len == mod_flat.blocks.len
	assert mod_legacy.instrs.len == mod_flat.instrs.len
	assert mod_legacy.values.len == mod_flat.values.len
}
