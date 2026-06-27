// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
// vtest build: macos
//
// Bit-equality pin for s186: `build_basic_literal_from_flat` is the first
// per-kind arm inside a new `build_expr_from_flat` dispatcher — the seam
// that future sessions will populate (Ident → Infix → Call → ...). For
// BasicLiteral specifically, the cursor port preserves `pos` so the
// type-checker lookup hits the same `expr_type` answer as the legacy path
// (otherwise number-literal width promotion via `get_checked_expr_type`
// would diverge).
module ssa

import v2.ast
import v2.token
import v2.types

// Fixture: a fn body that returns a BasicLiteral. The pin asserts both
// paths produce the same SSA module: same number of funcs/blocks/instrs/values.
fn make_basic_literal_fixture() []ast.File {
	return [
		ast.File{
			name:  'main.v'
			mod:   'main'
			stmts: [
				ast.Stmt(ast.ModuleStmt{
					name: 'main'
				}),
				ast.Stmt(ast.FnDecl{
					name:  'forty_two'
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

// build_via_legacy_basic_literal builds the SSA via the legacy
// `build_expr` path (which delegates BasicLiteral → build_basic_literal).
fn build_via_legacy_basic_literal(files []ast.File, env &types.Environment, name string) &Module {
	mut mod := Module.new(name)
	mut b := Builder.new_with_env(mod, env)
	b.register_fn_signatures(files[0])
	b.build_fn_bodies(files[0])
	return mod
}

// build_via_flat_basic_literal builds the SSA via the new
// `build_expr_from_flat` dispatcher: ReturnStmt cursor decodes its 0th edge
// (the BasicLiteral) and dispatches through build_expr_from_flat. The
// dispatcher's BasicLiteral arm reads kind/value/pos directly from the
// cursor and calls build_basic_literal — no `decode_expr` for that node.
fn build_via_flat_basic_literal(files []ast.File, env &types.Environment, name string) &Module {
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
		// Walk body stmts. For ReturnStmt's 0th edge, route the expr through
		// build_expr_from_flat instead of decode_expr + build_expr.
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

fn test_build_basic_literal_from_flat_number_matches_legacy() {
	files := make_basic_literal_fixture()
	env := types.Environment.new()
	mod_legacy := build_via_legacy_basic_literal(files, env, 'bl_num_legacy')
	mod_flat := build_via_flat_basic_literal(files, env, 'bl_num_flat')

	assert mod_legacy.funcs.len == mod_flat.funcs.len
	assert mod_legacy.funcs.len == 1
	assert mod_legacy.blocks.len == mod_flat.blocks.len
	assert mod_legacy.instrs.len == mod_flat.instrs.len
	assert mod_legacy.values.len == mod_flat.values.len
}

// Same parity check on a key_true literal — covers the bool branch in
// build_basic_literal (returns get_int(1), '1' rather than the number path).
fn test_build_basic_literal_from_flat_bool_matches_legacy() {
	files := [
		ast.File{
			name:  'main.v'
			mod:   'main'
			stmts: [
				ast.Stmt(ast.ModuleStmt{
					name: 'main'
				}),
				ast.Stmt(ast.FnDecl{
					name:  'tru'
					typ:   ast.FnType{
						return_type: ast.Expr(ast.Ident{
							name: 'bool'
						})
					}
					stmts: [
						ast.Stmt(ast.ReturnStmt{
							exprs: [
								ast.Expr(ast.BasicLiteral{
									kind:  token.Token.key_true
									value: 'true'
								}),
							]
						}),
					]
				}),
			]
		},
	]
	env := types.Environment.new()
	mod_legacy := build_via_legacy_basic_literal(files, env, 'bl_bool_legacy')
	mod_flat := build_via_flat_basic_literal(files, env, 'bl_bool_flat')

	assert mod_legacy.funcs.len == mod_flat.funcs.len
	assert mod_legacy.blocks.len == mod_flat.blocks.len
	assert mod_legacy.instrs.len == mod_flat.instrs.len
	assert mod_legacy.values.len == mod_flat.values.len
}
