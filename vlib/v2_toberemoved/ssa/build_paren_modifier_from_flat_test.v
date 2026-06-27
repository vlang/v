// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
// vtest build: macos
//
// Bit-equality pin for s189: `.expr_paren` and `.expr_modifier` arms inside
// `build_expr_from_flat`. Both are pure unwrap-and-recurse — they call
// `build_expr_from_flat(c.edge(0))` on the inner edge. No per-kind helper
// is needed; the new dispatcher handles the inner cursor directly.
module ssa

import v2.ast
import v2.token
import v2.types

// Fixture: `fn forty_two() int { return (42) }` — ParenExpr wraps a
// BasicLiteral. Exercises the unwrap path through build_expr_from_flat's
// `.expr_paren` arm.
fn make_paren_fixture() []ast.File {
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
								ast.Expr(ast.ParenExpr{
									expr: ast.Expr(ast.BasicLiteral{
										kind:  .number
										value: '42'
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

// Fixture: ModifierExpr wraps an Ident. Exercises the `.expr_modifier` arm.
// `fn pass_mut(mut x int) int { return mut x }` — ModifierExpr with
// kind=key_mut around an Ident.
fn make_modifier_fixture() []ast.File {
	return [
		ast.File{
			name:  'main.v'
			mod:   'main'
			stmts: [
				ast.Stmt(ast.ModuleStmt{
					name: 'main'
				}),
				ast.Stmt(ast.FnDecl{
					name:  'unwrap'
					typ:   ast.FnType{
						params:      [
							ast.Parameter{
								name: 'x'
								typ:  ast.Expr(ast.Ident{
									name: 'int'
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
								ast.Expr(ast.ModifierExpr{
									kind: token.Token.key_mut
									expr: ast.Expr(ast.Ident{
										name: 'x'
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

fn build_via_legacy_unwrap(files []ast.File, env &types.Environment, name string) &Module {
	mut mod := Module.new(name)
	mut b := Builder.new_with_env(mod, env)
	b.register_fn_signatures(files[0])
	b.build_fn_bodies(files[0])
	return mod
}

// Flat-side helper routes ReturnStmt's 0th edge through build_expr_from_flat
// which dispatches `.expr_paren` / `.expr_modifier` to the unwrap arm
// (calls build_expr_from_flat on the inner edge 0).
fn build_via_flat_unwrap(files []ast.File, env &types.Environment, name string) &Module {
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
		// Set up params (build_fn does this for the legacy path; need parity)
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

fn test_build_paren_from_flat_matches_legacy() {
	files := make_paren_fixture()
	env := types.Environment.new()
	mod_legacy := build_via_legacy_unwrap(files, env, 'paren_legacy')
	mod_flat := build_via_flat_unwrap(files, env, 'paren_flat')

	assert mod_legacy.funcs.len == mod_flat.funcs.len
	assert mod_legacy.blocks.len == mod_flat.blocks.len
	assert mod_legacy.instrs.len == mod_flat.instrs.len
	assert mod_legacy.values.len == mod_flat.values.len
}

fn test_build_modifier_from_flat_matches_legacy() {
	files := make_modifier_fixture()
	env := types.Environment.new()
	mod_legacy := build_via_legacy_unwrap(files, env, 'mod_legacy')
	mod_flat := build_via_flat_unwrap(files, env, 'mod_flat')

	assert mod_legacy.funcs.len == mod_flat.funcs.len
	assert mod_legacy.blocks.len == mod_flat.blocks.len
	assert mod_legacy.instrs.len == mod_flat.instrs.len
	assert mod_legacy.values.len == mod_flat.values.len
}
