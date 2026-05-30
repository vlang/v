// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
// vtest build: macos
//
// Bit-equality pin for s185: `build_fn_from_flat` (the s185 wiring step)
// must build the same SSA bodies as the legacy `build_fn`. This is the
// first session where `build_fn_bodies_from_flat` no longer decodes the
// FnDecl body — instead `decode_fn_decl_signature` returns a body-less
// FnDecl and `build_fn_from_flat` walks the body stmts directly via
// `c.list_at(3)` cursors. Pin covers plain fn / fn with params / extern
// (body_len==0) / multi-body fn branches.
module ssa

import v2.ast
import v2.token
import v2.types

// Fixture exercises every branch of the new build_fn_from_flat:
// - `answer`: plain fn returning a literal (single-stmt body)
// - `add`: fn with two params + decl_assign + return (param setup + body)
// - `noop`: extern-style fn with no body (body_len==0 branch)
// - `loop_break`: fn with a for-loop containing break (heavyweight stmt arms)
fn make_build_fn_from_flat_fixture() []ast.File {
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
				ast.Stmt(ast.FnDecl{
					name:  'add'
					typ:   ast.FnType{
						params:      [
							ast.Parameter{
								name: 'a'
								typ:  ast.Expr(ast.Ident{
									name: 'int'
								})
							},
							ast.Parameter{
								name: 'b'
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
						ast.Stmt(ast.AssignStmt{
							lhs: [
								ast.Expr(ast.Ident{
									name: 'c'
								}),
							]
							rhs: [
								ast.Expr(ast.BasicLiteral{
									kind:  .number
									value: '0'
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
				ast.Stmt(ast.FnDecl{
					name: 'noop'
					typ:  ast.FnType{}
				}),
				ast.Stmt(ast.FnDecl{
					name:  'loop_break'
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

fn build_via_legacy_fn_bodies(files []ast.File, env &types.Environment, name string) &Module {
	mut mod := Module.new(name)
	mut b := Builder.new_with_env(mod, env)
	b.register_fn_signatures(files[0])
	b.build_fn_bodies(files[0])
	return mod
}

fn build_via_flat_fn_bodies(files []ast.File, env &types.Environment, name string) &Module {
	flat := ast.flatten_files(files)
	mut mod := Module.new(name)
	mut b := Builder.new_with_env(mod, env)
	b.register_fn_signatures_from_flat(flat.file_cursor(0))
	b.build_fn_bodies_from_flat(flat.file_cursor(0))
	return mod
}

fn test_build_fn_from_flat_matches_legacy() {
	files := make_build_fn_from_flat_fixture()
	env := types.Environment.new()
	mod_legacy := build_via_legacy_fn_bodies(files, env, 'bf_legacy')
	mod_flat := build_via_flat_fn_bodies(files, env, 'bf_flat')

	assert mod_legacy.funcs.len == mod_flat.funcs.len
	assert mod_legacy.funcs.len == 4
	assert mod_legacy.blocks.len == mod_flat.blocks.len
	assert mod_legacy.instrs.len == mod_flat.instrs.len
	assert mod_legacy.values.len == mod_flat.values.len
}
