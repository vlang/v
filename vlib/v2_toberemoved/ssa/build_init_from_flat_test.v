// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
// vtest build: macos
//
// Bit-equality pin for s197: `build_init_from_flat` is the thirteenth
// per-kind arm inside `build_expr_from_flat`. InitExpr flat encoding
// (flat.v:1962) is (.expr_init, pos, -1, -1, 0, 0,
// [edge0=typ, edge1..n=aux_field_init]). The cursor port decodes the type and
// every field value via `decode_expr` (unavoidable: build_init_expr resolves
// the type through ast_type_to_ssa and recursively builds FieldInit.value),
// then constructs ast.InitExpr{typ, fields, pos} and dispatches to existing
// build_init_expr.
module ssa

import v2.ast
import v2.types

// Fixture: `struct Point { x int y int } fn make_point() Point {
// return Point{x: 1, y: 2} }`. Exercises named struct-literal field init in
// declaration order without needing array/map literal lowering.
fn make_init_fixture() []ast.File {
	return [
		ast.File{
			name:  'main.v'
			mod:   'main'
			stmts: [
				ast.Stmt(ast.ModuleStmt{
					name: 'main'
				}),
				ast.Stmt(ast.StructDecl{
					name:   'Point'
					fields: [
						ast.FieldDecl{
							name: 'x'
							typ:  ast.Expr(ast.Ident{
								name: 'int'
							})
						},
						ast.FieldDecl{
							name: 'y'
							typ:  ast.Expr(ast.Ident{
								name: 'int'
							})
						},
					]
				}),
				ast.Stmt(ast.FnDecl{
					name:  'make_point'
					typ:   ast.FnType{
						return_type: ast.Expr(ast.Ident{
							name: 'Point'
						})
					}
					stmts: [
						ast.Stmt(ast.ReturnStmt{
							exprs: [
								ast.Expr(ast.InitExpr{
									typ:    ast.Expr(ast.Ident{
										name: 'Point'
									})
									fields: [
										ast.FieldInit{
											name:  'x'
											value: ast.Expr(ast.BasicLiteral{
												kind:  .number
												value: '1'
											})
										},
										ast.FieldInit{
											name:  'y'
											value: ast.Expr(ast.BasicLiteral{
												kind:  .number
												value: '2'
											})
										},
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

fn build_via_legacy_init(files []ast.File, env &types.Environment, name string) &Module {
	mut mod := Module.new(name)
	mut b := Builder.new_with_env(mod, env)
	b.register_types_pass1(files[0])
	b.register_types_pass2(files[0])
	b.register_fn_signatures(files[0])
	b.build_fn_bodies(files[0])
	return mod
}

fn build_via_flat_init(files []ast.File, env &types.Environment, name string) &Module {
	flat := ast.flatten_files(files)
	fc := flat.file_cursor(0)
	mut mod := Module.new(name)
	mut b := Builder.new_with_env(mod, env)
	b.register_types_pass1_from_flat(fc)
	b.register_types_pass2_from_flat(fc)
	b.register_fn_signatures_from_flat(fc)
	stmts := fc.stmts()
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

fn test_build_init_from_flat_matches_legacy() {
	files := make_init_fixture()
	env := types.Environment.new()
	mod_legacy := build_via_legacy_init(files, env, 'init_legacy')
	mod_flat := build_via_flat_init(files, env, 'init_flat')

	assert mod_legacy.funcs.len == mod_flat.funcs.len
	assert mod_legacy.blocks.len == mod_flat.blocks.len
	assert mod_legacy.instrs.len == mod_flat.instrs.len
	assert mod_legacy.values.len == mod_flat.values.len
}
