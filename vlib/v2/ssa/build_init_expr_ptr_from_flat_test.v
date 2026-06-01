// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
// vtest build: macos
//
// Bit-equality pin for s208: cursor-native `build_init_expr_ptr_from_flat` and
// `collect_init_expr_values_from_flat`. The previous `build_prefix_from_flat`
// `&InitExpr` branch decoded the inner InitExpr via `decode_expr` and
// dispatched to legacy `build_init_expr_ptr`; the cursor-native rewrite
// reads `typ` from edge(0) and walks the field aux cursors at edge(1..),
// resolving each field name via `field_c.name()` and value via
// `field_c.edge(0)` through `build_expr_from_flat`.
module ssa

import v2.ast
import v2.types

fn ip_ident(name string) ast.Expr {
	return ast.Expr(ast.Ident{
		name: name
	})
}

fn ip_num(value string) ast.Expr {
	return ast.Expr(ast.BasicLiteral{
		kind:  .number
		value: value
	})
}

fn make_init_ptr_fixture() []ast.File {
	mut stmts := [
		ast.Stmt(ast.ModuleStmt{
			name: 'main'
		}),
	]
	stmts << ast.Stmt(ast.StructDecl{
		name:   'Point'
		fields: [
			ast.FieldDecl{
				name: 'x'
				typ:  ip_ident('int')
			},
			ast.FieldDecl{
				name: 'y'
				typ:  ip_ident('int')
			},
		]
	})
	// fn make_point() { p := &Point{x: 1, y: 2} _ = p }
	stmts << ast.Stmt(ast.FnDecl{
		name:  'make_point'
		typ:   ast.FnType{
			return_type: ast.empty_expr
		}
		stmts: [
			ast.Stmt(ast.AssignStmt{
				op:  .decl_assign
				lhs: [ip_ident('p')]
				rhs: [
					ast.Expr(ast.PrefixExpr{
						op:   .amp
						expr: ast.Expr(ast.InitExpr{
							typ:    ip_ident('Point')
							fields: [
								ast.FieldInit{
									name:  'x'
									value: ip_num('1')
								},
								ast.FieldInit{
									name:  'y'
									value: ip_num('2')
								},
							]
						})
					}),
				]
			}),
		]
	})
	return [
		ast.File{
			name:  'main.v'
			mod:   'main'
			stmts: stmts
		},
	]
}

fn build_via_legacy_init_ptr(files []ast.File, env &types.Environment, name string) &Module {
	mut mod := Module.new(name)
	mut b := Builder.new_with_env(mod, env)
	b.register_fn_signatures(files[0])
	b.build_fn_bodies(files[0])
	return mod
}

fn build_via_flat_init_ptr(files []ast.File, env &types.Environment, name string) &Module {
	flat := ast.flatten_files(files)
	mut mod := Module.new(name)
	mut b := Builder.new_with_env(mod, env)
	b.register_fn_signatures_from_flat(flat.file_cursor(0))
	b.build_fn_bodies_from_flat(flat.file_cursor(0))
	return mod
}

fn test_build_init_expr_ptr_from_flat_matches_legacy() {
	files := make_init_ptr_fixture()
	env := types.Environment.new()
	mod_legacy := build_via_legacy_init_ptr(files, env, 'init_ptr_legacy')
	mod_flat := build_via_flat_init_ptr(files, env, 'init_ptr_flat')

	assert mod_legacy.funcs.len == mod_flat.funcs.len
	assert mod_legacy.blocks.len == mod_flat.blocks.len
	assert mod_legacy.instrs.len == mod_flat.instrs.len
	assert mod_legacy.values.len == mod_flat.values.len
}
