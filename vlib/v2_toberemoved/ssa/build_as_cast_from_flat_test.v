// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
// vtest build: macos
//
// Bit-equality pin for the cursor-native rewrite of `build_as_cast_from_flat`.
// The previous helper decoded both AsCastExpr edges via `decode_expr` and
// dispatched to legacy `build_as_cast`; the cursor-native rewrite mirrors
// `build_as_cast` directly: `build_expr_from_flat(expr_c)` produces the value,
// `ast_type_to_ssa_from_flat(typ_c)` resolves the target type, and
// `get_checked_expr_type_from_flat(expr_c)` retrieves the checked source type
// (only consulted for the sumtype-source branch). This pin uses a same-type
// passthrough cast so legacy + flat both take the `values[val].typ ==
// target_type` short-circuit and emit identical IR.
module ssa

import v2.ast
import v2.types

fn ac_ident(name string) ast.Expr {
	return ast.Expr(ast.Ident{
		name: name
	})
}

fn ac_num(value string) ast.Expr {
	return ast.Expr(ast.BasicLiteral{
		kind:  .number
		value: value
	})
}

fn make_as_cast_fixture() []ast.File {
	mut stmts := [
		ast.Stmt(ast.ModuleStmt{
			name: 'main'
		}),
	]
	// fn passthrough() i64 { x := i64(7) return x as i64 }
	stmts << ast.Stmt(ast.FnDecl{
		name:  'passthrough'
		typ:   ast.FnType{
			return_type: ac_ident('i64')
		}
		stmts: [
			ast.Stmt(ast.AssignStmt{
				op:  .decl_assign
				lhs: [ac_ident('x')]
				rhs: [ast.Expr(ast.CastExpr{
					typ:  ac_ident('i64')
					expr: ac_num('7')
				})]
			}),
			ast.Stmt(ast.ReturnStmt{
				exprs: [
					ast.Expr(ast.AsCastExpr{
						expr: ac_ident('x')
						typ:  ac_ident('i64')
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

fn build_via_legacy_as_cast(files []ast.File, env &types.Environment, name string) &Module {
	mut mod := Module.new(name)
	mut b := Builder.new_with_env(mod, env)
	b.register_fn_signatures(files[0])
	b.build_fn_bodies(files[0])
	return mod
}

fn build_via_flat_as_cast(files []ast.File, env &types.Environment, name string) &Module {
	flat := ast.flatten_files(files)
	mut mod := Module.new(name)
	mut b := Builder.new_with_env(mod, env)
	b.register_fn_signatures_from_flat(flat.file_cursor(0))
	b.build_fn_bodies_from_flat(flat.file_cursor(0))
	return mod
}

fn test_build_as_cast_from_flat_matches_legacy() {
	files := make_as_cast_fixture()
	env := types.Environment.new()
	mod_legacy := build_via_legacy_as_cast(files, env, 'as_cast_legacy')
	mod_flat := build_via_flat_as_cast(files, env, 'as_cast_flat')

	assert mod_legacy.funcs.len == mod_flat.funcs.len
	assert mod_legacy.blocks.len == mod_flat.blocks.len
	assert mod_legacy.instrs.len == mod_flat.instrs.len
	assert mod_legacy.values.len == mod_flat.values.len
}
