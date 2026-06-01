// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
// vtest build: macos
//
// Bit-equality pin for the cursor-native rewrite of `build_array_init_from_flat`.
// The previous helper rehydrated every ArrayInitExpr field via `decode_expr`
// (typ, init, cap, len, update_expr, and each element) and dispatched to
// legacy `build_array_init_expr`. The cursor-native rewrite mirrors
// `build_array_init_expr` directly: elements walk `c.edge(5)..edge_count()` via
// `build_expr_from_flat`; the declared element type is resolved from `typ_c`
// using a `.typ_array` / `.typ_array_fixed` kind match (`edge(0)` / `edge(1)`
// for elem_type) plus an `ast_type_to_ssa_from_flat` call; the fixed-array
// marker check inspects `len_c.kind() == .expr_postfix` with `aux == .not`
// (the `!` suffix) or `typ_c.kind() == .typ_array_fixed`. The `init`, `cap`,
// and `update_expr` edges are never read — legacy `build_array_init_expr`
// never consults them. Pin asserts module-count parity for the common literal
// `[1, 2, 3]` path.
module ssa

import v2.ast
import v2.types

fn ai_ident(name string) ast.Expr {
	return ast.Expr(ast.Ident{
		name: name
	})
}

fn ai_num(value string) ast.Expr {
	return ast.Expr(ast.BasicLiteral{
		kind:  .number
		value: value
	})
}

fn make_array_init_fixture() []ast.File {
	mut stmts := [
		ast.Stmt(ast.ModuleStmt{
			name: 'main'
		}),
	]
	// fn make_arr() { a := [1, 2, 3] _ = a }
	stmts << ast.Stmt(ast.FnDecl{
		name:  'make_arr'
		typ:   ast.FnType{
			return_type: ast.empty_expr
		}
		stmts: [
			ast.Stmt(ast.AssignStmt{
				op:  .decl_assign
				lhs: [ai_ident('a')]
				rhs: [
					ast.Expr(ast.ArrayInitExpr{
						exprs: [ai_num('1'), ai_num('2'), ai_num('3')]
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

fn build_via_legacy_array_init(files []ast.File, env &types.Environment, name string) &Module {
	mut mod := Module.new(name)
	mut b := Builder.new_with_env(mod, env)
	b.register_fn_signatures(files[0])
	b.build_fn_bodies(files[0])
	return mod
}

fn build_via_flat_array_init(files []ast.File, env &types.Environment, name string) &Module {
	flat := ast.flatten_files(files)
	mut mod := Module.new(name)
	mut b := Builder.new_with_env(mod, env)
	b.register_fn_signatures_from_flat(flat.file_cursor(0))
	b.build_fn_bodies_from_flat(flat.file_cursor(0))
	return mod
}

fn test_build_array_init_from_flat_matches_legacy() {
	files := make_array_init_fixture()
	env := types.Environment.new()
	mod_legacy := build_via_legacy_array_init(files, env, 'array_init_legacy')
	mod_flat := build_via_flat_array_init(files, env, 'array_init_flat')

	assert mod_legacy.funcs.len == mod_flat.funcs.len
	assert mod_legacy.blocks.len == mod_flat.blocks.len
	assert mod_legacy.instrs.len == mod_flat.instrs.len
	assert mod_legacy.values.len == mod_flat.values.len
}
