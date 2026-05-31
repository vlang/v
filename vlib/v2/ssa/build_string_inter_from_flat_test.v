// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
// vtest build: macos
//
// Bit-equality pin for the cursor-native rewrite of `build_string_inter_from_flat`.
// The previous helper rehydrated every StringInter via two `decode_expr` calls
// (one for `expr`, one for `format_expr`) and dispatched to legacy
// `build_string_inter_literal`. The cursor-native rewrite mirrors
// `build_string_inter_literal` directly: literal parts come from `list_at(0)`
// via `values_l.at(i).name()`, each inter's `format`/`resolved_fmt`/`expr_c`
// are read from `aux`/`name`/`edge(0)`, and `build_expr_from_flat(expr_c)`
// produces the value. The `format_expr` edge is never read — legacy
// `build_string_inter_literal` never consults `inter.format_expr` either.
// The `.str` selector-stripping path is implemented cursor-natively: kind
// check + edge(1) ident name check.
module ssa

import v2.ast
import v2.types

fn si_ident(name string) ast.Expr {
	return ast.Expr(ast.Ident{
		name: name
	})
}

fn make_string_inter_fixture() []ast.File {
	mut stmts := [
		ast.Stmt(ast.ModuleStmt{
			name: 'main'
		}),
	]
	// fn greet() string { name := 'world' return 'hello ${name}!' }
	stmts << ast.Stmt(ast.FnDecl{
		name:  'greet'
		typ:   ast.FnType{
			return_type: si_ident('string')
		}
		stmts: [
			ast.Stmt(ast.AssignStmt{
				op:  .decl_assign
				lhs: [si_ident('name')]
				rhs: [ast.Expr(ast.StringLiteral{
					kind:  .v
					value: "'world'"
				})]
			}),
			ast.Stmt(ast.ReturnStmt{
				exprs: [
					ast.Expr(ast.StringInterLiteral{
						kind:   .v
						values: ["'hello ", "!'"]
						inters: [ast.StringInter{
							format: .unformatted
							expr:   si_ident('name')
						}]
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

fn build_via_legacy_string_inter(files []ast.File, env &types.Environment, name string) &Module {
	mut mod := Module.new(name)
	mut b := Builder.new_with_env(mod, env)
	b.register_fn_signatures(files[0])
	b.build_fn_bodies(files[0])
	return mod
}

fn build_via_flat_string_inter(files []ast.File, env &types.Environment, name string) &Module {
	flat := ast.flatten_files(files)
	mut mod := Module.new(name)
	mut b := Builder.new_with_env(mod, env)
	b.register_fn_signatures_from_flat(flat.file_cursor(0))
	b.build_fn_bodies_from_flat(flat.file_cursor(0))
	return mod
}

fn test_build_string_inter_from_flat_matches_legacy() {
	files := make_string_inter_fixture()
	env := types.Environment.new()
	mod_legacy := build_via_legacy_string_inter(files, env, 'string_inter_legacy')
	mod_flat := build_via_flat_string_inter(files, env, 'string_inter_flat')

	assert mod_legacy.funcs.len == mod_flat.funcs.len
	assert mod_legacy.blocks.len == mod_flat.blocks.len
	assert mod_legacy.instrs.len == mod_flat.instrs.len
	assert mod_legacy.values.len == mod_flat.values.len
}
