// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
// vtest build: macos
//
// Bit-equality pin for s172: `register_fn_signatures_from_flat` (flat-cursor
// port) must register the same fn signatures (fn_index, mod.funcs) as the
// legacy `register_fn_signatures`. The flat port walks one file's top-level
// stmts via FileCursor and only reads `.stmt_fn_decl` signatures via
// `Cursor.fn_decl_signature()` which returns FnDecl with `stmts = []`. Fn
// bodies are never decoded — the largest per-stmt savings of any SSA
// phase port.
module ssa

import v2.ast
import v2.types

fn make_fn_sigs_fixture() []ast.File {
	return [
		ast.File{
			name:  'main.v'
			mod:   'main'
			stmts: [
				ast.Stmt(ast.ModuleStmt{
					name: 'main'
				}),
				ast.Stmt(ast.FnDecl{
					name: 'add'
					typ:  ast.FnType{
						return_type: ast.Expr(ast.Ident{
							name: 'int'
						})
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
					}
				}),
				ast.Stmt(ast.FnDecl{
					name: 'noop'
					typ:  ast.FnType{}
				}),
			]
		},
	]
}

// register_fn_signatures_from_flat on a file with no fns must not change
// fn_index or mod.funcs — same as register_fn_signatures.
fn test_register_fn_signatures_from_flat_no_fns_matches_legacy() {
	files := [
		ast.File{
			name:  'empty.v'
			mod:   'main'
			stmts: [ast.Stmt(ast.ModuleStmt{
				name: 'main'
			})]
		},
	]
	flat := ast.flatten_files(files)

	env := types.Environment.new()

	mut mod_legacy := Module.new('fns_empty_legacy')
	mut b_legacy := Builder.new_with_env(mod_legacy, env)
	b_legacy.register_fn_signatures(files[0])

	mut mod_flat := Module.new('fns_empty_flat')
	mut b_flat := Builder.new_with_env(mod_flat, env)
	b_flat.register_fn_signatures_from_flat(flat.file_cursor(0))

	assert b_legacy.fn_index.len == b_flat.fn_index.len
	assert mod_legacy.funcs.len == mod_flat.funcs.len
}

// register_fn_signatures_from_flat on a file with two FnDecls (add + noop)
// must register both with identical fn_index entries and matching parameter
// counts on the SSA Function.
fn test_register_fn_signatures_from_flat_matches_legacy_for_two_fns() {
	files := make_fn_sigs_fixture()
	flat := ast.flatten_files(files)

	env := types.Environment.new()

	mut mod_legacy := Module.new('fns_two_legacy')
	mut b_legacy := Builder.new_with_env(mod_legacy, env)
	b_legacy.register_fn_signatures(files[0])

	mut mod_flat := Module.new('fns_two_flat')
	mut b_flat := Builder.new_with_env(mod_flat, env)
	b_flat.register_fn_signatures_from_flat(flat.file_cursor(0))

	// Both paths registered exactly 2 fns.
	assert b_legacy.fn_index.len == b_flat.fn_index.len
	assert b_legacy.fn_index.len == 2
	assert 'add' in b_legacy.fn_index
	assert 'add' in b_flat.fn_index
	assert 'noop' in b_legacy.fn_index
	assert 'noop' in b_flat.fn_index
	// Function ids match because identical registration order on isolated Modules.
	assert b_legacy.fn_index['add'] == b_flat.fn_index['add']
	assert b_legacy.fn_index['noop'] == b_flat.fn_index['noop']
	// mod.funcs slot counts match.
	assert mod_legacy.funcs.len == mod_flat.funcs.len
	// add has 2 params on both paths.
	add_legacy := mod_legacy.funcs[b_legacy.fn_index['add']]
	add_flat := mod_flat.funcs[b_flat.fn_index['add']]
	assert add_legacy.params.len == add_flat.params.len
	assert add_legacy.params.len == 2
	// noop has 0 params on both paths.
	noop_legacy := mod_legacy.funcs[b_legacy.fn_index['noop']]
	noop_flat := mod_flat.funcs[b_flat.fn_index['noop']]
	assert noop_legacy.params.len == noop_flat.params.len
	assert noop_legacy.params.len == 0
}
