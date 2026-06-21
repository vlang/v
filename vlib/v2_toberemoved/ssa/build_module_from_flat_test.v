// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
// vtest build: macos
//
// Bit-equality pin for s180: `build_module_from_flat` (fifth per-kind port
// inside the s175 seam) must set `b.cur_module` to the same dotted-to-
// underscore form as the legacy `ast.ModuleStmt` arm in `build_stmt`. The
// flat path drops the ModuleStmt struct decode entirely and reads the
// module name straight from `c.name()`.
module ssa

import v2.ast
import v2.types

// Fixture: a file with `module foo.bar` (dotted name to exercise the
// `.replace('.', '_')`). No fns — we test the seam by directly driving
// `build_stmt`/`build_stmt_from_flat` on the file's top-level stmts list.
fn make_module_fixture() []ast.File {
	return [
		ast.File{
			name:  'main.v'
			mod:   'foo.bar'
			stmts: [
				ast.Stmt(ast.ModuleStmt{
					name: 'foo.bar'
				}),
			]
		},
	]
}

fn test_build_module_from_flat_matches_legacy() {
	files := make_module_fixture()
	flat := ast.flatten_files(files)
	env := types.Environment.new()

	mut mod_legacy := Module.new('mod_legacy')
	mut b_legacy := Builder.new_with_env(mod_legacy, env)
	b_legacy.build_stmts(files[0].stmts)

	mut mod_flat := Module.new('mod_flat')
	mut b_flat := Builder.new_with_env(mod_flat, env)
	b_flat.build_stmts_from_flat(flat.file_cursor(0).stmts())

	// Both paths must transform 'foo.bar' → 'foo_bar' on b.cur_module.
	assert b_legacy.cur_module == 'foo_bar'
	assert b_flat.cur_module == b_legacy.cur_module
	// And neither path emits any SSA for a ModuleStmt.
	assert mod_legacy.instrs.len == mod_flat.instrs.len
	assert mod_legacy.blocks.len == mod_flat.blocks.len
	assert mod_legacy.values.len == mod_flat.values.len
}

fn test_module_import_aliases_keep_nested_module_path() {
	aliases := module_import_aliases_from_imports([
		ast.ImportStmt{
			name:  'foo.bar'
			alias: 'bar'
		},
		ast.ImportStmt{
			name:       'foo.baz'
			alias:      'qux'
			is_aliased: true
		},
	])
	assert aliases['bar'] == 'foo.bar'
	assert aliases['qux'] == 'foo.baz'
}

fn test_selective_import_fn_names_keep_nested_module_path() {
	names := selective_import_fn_names_from_imports([
		ast.ImportStmt{
			name:    'foo.bar'
			alias:   'bar'
			symbols: [
				ast.Expr(ast.Ident{
					name: 'leaf'
				}),
			]
		},
	])
	assert names['leaf'] == 'foo_bar__leaf'
}

fn test_selective_import_fn_candidates_try_nested_path_then_leaf_module() {
	candidates := selective_import_fn_candidates_from_imports([
		ast.ImportStmt{
			name:    'foo.bar'
			alias:   'bar'
			symbols: [
				ast.Expr(ast.Ident{
					name: 'leaf'
				}),
			]
		},
	])
	assert candidates['leaf'] == ['foo_bar__leaf', 'bar__leaf']

	mut mod_full := Module.new('selective_full')
	mut b_full := Builder.new_with_env(mod_full, types.Environment.new())
	b_full.selective_import_fn_candidates = candidates.clone()
	b_full.fn_index['foo_bar__leaf'] = 0
	b_full.fn_index['bar__leaf'] = 1
	resolved_full := b_full.selective_import_fn_name('leaf') or { '' }
	assert resolved_full == 'foo_bar__leaf'

	mut mod_leaf := Module.new('selective_leaf')
	mut b_leaf := Builder.new_with_env(mod_leaf, types.Environment.new())
	b_leaf.selective_import_fn_candidates = candidates.clone()
	b_leaf.fn_index['bar__leaf'] = 1
	resolved_leaf := b_leaf.selective_import_fn_name('leaf') or { '' }
	assert resolved_leaf == 'bar__leaf'
}
