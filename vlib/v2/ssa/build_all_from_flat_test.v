// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
// vtest build: macos
//
// Bit-equality pin for s168: `build_all_from_flat` (a thin rehydrate-then-call
// wrapper) must produce the same `Module` state as `build_all`. The wrapper
// rehydrates `flat` into `[]ast.File` via `flat.to_files_range(0,
// flat.files.len)` and delegates to the legacy walker. Behaviour is identical
// by construction provided flatten+rehydrate preserves the bits build_all
// reads (file.mod, file.stmts shape).
module ssa

import v2.ast
import v2.types

fn make_build_all_test_files() []ast.File {
	return [
		ast.File{
			name:  'main.v'
			mod:   'main'
			stmts: [
				ast.Stmt(ast.ModuleStmt{
					name: 'main'
				}),
			]
		},
	]
}

// Empty flat — both paths must pre-register the same builtin globals and not
// crash on the empty stmt set.
fn test_build_all_from_flat_empty_flat_matches_legacy() {
	flat := ast.FlatAst{}

	env := types.Environment.new()
	mut mod_legacy := Module.new('build_all_empty_legacy')
	mut b_legacy := Builder.new_with_env(mod_legacy, env)
	b_legacy.build_all([]ast.File{})

	mut mod_flat := Module.new('build_all_empty_flat')
	mut b_flat := Builder.new_with_env(mod_flat, env)
	b_flat.build_all_from_flat(&flat)

	// Both modules should have the same number of pre-registered builtin
	// globals (g_main_argc, g_main_argv, __stdoutp, __stdinp, __stderrp).
	assert mod_legacy.globals.len == mod_flat.globals.len
	for i in 0 .. mod_legacy.globals.len {
		assert mod_legacy.globals[i].name == mod_flat.globals[i].name
	}
}

// Single ModuleStmt file — flatten+rehydrate must preserve the stmt shape so
// that build_all sees the same input both times.
fn test_build_all_from_flat_matches_legacy_for_simple_files() {
	files := make_build_all_test_files()
	flat := ast.flatten_files(files)

	env := types.Environment.new()
	mut mod_legacy := Module.new('build_all_simple_legacy')
	mut b_legacy := Builder.new_with_env(mod_legacy, env)
	b_legacy.build_all(files)

	mut mod_flat := Module.new('build_all_simple_flat')
	mut b_flat := Builder.new_with_env(mod_flat, env)
	b_flat.build_all_from_flat(&flat)

	// Globals, values, funcs all materialised the same way on both paths.
	assert mod_legacy.globals.len == mod_flat.globals.len
	assert mod_legacy.values.len == mod_flat.values.len
	assert mod_legacy.funcs.len == mod_flat.funcs.len
	// Builder state mirrors module state.
	assert b_legacy.cur_module == b_flat.cur_module
}
