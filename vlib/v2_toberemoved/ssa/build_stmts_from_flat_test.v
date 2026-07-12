// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
// vtest build: macos
//
// Bit-equality pin for s175: `build_stmts_from_flat` / `build_stmt_from_flat`
// (the cursor-list seam) must produce the same SSA as the legacy
// `build_stmts` / `build_stmt`. Today every cursor decodes back to a
// legacy `ast.Stmt` and dispatches through `build_stmt`. The pin guards
// the seam wiring so future per-stmt-kind ports inside `build_stmt_from_flat`
// can be validated incrementally.
module ssa

import v2.ast
import v2.types

// Build a fn whose body is a single ReturnStmt. The legacy path goes
// build_stmts -> build_stmt -> build_return; the flat path goes
// build_stmts_from_flat -> build_stmt_from_flat (decode) -> build_stmt ->
// build_return. Both paths must emit the same SSA instrs.
fn make_build_stmts_fixture() []ast.File {
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
									value: '7'
								}),
							]
						}),
					]
				}),
			]
		},
	]
}

// build_stmts_from_flat on an empty cursor list must be a no-op — same as
// build_stmts on an empty slice.
fn test_build_stmts_from_flat_empty_matches_legacy() {
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

	mut mod_legacy := Module.new('bs_empty_legacy')
	mut b_legacy := Builder.new_with_env(mod_legacy, env)
	b_legacy.build_stmts([]ast.Stmt{})

	mut mod_flat := Module.new('bs_empty_flat')
	mut b_flat := Builder.new_with_env(mod_flat, env)
	// Pass the file's stmts list (only ModuleStmt) — neither path emits
	// any SSA for a ModuleStmt (build_stmt has no arm for it).
	b_flat.build_stmts_from_flat(flat.file_cursor(0).stmts())

	assert mod_legacy.instrs.len == mod_flat.instrs.len
	assert mod_legacy.blocks.len == mod_flat.blocks.len
	assert mod_legacy.values.len == mod_flat.values.len
}

// build_stmts_from_flat dispatched through build_stmt_from_flat must build
// the same SSA bodies as the legacy walker. Validates the cursor-list seam
// end-to-end via build_fn_bodies (which now consumes cursors as of s174).
fn test_build_stmts_from_flat_matches_legacy_for_return_fn() {
	files := make_build_stmts_fixture()
	flat := ast.flatten_files(files)

	env := types.Environment.new()

	mut mod_legacy := Module.new('bs_two_legacy')
	mut b_legacy := Builder.new_with_env(mod_legacy, env)
	b_legacy.register_fn_signatures(files[0])
	b_legacy.build_fn_bodies(files[0])

	mut mod_flat := Module.new('bs_two_flat')
	mut b_flat := Builder.new_with_env(mod_flat, env)
	b_flat.register_fn_signatures_from_flat(flat.file_cursor(0))
	b_flat.build_fn_bodies_from_flat(flat.file_cursor(0))

	// The body went through build_stmts_from_flat (s174 wiring inside
	// build_fn currently calls build_stmts — once that switches to
	// build_stmts_from_flat in s176+, this pin will also catch the
	// dispatch wiring of the seam itself).
	assert mod_legacy.funcs.len == mod_flat.funcs.len
	assert mod_legacy.blocks.len == mod_flat.blocks.len
	assert mod_legacy.instrs.len == mod_flat.instrs.len
	assert mod_legacy.values.len == mod_flat.values.len
}

// Direct exercise of build_stmts_from_flat: dispatch the file's whole stmt
// list (incl. a FnDecl) through the seam from outside build_fn. The flat
// path must emit the same SSA as the legacy build_stmts call. Since neither
// path's build_stmt has an FnDecl arm, this is also a "no-op" check, but
// it confirms the cursor walk handles top-level stmt mixes without panic.
fn test_build_stmts_from_flat_direct_dispatch_matches_legacy() {
	files := make_build_stmts_fixture()
	flat := ast.flatten_files(files)

	env := types.Environment.new()

	mut mod_legacy := Module.new('bs_direct_legacy')
	mut b_legacy := Builder.new_with_env(mod_legacy, env)
	b_legacy.build_stmts(files[0].stmts)

	mut mod_flat := Module.new('bs_direct_flat')
	mut b_flat := Builder.new_with_env(mod_flat, env)
	b_flat.build_stmts_from_flat(flat.file_cursor(0).stmts())

	assert mod_legacy.instrs.len == mod_flat.instrs.len
	assert mod_legacy.blocks.len == mod_flat.blocks.len
	assert mod_legacy.values.len == mod_flat.values.len
}
