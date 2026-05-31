// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
// vtest build: macos
//
// Bit-equality pin for s161: the `post_pass_to_flat` driver must run
// the six flat-aware post_pass steps in the same order as legacy
// `post_pass`, with the same gating, producing a FlatAst whose
// `signature()` matches what legacy `post_pass` + `flatten_files`
// produces. Pins the driver assembly that wires together
// s151/s152/s153/s155/s159/s160's `_to_flat` variants.
module transformer

import v2.ast
import v2.pref as vpref
import v2.types

// Local helper — `v test` compiles each `_test.v` file in isolation.
fn create_post_pass_to_flat_test_transformer() &Transformer {
	env := &types.Environment{}
	return &Transformer{
		pref:                        &vpref.Preferences{}
		env:                         unsafe { env }
		needed_clone_fns:            map[string]string{}
		needed_array_contains_fns:   map[string]ArrayMethodInfo{}
		needed_array_index_fns:      map[string]ArrayMethodInfo{}
		needed_array_last_index_fns: map[string]ArrayMethodInfo{}
		local_decl_types:            map[string]types.Type{}
		runtime_const_inits_by_mod:  map[string][]RuntimeConstInit{}
		runtime_const_init_fn_name:  map[string]string{}
	}
}

fn make_minimal_main_file_with_main_fn() ast.File {
	return ast.File{
		name:  'main.v'
		mod:   'main'
		stmts: [
			ast.Stmt(ast.ModuleStmt{
				name: 'main'
			}),
			ast.Stmt(ast.FnDecl{
				name:  'main'
				stmts: []ast.Stmt{}
			}),
		]
	}
}

fn make_mymod_file_for_driver() ast.File {
	return ast.File{
		name:  'mymod.v'
		mod:   'mymod'
		stmts: [
			ast.Stmt(ast.ModuleStmt{
				name: 'mymod'
			}),
		]
	}
}

// All gates off → driver is a near-no-op (only `inject_runtime_const_init_fns_to_flat`
// and `inject_main_runtime_const_init_to_flat` and `inject_test_main_to_flat`
// have unconditional entry, but they short-circuit because state is empty).
// Bit-equality with legacy `post_pass + flatten_files` proves the driver
// preserves the post_pass control-flow tree even when all steps no-op.
fn test_post_pass_to_flat_all_gates_off_matches_legacy() {
	mut ref_files := [make_minimal_main_file_with_main_fn()]
	mut t_ref := create_post_pass_to_flat_test_transformer()
	t_ref.post_pass(mut ref_files)
	ref_sig := ast.flatten_files(ref_files).signature()

	mut b := ast.new_flat_builder()
	b.append_file(make_minimal_main_file_with_main_fn())
	mut t_sub := create_post_pass_to_flat_test_transformer()
	t_sub.post_pass_to_flat(mut b, none)
	assert b.flat.signature() == ref_sig
}

// Active runtime-const-init gate exercises s160 (generate the
// `__v_init_consts_mymod` fn into the mymod file) AND s155 (prepend init
// call to main's body). Two-mod fixture leaks intern order through
// `.file.extra=intern(mod)`, so compare per-file via subtree_signature.
fn test_post_pass_to_flat_runtime_const_init_matches_legacy() {
	mut t_ref := create_post_pass_to_flat_test_transformer()
	t_ref.runtime_const_modules = ['mymod']
	t_ref.runtime_const_inits_by_mod['mymod'] = [
		RuntimeConstInit{
			name: 'mymod__answer'
			expr: ast.Expr(ast.BasicLiteral{
				kind:  .number
				value: '42'
			})
		},
	]
	mut ref_files := [make_minimal_main_file_with_main_fn(), make_mymod_file_for_driver()]
	t_ref.post_pass(mut ref_files)
	ref_flat := ast.flatten_files(ref_files)
	// File 0 (main): edge 2 = stmts list — main's body should be prepended
	// with init call.
	ref_main_stmts := ref_flat.child_at(ref_flat.files[0].file_id, 2)
	ref_main_sub_sig := ref_flat.subtree_signature(ref_main_stmts)
	// File 1 (mymod): edge 2 = stmts list — should contain the generated
	// `__v_init_consts_mymod` fn.
	ref_mymod_stmts := ref_flat.child_at(ref_flat.files[1].file_id, 2)
	ref_mymod_sub_sig := ref_flat.subtree_signature(ref_mymod_stmts)

	mut t_sub := create_post_pass_to_flat_test_transformer()
	t_sub.runtime_const_modules = ['mymod']
	t_sub.runtime_const_inits_by_mod['mymod'] = [
		RuntimeConstInit{
			name: 'mymod__answer'
			expr: ast.Expr(ast.BasicLiteral{
				kind:  .number
				value: '42'
			})
		},
	]
	mut b := ast.new_flat_builder()
	b.append_file(make_minimal_main_file_with_main_fn())
	b.append_file(make_mymod_file_for_driver())
	t_sub.post_pass_to_flat(mut b, none)
	sub_main_stmts := b.flat.child_at(b.flat.files[0].file_id, 2)
	sub_main_sub_sig := b.flat.subtree_signature(sub_main_stmts)
	sub_mymod_stmts := b.flat.child_at(b.flat.files[1].file_id, 2)
	sub_mymod_sub_sig := b.flat.subtree_signature(sub_mymod_stmts)

	assert ref_main_sub_sig == sub_main_sub_sig
	assert ref_mymod_sub_sig == sub_mymod_sub_sig
}

// Backend gating: cleanc must skip inject_test_main, eval must skip
// inject_runtime_const_init_fns + inject_main_runtime_const_init_calls.
// We don't fully exercise eval backend here (no test_ fns, no native
// backend), but cleanc gating is observable: with a `test_*` fn, default
// backend produces `fn main()` (synthesised), cleanc skips that. The
// driver must match legacy on both.
fn test_post_pass_to_flat_cleanc_skips_test_main() {
	mut t_ref := create_post_pass_to_flat_test_transformer()
	t_ref.pref = &vpref.Preferences{
		backend: .cleanc
	}
	test_file := ast.File{
		name:  'foo_test.v'
		mod:   'main'
		stmts: [
			ast.Stmt(ast.ModuleStmt{
				name: 'main'
			}),
			ast.Stmt(ast.FnDecl{
				name:  'test_one'
				stmts: []ast.Stmt{}
			}),
		]
	}
	mut ref_files := [test_file]
	t_ref.post_pass(mut ref_files)
	ref_sig := ast.flatten_files(ref_files).signature()

	mut t_sub := create_post_pass_to_flat_test_transformer()
	t_sub.pref = &vpref.Preferences{
		backend: .cleanc
	}
	mut b := ast.new_flat_builder()
	b.append_file(ast.File{
		name:  'foo_test.v'
		mod:   'main'
		stmts: [
			ast.Stmt(ast.ModuleStmt{
				name: 'main'
			}),
			ast.Stmt(ast.FnDecl{
				name:  'test_one'
				stmts: []ast.Stmt{}
			}),
		]
	})
	t_sub.post_pass_to_flat(mut b, none)
	assert b.flat.signature() == ref_sig
}
