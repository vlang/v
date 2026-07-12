// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
// vtest build: macos
//
// Bit-equality pin for the fourth flat-aware post_pass port (s155):
// `inject_main_runtime_const_init_to_flat` must produce a FlatAst whose
// `signature()` matches what the legacy `inject_main_runtime_const_init_calls`
// (mut []ast.File) + `flatten_files` pair produces. Uses the s154 primitives
// `prepend_to_fn_body` (rebuilds main's FnDecl with init calls prepended)
// and `replace_file_stmt` (rewires the file's stmts list to the new fn id).
module transformer

import v2.ast
import v2.pref as vpref
import v2.types

// Local helper — `v test` compiles each `_test.v` file in isolation, so the
// shared `create_test_transformer` is not visible here. Seeded with the two
// runtime-const fields the legacy + flat-aware paths both read off
// `Transformer` state.
fn create_main_init_to_flat_test_transformer() &Transformer {
	env := &types.Environment{}
	return &Transformer{
		pref:                        &vpref.Preferences{}
		env:                         unsafe { env }
		needed_clone_fns:            map[string]string{}
		needed_array_contains_fns:   map[string]ArrayMethodInfo{}
		needed_array_index_fns:      map[string]ArrayMethodInfo{}
		needed_array_last_index_fns: map[string]ArrayMethodInfo{}
		local_decl_types:            map[string]types.Type{}
		runtime_const_modules:       ['mymod']
		runtime_const_init_fn_name:  {
			'mymod': '__v_init_consts_mymod'
		}
	}
}

fn make_main_file_with_main_fn(body []ast.Stmt) ast.File {
	return ast.File{
		name:  'main_min.v'
		mod:   'main'
		stmts: [
			ast.Stmt(ast.ModuleStmt{
				name: 'main'
			}),
			ast.Stmt(ast.FnDecl{
				name:  'main'
				stmts: body
			}),
		]
	}
}

fn make_assert_one_stmt() ast.Stmt {
	return ast.Stmt(ast.AssertStmt{
		expr: ast.Expr(ast.BasicLiteral{
			kind:  .number
			value: '1'
		})
	})
}

// Reference: run legacy mutator on []ast.File then flatten.
//
// Why single-file: the file root's `extra` slot stores `intern(mod)` as a
// raw intern index; multi-file fixtures leak intern order between the
// reference (which interns init-call strings BEFORE the next file's mod) and
// subject (interns next file's mod BEFORE init-call strings) paths. Same
// quirk as s151/s152/s153 single-file fixtures.
fn build_reference_signature_with_main_init() string {
	mut files := [make_main_file_with_main_fn([make_assert_one_stmt()])]
	mut t := create_main_init_to_flat_test_transformer()
	t.inject_main_runtime_const_init_calls(mut files)
	return ast.flatten_files(files).signature()
}

// Subject: append bare main file to FlatBuilder, then run flat-aware splice.
fn build_subject_signature_with_main_init() string {
	mut b := ast.new_flat_builder()
	b.append_file(make_main_file_with_main_fn([make_assert_one_stmt()]))
	mut t := create_main_init_to_flat_test_transformer()
	t.inject_main_runtime_const_init_to_flat(mut b)
	return b.flat.signature()
}

fn test_inject_main_runtime_const_init_to_flat_signature_matches_legacy() {
	ref_sig := build_reference_signature_with_main_init()
	sub_sig := build_subject_signature_with_main_init()
	assert ref_sig == sub_sig
}

fn test_inject_main_runtime_const_init_to_flat_no_init_calls_is_noop() {
	// Transformer with empty runtime_const_modules → init_calls is empty →
	// no splice; flat signature must be identical to the baseline.
	mut b := ast.new_flat_builder()
	b.append_file(make_main_file_with_main_fn([make_assert_one_stmt()]))
	baseline_sig := b.flat.signature()
	env := &types.Environment{}
	mut t := &Transformer{
		pref:                        &vpref.Preferences{}
		env:                         unsafe { env }
		needed_clone_fns:            map[string]string{}
		needed_array_contains_fns:   map[string]ArrayMethodInfo{}
		needed_array_index_fns:      map[string]ArrayMethodInfo{}
		needed_array_last_index_fns: map[string]ArrayMethodInfo{}
		local_decl_types:            map[string]types.Type{}
		runtime_const_modules:       []string{}
		runtime_const_init_fn_name:  map[string]string{}
	}
	t.inject_main_runtime_const_init_to_flat(mut b)
	assert b.flat.signature() == baseline_sig
}

fn test_inject_main_runtime_const_init_to_flat_no_main_fn_is_noop() {
	// File has no `fn main()` — locator returns none; signature unchanged
	// even though init_calls is non-empty.
	module_only_file := ast.File{
		name:  'lib.v'
		mod:   'main'
		stmts: [
			ast.Stmt(ast.ModuleStmt{
				name: 'main'
			}),
		]
	}
	mut b := ast.new_flat_builder()
	b.append_file(module_only_file)
	baseline_sig := b.flat.signature()
	mut t := create_main_init_to_flat_test_transformer()
	t.inject_main_runtime_const_init_to_flat(mut b)
	assert b.flat.signature() == baseline_sig
}

fn test_inject_main_runtime_const_init_to_flat_picks_first_main() {
	// Two files, both contain `fn main()`. Legacy picks the FIRST match
	// (loop breaks on first changed file). The flat-aware port must
	// replicate this — only the first file's main gets the init prepended.
	//
	// Note: this is multi-file, which leaks intern-order through the .file
	// node's `extra=intern(mod)` slot. We compare ONLY the FnDecl subtree of
	// the first file's main, which has identical content in both paths.
	mut ref_files := [
		make_main_file_with_main_fn([make_assert_one_stmt()]),
		make_main_file_with_main_fn([make_assert_one_stmt()]),
	]
	mut t_ref := create_main_init_to_flat_test_transformer()
	t_ref.inject_main_runtime_const_init_calls(mut ref_files)
	ref_flat := ast.flatten_files(ref_files)
	ref_main_fn_id := ref_flat.child_at(ref_flat.child_at(ref_flat.files[0].file_id, 2), 1)
	ref_sub_sig := ref_flat.subtree_signature(ref_main_fn_id)

	mut b := ast.new_flat_builder()
	b.append_file(make_main_file_with_main_fn([make_assert_one_stmt()]))
	b.append_file(make_main_file_with_main_fn([make_assert_one_stmt()]))
	mut t_sub := create_main_init_to_flat_test_transformer()
	t_sub.inject_main_runtime_const_init_to_flat(mut b)
	sub_main_fn_id := b.flat.child_at(b.flat.child_at(b.flat.files[0].file_id, 2), 1)
	sub_sub_sig := b.flat.subtree_signature(sub_main_fn_id)
	assert ref_sub_sig == sub_sub_sig
}
