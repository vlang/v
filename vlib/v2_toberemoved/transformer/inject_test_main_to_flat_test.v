// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
// vtest build: macos
//
// Bit-equality pin for the second flat-aware post_pass port (s152):
// `inject_test_main_to_flat` must produce a FlatAst whose `signature()`
// matches what the legacy `inject_test_main` (mut []ast.File) +
// `flatten_files` pair produces. The s150 primitive `append_file_stmts`
// is the splice mechanism; the synthesised main payload is built by the
// shared `synthesise_test_main_fn` helper so both paths produce
// identical FnDecl content by construction.
module transformer

import v2.ast
import v2.pref as vpref
import v2.types

// Local helper — `v test` compiles each `_test.v` file in isolation, so
// `create_test_transformer` from `transformer_test.v` is not visible here.
fn create_test_main_to_flat_test_transformer() &Transformer {
	env := &types.Environment{}
	return &Transformer{
		pref:                        &vpref.Preferences{}
		env:                         unsafe { env }
		needed_clone_fns:            map[string]string{}
		needed_array_contains_fns:   map[string]ArrayMethodInfo{}
		needed_array_index_fns:      map[string]ArrayMethodInfo{}
		needed_array_last_index_fns: map[string]ArrayMethodInfo{}
		local_decl_types:            map[string]types.Type{}
	}
}

fn make_test_file_with_stmts(extra []ast.Stmt) ast.File {
	mut stmts := []ast.Stmt{cap: 1 + extra.len}
	stmts << ast.Stmt(ast.ModuleStmt{
		name: 'main'
	})
	for s in extra {
		stmts << s
	}
	return ast.File{
		name:  'inline_test.v'
		mod:   'main'
		stmts: stmts
	}
}

fn make_test_fn_decl(name string) ast.Stmt {
	return ast.Stmt(ast.FnDecl{
		name: name
	})
}

// Reference: run legacy mutator on []ast.File then flatten.
//
// Why single-file (no sibling): the file-root node's `extra` slot stores
// `intern(mod)` as a raw intern index, and `signature()` prints it
// unresolved. Legacy `flatten_files` and the subject splice path intern
// strings in different orders when multiple files are present, so a
// single-file fixture is the apples-to-apples comparison. (Same reason
// as s151's `inject_embed_file_helper_to_flat_test.v`.)
fn build_reference_signature_with_test_main() string {
	mut files := [
		make_test_file_with_stmts([make_test_fn_decl('test_first'),
			make_test_fn_decl('test_second')]),
	]
	mut t := create_test_main_to_flat_test_transformer()
	t.inject_test_main(mut files)
	return ast.flatten_files(files).signature()
}

// Subject: append bare test file to FlatBuilder, then run flat-aware splice.
fn build_subject_signature_with_test_main() string {
	mut b := ast.new_flat_builder()
	b.append_file(make_test_file_with_stmts([make_test_fn_decl('test_first'),
		make_test_fn_decl('test_second')]))
	mut t := create_test_main_to_flat_test_transformer()
	t.inject_test_main_to_flat(mut b)
	return b.flat.signature()
}

fn test_inject_test_main_to_flat_signature_matches_legacy() {
	ref_sig := build_reference_signature_with_test_main()
	sub_sig := build_subject_signature_with_test_main()
	assert ref_sig == sub_sig
}

fn test_inject_test_main_to_flat_no_test_fns_is_noop() {
	mut b := ast.new_flat_builder()
	b.append_file(make_test_file_with_stmts([]))
	baseline_sig := b.flat.signature()
	mut t := create_test_main_to_flat_test_transformer()
	t.inject_test_main_to_flat(mut b)
	assert b.flat.signature() == baseline_sig
}

fn test_inject_test_main_to_flat_existing_main_is_noop() {
	// Pre-seed the file with a top-level non-method FnDecl named `main` —
	// the user-main detection must skip the splice even though there's a
	// `test_*` fn present.
	preseeded_main := ast.Stmt(ast.FnDecl{
		name: 'main'
	})
	mut b := ast.new_flat_builder()
	b.append_file(make_test_file_with_stmts([preseeded_main, make_test_fn_decl('test_first')]))
	baseline_sig := b.flat.signature()
	mut t := create_test_main_to_flat_test_transformer()
	t.inject_test_main_to_flat(mut b)
	assert b.flat.signature() == baseline_sig
}
