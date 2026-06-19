// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
// vtest build: macos
//
// Bit-equality pin for the first flat-aware post_pass port (s151):
// `inject_embed_file_helper_to_flat` must produce a FlatAst whose
// `signature()` matches what the legacy `inject_embed_file_helper`
// (mut []ast.File) + `flatten_files` pair produces. The s150 primitive
// `append_file_stmts` is the splice mechanism under the hood.
module transformer

import v2.ast
import v2.pref as vpref
import v2.types

// Local helper — `v test` compiles each `_test.v` file in isolation, so
// `create_test_transformer` from `transformer_test.v` is not visible here.
fn create_embed_to_flat_test_transformer() &Transformer {
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

fn make_minimal_builtin_file_with_stmts(extra []ast.Stmt) ast.File {
	mut stmts := []ast.Stmt{cap: 1 + extra.len}
	stmts << ast.Stmt(ast.ModuleStmt{
		name: 'builtin'
	})
	for s in extra {
		stmts << s
	}
	return ast.File{
		name:  'builtin_min.v'
		mod:   'builtin'
		stmts: stmts
	}
}

fn make_minimal_main_file() ast.File {
	return ast.File{
		name:  'main_min.v'
		mod:   'main'
		stmts: [
			ast.Stmt(ast.ModuleStmt{
				name: 'main'
			}),
		]
	}
}

// Reference: run legacy mutator on []ast.File then flatten.
//
// Why single-file (no non-builtin sibling): the file-root node's `extra`
// slot stores `intern(mod)` (raw intern index), and the legacy path
// interns all helper-stmt strings BEFORE it interns the next file's
// mod string, while the subject path interns the next file's mod BEFORE
// the helper-stmt strings. With only the builtin file present, both
// paths intern 'builtin' as idx 0 first, so the file root's `extra=0`
// matches in both signatures.
fn build_reference_signature_with_helper() string {
	mut files := [make_minimal_builtin_file_with_stmts([])]
	mut t := create_embed_to_flat_test_transformer()
	t.inject_embed_file_helper(mut files)
	return ast.flatten_files(files).signature()
}

// Subject: append bare builtin file to FlatBuilder, then run flat-aware splice.
fn build_subject_signature_with_helper() string {
	mut b := ast.new_flat_builder()
	b.append_file(make_minimal_builtin_file_with_stmts([]))
	mut t := create_embed_to_flat_test_transformer()
	t.inject_embed_file_helper_to_flat(mut b)
	return b.flat.signature()
}

fn test_inject_embed_file_helper_to_flat_signature_matches_legacy() {
	ref_sig := build_reference_signature_with_helper()
	sub_sig := build_subject_signature_with_helper()
	assert ref_sig == sub_sig
}

fn test_inject_embed_file_helper_to_flat_no_builtin_is_noop() {
	mut b := ast.new_flat_builder()
	b.append_file(make_minimal_main_file())
	baseline_sig := b.flat.signature()
	mut t := create_embed_to_flat_test_transformer()
	t.inject_embed_file_helper_to_flat(mut b)
	assert b.flat.signature() == baseline_sig
}

fn test_inject_embed_file_helper_to_flat_already_present_is_noop() {
	// Pre-seed the builtin file with a StructDecl whose name matches the
	// helper type — the re-entry guard must skip the splice.
	preseed := ast.Stmt(ast.StructDecl{
		name: embed_file_helper_type_name
	})
	mut b := ast.new_flat_builder()
	b.append_file(make_minimal_builtin_file_with_stmts([preseed]))
	baseline_sig := b.flat.signature()
	mut t := create_embed_to_flat_test_transformer()
	t.inject_embed_file_helper_to_flat(mut b)
	assert b.flat.signature() == baseline_sig
}
