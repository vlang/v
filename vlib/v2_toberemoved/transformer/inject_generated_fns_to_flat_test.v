// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
// vtest build: macos
//
// Bit-equality pin for the third flat-aware post_pass port (s153):
// `inject_generated_fns_to_flat` must produce a FlatAst whose
// `signature()` matches what `inject_generated_fns_to_files` (the
// extracted legacy splice) + `flatten_files` produces. Each bucket
// (core_fns / module_fns[mod] / user_fns) is exercised in isolation
// against a single-file fixture — the multi-file intern-order quirk
// (same as s151/s152) makes multi-file signature comparison brittle, so
// we test the routing fallbacks of each bucket independently.
module transformer

import v2.ast
import v2.pref as vpref
import v2.types

// Local helper — `v test` compiles each `_test.v` file in isolation, so
// `create_test_transformer` from `transformer_test.v` is not visible here.
fn create_generated_fns_to_flat_test_transformer() &Transformer {
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

fn make_file_for_mod(mod string) ast.File {
	return ast.File{
		name:  '${mod}_min.v'
		mod:   mod
		stmts: [
			ast.Stmt(ast.ModuleStmt{
				name: mod
			}),
		]
	}
}

fn make_fn_decl(name string) ast.Stmt {
	return ast.Stmt(ast.FnDecl{
		name: name
	})
}

// Bucket 1: core_fns → builtin file.
fn test_inject_generated_fns_to_flat_core_fns_into_builtin() {
	parts := GeneratedFnsParts{
		core_fns:   [make_fn_decl('int_str'), make_fn_decl('f64_str')]
		module_fns: map[string][]ast.Stmt{}
		user_fns:   []ast.Stmt{}
	}
	mut ref_files := [make_file_for_mod('builtin')]
	inject_generated_fns_to_files(mut ref_files, parts)
	ref_sig := ast.flatten_files(ref_files).signature()

	mut b := ast.new_flat_builder()
	b.append_file(make_file_for_mod('builtin'))
	mut t := create_generated_fns_to_flat_test_transformer()
	t.inject_generated_fns_to_flat(mut b, parts)
	assert b.flat.signature() == ref_sig
}

// Bucket 2: module_fns[mod] → mod file.
fn test_inject_generated_fns_to_flat_module_fns_into_mod() {
	mut module_fns := map[string][]ast.Stmt{}
	module_fns['time'] = [make_fn_decl('time__FormatDate__str')]
	parts := GeneratedFnsParts{
		core_fns:   []ast.Stmt{}
		module_fns: module_fns
		user_fns:   []ast.Stmt{}
	}
	mut ref_files := [make_file_for_mod('time')]
	inject_generated_fns_to_files(mut ref_files, parts)
	ref_sig := ast.flatten_files(ref_files).signature()

	mut b := ast.new_flat_builder()
	b.append_file(make_file_for_mod('time'))
	mut t := create_generated_fns_to_flat_test_transformer()
	t.inject_generated_fns_to_flat(mut b, parts)
	assert b.flat.signature() == ref_sig
}

// Bucket 3: user_fns → main file.
fn test_inject_generated_fns_to_flat_user_fns_into_main() {
	parts := GeneratedFnsParts{
		core_fns:   []ast.Stmt{}
		module_fns: map[string][]ast.Stmt{}
		user_fns:   [make_fn_decl('Array_Test2_str'), make_fn_decl('Map_int_string_str')]
	}
	mut ref_files := [make_file_for_mod('main')]
	inject_generated_fns_to_files(mut ref_files, parts)
	ref_sig := ast.flatten_files(ref_files).signature()

	mut b := ast.new_flat_builder()
	b.append_file(make_file_for_mod('main'))
	mut t := create_generated_fns_to_flat_test_transformer()
	t.inject_generated_fns_to_flat(mut b, parts)
	assert b.flat.signature() == ref_sig
}

// Fallback: core_fns lands in user_fns when no builtin file exists.
// With a single main file, user_fns → main → both legacy and flat-aware
// should splice core_fns into main via the user_fns route.
fn test_inject_generated_fns_to_flat_core_fns_fallback_no_builtin() {
	parts := GeneratedFnsParts{
		core_fns:   [make_fn_decl('int_str')]
		module_fns: map[string][]ast.Stmt{}
		user_fns:   []ast.Stmt{}
	}
	mut ref_files := [make_file_for_mod('main')]
	inject_generated_fns_to_files(mut ref_files, parts)
	ref_sig := ast.flatten_files(ref_files).signature()

	mut b := ast.new_flat_builder()
	b.append_file(make_file_for_mod('main'))
	mut t := create_generated_fns_to_flat_test_transformer()
	t.inject_generated_fns_to_flat(mut b, parts)
	assert b.flat.signature() == ref_sig
}
