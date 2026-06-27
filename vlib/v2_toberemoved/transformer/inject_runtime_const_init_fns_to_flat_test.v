// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
// vtest build: macos
//
// Bit-equality pin for the sixth flat-aware post_pass port (s160):
// `inject_runtime_const_init_fns_to_flat` must produce a FlatAst whose
// `signature()` matches what the legacy `inject_runtime_const_init_fns`
// (mut []ast.File) + `flatten_files` pair produces. Uses s150's
// `append_file_stmts` to splice the synthesised `__v_init_consts_<mod>`
// FnDecl into the matching module file. Closes the post_pass port arc:
// every file-mutating step in legacy `post_pass` now has a flat-aware
// `_to_flat` variant — s151/s152/s153/s155/s159/s160.
module transformer

import v2.ast
import v2.pref as vpref
import v2.types

// Local helper — `v test` compiles each `_test.v` file in isolation.
// Seeds `runtime_const_modules` + `runtime_const_inits_by_mod` so
// `runtime_const_init_fn_stmts_parts` produces a non-empty (mod, fn_stmt)
// map. The init expr is a trivial integer literal — the legacy splice
// transforms it via `transform_expr_in_module` (identity for literals),
// so both paths produce identical FnDecl bodies.
fn create_runtime_init_fns_to_flat_test_transformer() &Transformer {
	env := &types.Environment{}
	mut t := &Transformer{
		pref:                        &vpref.Preferences{}
		env:                         unsafe { env }
		needed_clone_fns:            map[string]string{}
		needed_array_contains_fns:   map[string]ArrayMethodInfo{}
		needed_array_index_fns:      map[string]ArrayMethodInfo{}
		needed_array_last_index_fns: map[string]ArrayMethodInfo{}
		local_decl_types:            map[string]types.Type{}
		runtime_const_modules:       ['mymod']
		runtime_const_inits_by_mod:  map[string][]RuntimeConstInit{}
		runtime_const_init_fn_name:  map[string]string{}
	}
	t.runtime_const_inits_by_mod['mymod'] = [
		RuntimeConstInit{
			name: 'mymod__answer'
			expr: ast.Expr(ast.BasicLiteral{
				kind:  .number
				value: '42'
			})
		},
	]
	return t
}

fn make_mymod_file() ast.File {
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

fn make_other_file() ast.File {
	return ast.File{
		name:  'other.v'
		mod:   'other'
		stmts: [
			ast.Stmt(ast.ModuleStmt{
				name: 'other'
			}),
		]
	}
}

// Reference: run legacy mutator on []ast.File then flatten.
//
// Why single-file: the file root's `extra` slot stores `intern(mod)` as a
// raw intern index; the legacy path interns the synthesised fn's strings
// BEFORE the next file's mod string, but the subject path interns the
// next file's mod string FIRST (during the initial bare-file append) and
// THEN the synthesised fn's strings. With one file present, both paths
// intern 'mymod' as idx 0 first — `extra=0` matches in both signatures.
fn build_reference_signature_with_init_fn() string {
	mut files := [make_mymod_file()]
	mut t := create_runtime_init_fns_to_flat_test_transformer()
	t.inject_runtime_const_init_fns(mut files)
	return ast.flatten_files(files).signature()
}

fn build_subject_signature_with_init_fn() string {
	mut b := ast.new_flat_builder()
	b.append_file(make_mymod_file())
	mut t := create_runtime_init_fns_to_flat_test_transformer()
	t.inject_runtime_const_init_fns_to_flat(mut b)
	return b.flat.signature()
}

fn test_inject_runtime_const_init_fns_to_flat_signature_matches_legacy() {
	ref_sig := build_reference_signature_with_init_fn()
	sub_sig := build_subject_signature_with_init_fn()
	assert ref_sig == sub_sig
}

fn test_inject_runtime_const_init_fns_legacy_path_decodes_cursor_backed_inits() {
	mut source_builder := ast.new_flat_builder()
	source_builder.append_file(ast.File{
		name:  'mymod.v'
		mod:   'mymod'
		stmts: [
			ast.Stmt(ast.ModuleStmt{
				name: 'mymod'
			}),
			ast.Stmt(ast.ConstDecl{
				fields: [
					ast.FieldInit{
						name:  'answer'
						value: ast.Expr(ast.CallExpr{
							lhs: ast.Expr(ast.Ident{
								name: 'make_value'
							})
						})
					},
				]
			}),
		]
	})
	field := source_builder.flat.file_cursor(0).stmts().at(1).list_at(0).at(0)
	mut t := create_runtime_init_fns_to_flat_test_transformer()
	t.runtime_const_inits_by_mod['mymod'] = [
		RuntimeConstInit{
			name:        'mymod__answer'
			expr_cursor: field.edge(0)
		},
	]
	mut files := [make_mymod_file()]
	t.inject_runtime_const_init_fns(mut files)
	assert files[0].stmts.len == 2
	fn_decl := files[0].stmts[1] as ast.FnDecl
	assert fn_decl.name == '__v_init_consts_mymod'
	assert fn_decl.stmts.len == 1
	assign := fn_decl.stmts[0] as ast.AssignStmt
	assert assign.rhs.len == 1
	call := assign.rhs[0] as ast.CallExpr
	assert call.lhs is ast.Ident
	assert (call.lhs as ast.Ident).name == 'make_value'
}

fn test_inject_runtime_const_init_fns_to_flat_no_matching_file_is_noop() {
	// runtime_const_modules names 'mymod' but no file has that mod — the
	// inner loop never finds a match, so the splice is a no-op.
	mut b := ast.new_flat_builder()
	b.append_file(make_other_file())
	baseline_sig := b.flat.signature()
	mut t := create_runtime_init_fns_to_flat_test_transformer()
	t.inject_runtime_const_init_fns_to_flat(mut b)
	assert b.flat.signature() == baseline_sig
}

fn test_inject_runtime_const_init_fns_to_flat_empty_modules_is_noop() {
	// Transformer with empty runtime_const_modules → parts map is empty →
	// no splice.
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
		runtime_const_inits_by_mod:  map[string][]RuntimeConstInit{}
		runtime_const_init_fn_name:  map[string]string{}
	}
	mut b := ast.new_flat_builder()
	b.append_file(make_mymod_file())
	baseline_sig := b.flat.signature()
	t.inject_runtime_const_init_fns_to_flat(mut b)
	assert b.flat.signature() == baseline_sig
}

fn test_inject_runtime_const_init_fns_to_flat_picks_first_matching_file() {
	// Two files with mod='mymod'. Legacy picks the FIRST match (loop breaks
	// on first append). The flat-aware port must replicate this — only the
	// first 'mymod' file gets the synthesised fn appended.
	//
	// Multi-file leaks intern-order through `.file extra=intern(mod)`, so
	// we compare ONLY the FnDecl subtree (last stmt of file 0's stmts list),
	// which has identical content in both paths.
	mut ref_files := [make_mymod_file(), make_mymod_file()]
	mut t_ref := create_runtime_init_fns_to_flat_test_transformer()
	t_ref.inject_runtime_const_init_fns(mut ref_files)
	ref_flat := ast.flatten_files(ref_files)
	ref_stmts_list := ref_flat.child_at(ref_flat.files[0].file_id, 2)
	// After append, the stmts list has 2 children: [ModuleStmt, appended FnDecl].
	ref_fn_id := ref_flat.child_at(ref_stmts_list, 1)
	ref_sub_sig := ref_flat.subtree_signature(ref_fn_id)

	mut b := ast.new_flat_builder()
	b.append_file(make_mymod_file())
	b.append_file(make_mymod_file())
	mut t_sub := create_runtime_init_fns_to_flat_test_transformer()
	t_sub.inject_runtime_const_init_fns_to_flat(mut b)
	sub_stmts_list := b.flat.child_at(b.flat.files[0].file_id, 2)
	sub_fn_id := b.flat.child_at(sub_stmts_list, 1)
	sub_sub_sig := b.flat.subtree_signature(sub_fn_id)
	assert ref_sub_sig == sub_sub_sig
}
