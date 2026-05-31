// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
// vtest build: macos
//
// Bit-equality pin for the fifth flat-aware post_pass port (s159):
// `inject_live_reload_to_flat` must produce a FlatAst whose signature
// matches what the legacy `inject_live_reload` + `flatten_files` pair
// produces. Uses all six FlatBuilder primitives needed by live_reload:
// `replace_fn_body_stmts` (s158), `replace_file_stmt` (s155),
// `prepend_file_stmts` (s156), plus the legacy `inject_live_into_stmts`
// helper hydrating each FnDecl's body via `to_files_range`.
module transformer

import v2.ast
import v2.pref as vpref
import v2.types

// Local helper — `v test` compiles each `_test.v` file in isolation so the
// shared transformer-test scaffold isn't visible here. Seeded with two
// optional @[live] fn entries (controlled by `with_live`) and a stable
// source_file path so both paths see identical LiveReloadParts.
fn create_live_reload_to_flat_test_transformer(with_live bool) &Transformer {
	env := &types.Environment{}
	mut t := &Transformer{
		pref:                        &vpref.Preferences{}
		env:                         unsafe { env }
		needed_clone_fns:            map[string]string{}
		needed_array_contains_fns:   map[string]ArrayMethodInfo{}
		needed_array_index_fns:      map[string]ArrayMethodInfo{}
		needed_array_last_index_fns: map[string]ArrayMethodInfo{}
		local_decl_types:            map[string]types.Type{}
		live_source_file:            '/tmp/_live_reload_to_flat_test.v'
	}
	if with_live {
		t.live_fns << LiveFn{
			decl_name:    'tick'
			mangled_name: 'tick'
			is_method:    false
		}
	}
	return t
}

fn make_main_file_with_main_and_for() ast.File {
	return ast.File{
		name:  'main_live.v'
		mod:   'main'
		stmts: [
			ast.Stmt(ast.ModuleStmt{
				name: 'main'
			}),
			ast.Stmt(ast.FnDecl{
				name:  'tick'
				stmts: [
					ast.Stmt(ast.AssertStmt{
						expr: ast.Expr(ast.BasicLiteral{
							kind:  .number
							value: '1'
						})
					}),
				]
			}),
			ast.Stmt(ast.FnDecl{
				name:  'main'
				stmts: [
					ast.Stmt(ast.ForStmt{
						cond:  ast.Expr(ast.BasicLiteral{
							kind:  .number
							value: '1'
						})
						stmts: [
							ast.Stmt(ast.ExprStmt{
								expr: ast.Expr(ast.CallExpr{
									lhs: ast.Expr(ast.Ident{
										name: 'tick'
									})
								})
							}),
						]
					}),
				]
			}),
		]
	}
}

fn test_inject_live_reload_to_flat_empty_live_fns_is_noop() {
	mut b := ast.new_flat_builder()
	b.append_file(make_main_file_with_main_and_for())
	baseline_sig := b.flat.signature()
	mut t := create_live_reload_to_flat_test_transformer(false)
	t.inject_live_reload_to_flat(mut b)
	assert b.flat.signature() == baseline_sig
}

fn test_inject_live_reload_to_flat_no_main_fn_is_noop() {
	// File with no main fn — locator returns none even though live_fns is
	// populated; signature must be unchanged.
	module_only_file := ast.File{
		name:  'lib_live.v'
		mod:   'main'
		stmts: [
			ast.Stmt(ast.ModuleStmt{
				name: 'main'
			}),
			ast.Stmt(ast.FnDecl{
				name:  'tick'
				stmts: []ast.Stmt{}
			}),
		]
	}
	mut b := ast.new_flat_builder()
	b.append_file(module_only_file)
	baseline_sig := b.flat.signature()
	mut t := create_live_reload_to_flat_test_transformer(true)
	t.inject_live_reload_to_flat(mut b)
	assert b.flat.signature() == baseline_sig
}

fn test_inject_live_reload_to_flat_signature_matches_legacy() {
	// Populated live_fns + main fn — splice happens. Compare the stmts-list
	// subtree of the spliced file in both paths via `subtree_signature` on
	// the file root's edge 2.
	//
	// Why subtree-of-stmts-list instead of full signature: the .file node's
	// `extra` slot stores `intern(mod)` as a raw index. The legacy path
	// flattens an already-mutated file (c_decls + global_decls + body
	// emitted in declaration order → 'main' interned LATE) vs the flat
	// path (file appended first → 'main' interned at slot 0 → splice
	// re-emits the file root reusing saved mod_idx). Comparing the stmts
	// list subtree skips the leaky `extra` field.
	mut ref_files := [make_main_file_with_main_and_for()]
	mut t_ref := create_live_reload_to_flat_test_transformer(true)
	t_ref.inject_live_reload(mut ref_files)
	ref_flat := ast.flatten_files(ref_files)
	ref_stmts_list_id := ref_flat.child_at(ref_flat.files[0].file_id, 2)
	ref_sub_sig := ref_flat.subtree_signature(ref_stmts_list_id)

	mut b := ast.new_flat_builder()
	b.append_file(make_main_file_with_main_and_for())
	mut t_sub := create_live_reload_to_flat_test_transformer(true)
	t_sub.inject_live_reload_to_flat(mut b)
	sub_stmts_list_id := b.flat.child_at(b.flat.files[0].file_id, 2)
	sub_sub_sig := b.flat.subtree_signature(sub_stmts_list_id)

	assert ref_sub_sig == sub_sub_sig
}
