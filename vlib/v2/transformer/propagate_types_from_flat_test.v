// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
// vtest build: macos
//
// Bit-equality pin for s167: `propagate_types_from_flat` (a thin
// rehydrate-then-call wrapper) must produce the same `t.env` mutations as
// `propagate_types`. Also pins `apply_post_pass_tail_from_flat` against
// `apply_post_pass_tail`.
//
// The wrapper rehydrates `flat` into `[]ast.File` via
// `flat.to_files_range(0, flat.files.len)` and delegates to the legacy
// walker. Behavior is identical by construction provided flatten+rehydrate
// preserves the bits propagate_types reads (pos.id, stmt/expr shape).
module transformer

import v2.ast
import v2.pref as vpref
import v2.types

fn create_propagate_test_transformer(backend vpref.Backend) &Transformer {
	env := &types.Environment{}
	return &Transformer{
		pref:                        &vpref.Preferences{
			backend: backend
		}
		env:                         unsafe { env }
		needed_clone_fns:            map[string]string{}
		needed_array_contains_fns:   map[string]ArrayMethodInfo{}
		needed_array_index_fns:      map[string]ArrayMethodInfo{}
		needed_array_last_index_fns: map[string]ArrayMethodInfo{}
		local_decl_types:            map[string]types.Type{}
	}
}

fn make_propagate_test_files() []ast.File {
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
		ast.File{
			name:  'time.v'
			mod:   'time'
			stmts: [
				ast.Stmt(ast.ModuleStmt{
					name: 'time'
				}),
			]
		},
	]
}

// Calling propagate_types_from_flat on an empty flat must not crash and
// must produce no env mutations.
fn test_propagate_types_from_flat_empty_flat_is_noop() {
	mut t := create_propagate_test_transformer(.cleanc)
	flat := ast.FlatAst{}
	pre := t.env.expr_type_values.len
	t.propagate_types_from_flat(&flat)
	assert t.env.expr_type_values.len == pre
}

// On a non-empty flat, propagate_types_from_flat visits the same files as
// propagate_types(files) and produces the same env state.
fn test_propagate_types_from_flat_matches_legacy_for_simple_files() {
	files := make_propagate_test_files()
	flat := ast.flatten_files(files)

	mut t_legacy := create_propagate_test_transformer(.cleanc)
	mut t_flat := create_propagate_test_transformer(.cleanc)

	t_legacy.propagate_types(files)
	t_flat.propagate_types_from_flat(&flat)

	// Both transformers should end with identical env state. With no
	// expressions to propagate, both expr_type_values arrays stay empty.
	assert t_legacy.env.expr_type_values.len == t_flat.env.expr_type_values.len
	// cur_module is set to the last visited file's mod on both paths.
	assert t_legacy.cur_module == t_flat.cur_module
}

// apply_post_pass_tail_from_flat must match apply_post_pass_tail for the
// same input (synth_types + cached_fn_scopes seeded identically).
fn test_apply_post_pass_tail_from_flat_matches_legacy_arm64_skip() {
	// On arm64 the propagate_types step is skipped — both helpers reduce to
	// synth_types + fn_scopes pushes which don't touch the file/flat input.
	files := make_propagate_test_files()
	flat := ast.flatten_files(files)

	mut t_legacy := create_propagate_test_transformer(.arm64)
	mut t_flat := create_propagate_test_transformer(.arm64)

	// Seed both with one synth_types entry and one fn_scope.
	t_legacy.synth_types[42] = types.Type(types.Primitive{
		size: 4
	})
	t_flat.synth_types[42] = types.Type(types.Primitive{
		size: 4
	})
	t_legacy.cached_fn_scopes['Foo__bar'] = &types.Scope{}
	t_flat.cached_fn_scopes['Foo__bar'] = &types.Scope{}

	t_legacy.apply_post_pass_tail(files)
	t_flat.apply_post_pass_tail_from_flat(&flat)

	// expr_type_values must contain the synth_types[42] entry on both.
	assert t_legacy.env.expr_type_values.len > 42
	assert t_flat.env.expr_type_values.len > 42
	assert t_legacy.env.expr_type_values.len == t_flat.env.expr_type_values.len
	// fn_scopes must have the seeded key on both.
	lock t_legacy.env.fn_scopes {
		assert 'Foo__bar' in t_legacy.env.fn_scopes
	}
	lock t_flat.env.fn_scopes {
		assert 'Foo__bar' in t_flat.env.fn_scopes
	}
}

// apply_post_pass_tail_from_flat on non-arm64 must run propagate path.
fn test_apply_post_pass_tail_from_flat_matches_legacy_cleanc() {
	files := make_propagate_test_files()
	flat := ast.flatten_files(files)

	mut t_legacy := create_propagate_test_transformer(.cleanc)
	mut t_flat := create_propagate_test_transformer(.cleanc)

	t_legacy.apply_post_pass_tail(files)
	t_flat.apply_post_pass_tail_from_flat(&flat)

	// Both paths should end with identical env state. With a stmt-free
	// fixture, propagate_types is a no-op and expr_type_values stays empty.
	assert t_legacy.env.expr_type_values.len == t_flat.env.expr_type_values.len
	assert t_legacy.cur_module == t_flat.cur_module
}
