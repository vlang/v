// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
// vtest build: macos
//
// Bit-equality pin for s167: `propagate_types_from_flat` walks flat files
// directly and must produce the same `t.env` mutations as `propagate_types`.
// Also pins `apply_post_pass_tail_from_flat` against `apply_post_pass_tail`.
//
// The flat path walks stmt/expression cursors directly. Behavior should match
// legacy provided flattening preserves the bits propagate_types reads
// (pos.id, stmt/expr shape).
module transformer

import v2.ast
import v2.pref as vpref
import v2.token
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

fn propagate_test_pos(id int) token.Pos {
	return token.Pos{
		offset: id
		id:     id
	}
}

fn make_propagate_expr_test_files() []ast.File {
	arr_ident := ast.Expr(ast.Ident{
		name: 'arr'
		pos:  propagate_test_pos(1)
	})
	arr_len := ast.Expr(ast.SelectorExpr{
		lhs: arr_ident
		rhs: ast.Ident{
			name: 'len'
			pos:  propagate_test_pos(2)
		}
		pos: propagate_test_pos(3)
	})
	literal := ast.Expr(ast.BasicLiteral{
		kind:  .number
		value: '7'
		pos:   propagate_test_pos(6)
	})
	clone_call := ast.Expr(ast.CallExpr{
		lhs:  ast.Expr(ast.Ident{
			name: 'string__clone'
			pos:  propagate_test_pos(7)
		})
		args: [
			ast.Expr(ast.StringLiteral{
				kind:  .v
				value: 'x'
				pos:   propagate_test_pos(8)
			}),
		]
		pos:  propagate_test_pos(9)
	})
	return [
		ast.File{
			name:  'expr.v'
			mod:   'main'
			stmts: [
				ast.Stmt(ast.ModuleStmt{
					name: 'main'
				}),
				ast.Stmt(ast.AssignStmt{
					op:  .decl_assign
					lhs: [
						ast.Expr(ast.Ident{
							name: 'n'
							pos:  propagate_test_pos(4)
						}),
					]
					rhs: [
						arr_len,
					]
				}),
				ast.Stmt(ast.AssignStmt{
					op:  .decl_assign
					lhs: [
						ast.Expr(ast.Ident{
							name: 'm'
							pos:  propagate_test_pos(5)
						}),
					]
					rhs: [
						literal,
					]
				}),
				ast.Stmt(ast.AssignStmt{
					op:  .decl_assign
					lhs: [
						ast.Expr(ast.Ident{
							name: 's'
							pos:  propagate_test_pos(10)
						}),
					]
					rhs: [
						clone_call,
					]
				}),
			]
		},
	]
}

fn seed_propagate_expr_scope(mut t Transformer) {
	mut main_scope := types.new_scope(unsafe { nil })
	main_scope.insert('arr', types.Type(types.Array{
		elem_type: types.Type(types.int_)
	}))
	t.cached_scopes['main'] = main_scope
}

// Calling propagate_types_from_flat on an empty flat must not crash and
// must produce no env mutations.
fn test_propagate_types_from_flat_empty_flat_is_noop() {
	mut t := create_propagate_test_transformer(.cleanc)
	flat := ast.FlatAst{}
	pre := t.env.expr_type_count()
	t.propagate_types_from_flat(&flat)
	assert t.env.expr_type_count() == pre
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
	// expressions to propagate, both expression type maps stay empty.
	assert t_legacy.env.expr_type_count() == t_flat.env.expr_type_count()
	// cur_module is set to the last visited file's mod on both paths.
	assert t_legacy.cur_module == t_flat.cur_module
}

fn test_propagate_types_from_flat_matches_legacy_for_expression_shapes() {
	files := make_propagate_expr_test_files()
	flat := ast.flatten_files(files)

	mut t_legacy := create_propagate_test_transformer(.cleanc)
	mut t_flat := create_propagate_test_transformer(.cleanc)
	seed_propagate_expr_scope(mut t_legacy)
	seed_propagate_expr_scope(mut t_flat)

	t_legacy.propagate_types(files)
	t_flat.propagate_types_from_flat(&flat)

	assert t_legacy.env.expr_type_count() == t_flat.env.expr_type_count()
	for id in [3, 4, 5, 6, 8, 9, 10] {
		legacy_type := t_legacy.env.get_expr_type(id) or { panic('missing legacy type ${id}') }
		flat_type := t_flat.env.get_expr_type(id) or { panic('missing flat type ${id}') }
		assert legacy_type.name() == flat_type.name()
	}
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

	// Expression type maps must contain the synth_types[42] entry on both.
	assert t_legacy.env.has_expr_type(42)
	assert t_flat.env.has_expr_type(42)
	assert t_legacy.env.expr_type_count() == t_flat.env.expr_type_count()
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
	// fixture, propagate_types is a no-op and expression type maps stay empty.
	assert t_legacy.env.expr_type_count() == t_flat.env.expr_type_count()
	assert t_legacy.cur_module == t_flat.cur_module
}
