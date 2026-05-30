// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
// vtest build: macos
//
// Bit-equality pin for s166: `generated_fns_parts_from_flat` must produce
// a `GeneratedFnsParts` whose buckets contain the same fn names in the
// same order as the legacy `generated_fns_parts([]ast.File)`. The two
// flat-input helpers (`explicit_str_method_fn_names_from_flat` from s165
// and `generated_fn_module_from_flat` from s164) are individually pinned;
// this test pins their assembly.
module transformer

import v2.ast
import v2.pref as vpref
import v2.types

fn create_parts_from_flat_test_transformer() &Transformer {
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

fn fn_names_from_stmts(stmts []ast.Stmt) []string {
	mut names := []string{cap: stmts.len}
	for s in stmts {
		if s is ast.FnDecl {
			names << s.name
		}
	}
	return names
}

fn assert_parts_equal(legacy GeneratedFnsParts, fromflat GeneratedFnsParts) {
	legacy_core := fn_names_from_stmts(legacy.core_fns)
	flat_core := fn_names_from_stmts(fromflat.core_fns)
	assert legacy_core == flat_core

	legacy_user := fn_names_from_stmts(legacy.user_fns)
	flat_user := fn_names_from_stmts(fromflat.user_fns)
	assert legacy_user == flat_user

	assert legacy.module_fns.len == fromflat.module_fns.len
	for mod, _ in legacy.module_fns {
		legacy_mod := fn_names_from_stmts(legacy.module_fns[mod])
		flat_mod := fn_names_from_stmts(fromflat.module_fns[mod])
		assert legacy_mod == flat_mod
	}
}

// none branch: no needed_* maps populated → both return `none`.
fn test_generated_fns_parts_from_flat_none_when_no_generators_fire() {
	files := [ast.File{
		name: 'main.v'
		mod:  'main'
	}]
	flat := ast.flatten_files(files)
	mut t_legacy := create_parts_from_flat_test_transformer()
	mut t_flat := create_parts_from_flat_test_transformer()
	legacy := t_legacy.generated_fns_parts(files) or {
		assert true
		assert t_flat.generated_fns_parts_from_flat(&flat) == none
		return
	}
	_ = legacy
	assert false, 'expected none'
}

// str-fn routed to a registered module: Duration__str → 'time' bucket.
fn test_generated_fns_parts_from_flat_str_fn_routed_to_module() {
	// Two files: main + time. Both register `time__Duration__str` as needed.
	files := [
		ast.File{
			name: 'main.v'
			mod:  'main'
		},
		ast.File{
			name: 'time.v'
			mod:  'time'
		},
	]
	flat := ast.flatten_files(files)
	mut t_legacy := create_parts_from_flat_test_transformer()
	mut t_flat := create_parts_from_flat_test_transformer()
	t_legacy.needed_str_fns['time__Duration__str'] = ''
	t_flat.needed_str_fns['time__Duration__str'] = ''
	legacy := t_legacy.generated_fns_parts(files) or {
		assert false, 'legacy returned none'
		return
	}
	fromflat := t_flat.generated_fns_parts_from_flat(&flat) or {
		assert false, 'flat returned none'
		return
	}
	assert legacy.module_fns['time'].len == 1
	assert fromflat.module_fns['time'].len == 1
	assert_parts_equal(legacy, fromflat)
}

// core_fn (int_str) routed to core bucket regardless of files.
fn test_generated_fns_parts_from_flat_core_fn_routed_to_core() {
	files := [ast.File{
		name: 'main.v'
		mod:  'main'
	}]
	flat := ast.flatten_files(files)
	mut t_legacy := create_parts_from_flat_test_transformer()
	mut t_flat := create_parts_from_flat_test_transformer()
	// int__str is core (is_core_generated_fn matches it). Use a typed
	// non-existent str entry that gets generated but goes to the core bucket
	// via the prefix list. We rely on the existing `int__str` path.
	t_legacy.needed_str_fns['int__str'] = ''
	t_flat.needed_str_fns['int__str'] = ''
	legacy := t_legacy.generated_fns_parts(files) or {
		// int__str may short-circuit to skip generation; if so just assert
		// both paths agree by returning none.
		assert t_flat.generated_fns_parts_from_flat(&flat) == none
		return
	}
	fromflat := t_flat.generated_fns_parts_from_flat(&flat) or {
		assert false, 'flat returned none while legacy returned parts'
		return
	}
	assert_parts_equal(legacy, fromflat)
}

// user_fn (Array_int_str) routed to user bucket (not core, no module prefix).
fn test_generated_fns_parts_from_flat_array_str_fn_routed_to_user() {
	files := [ast.File{
		name: 'main.v'
		mod:  'main'
	}]
	flat := ast.flatten_files(files)
	mut t_legacy := create_parts_from_flat_test_transformer()
	mut t_flat := create_parts_from_flat_test_transformer()
	t_legacy.needed_str_fns['Array_int_str'] = 'int'
	t_flat.needed_str_fns['Array_int_str'] = 'int'
	legacy := t_legacy.generated_fns_parts(files) or {
		assert false, 'legacy returned none'
		return
	}
	fromflat := t_flat.generated_fns_parts_from_flat(&flat) or {
		assert false, 'flat returned none'
		return
	}
	assert_parts_equal(legacy, fromflat)
}

// mixed buckets: time__Duration__str → 'time' module, Array_int_str → user.
fn test_generated_fns_parts_from_flat_mixed_buckets_match() {
	files := [
		ast.File{
			name: 'main.v'
			mod:  'main'
		},
		ast.File{
			name: 'time.v'
			mod:  'time'
		},
	]
	flat := ast.flatten_files(files)
	mut t_legacy := create_parts_from_flat_test_transformer()
	mut t_flat := create_parts_from_flat_test_transformer()
	t_legacy.needed_str_fns['time__Duration__str'] = ''
	t_legacy.needed_str_fns['Array_int_str'] = 'int'
	t_flat.needed_str_fns['time__Duration__str'] = ''
	t_flat.needed_str_fns['Array_int_str'] = 'int'
	legacy := t_legacy.generated_fns_parts(files) or {
		assert false, 'legacy returned none'
		return
	}
	fromflat := t_flat.generated_fns_parts_from_flat(&flat) or {
		assert false, 'flat returned none'
		return
	}
	assert_parts_equal(legacy, fromflat)
}

// explicit str method skips generation: file has `fn (d Duration) str()` for
// time module, and `needed_str_fns` requests `time__Duration__str` — both
// paths must skip the generator (explicit_str_fns contains the name).
fn test_generated_fns_parts_from_flat_explicit_str_method_skips_generation() {
	files := [
		ast.File{
			name:  'time.v'
			mod:   'time'
			stmts: [
				ast.Stmt(ast.FnDecl{
					name:      'str'
					is_method: true
					receiver:  ast.Parameter{
						name: 'd'
						typ:  ast.Expr(ast.Ident{
							name: 'Duration'
						})
					}
					typ:       ast.FnType{
						return_type: ast.Ident{
							name: 'string'
						}
					}
				}),
			]
		},
	]
	flat := ast.flatten_files(files)
	mut t_legacy := create_parts_from_flat_test_transformer()
	mut t_flat := create_parts_from_flat_test_transformer()
	t_legacy.needed_str_fns['time__Duration__str'] = ''
	t_flat.needed_str_fns['time__Duration__str'] = ''
	// Generator should skip because explicit fn exists; both paths return none.
	legacy_opt := t_legacy.generated_fns_parts(files)
	flat_opt := t_flat.generated_fns_parts_from_flat(&flat)
	assert legacy_opt == none
	assert flat_opt == none
}
