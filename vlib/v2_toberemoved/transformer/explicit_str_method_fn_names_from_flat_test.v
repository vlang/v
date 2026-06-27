// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
// vtest build: macos
//
// Bit-equality pin for s165: `explicit_str_method_fn_names_from_flat`
// (FlatAst input) must produce the same `map[string]bool` as the legacy
// `explicit_str_method_fn_names([]ast.File)` across the module-prefix
// branches: main excluded, builtin excluded, module-prefixed name short-
// circuit (already-`__`), no-method skip, and non-str method skip.
module transformer

import v2.ast
import v2.pref as vpref
import v2.types

fn create_str_fn_names_test_transformer() &Transformer {
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

fn make_str_method_decl(recv_name string, fn_name string) ast.Stmt {
	return ast.Stmt(ast.FnDecl{
		name:      fn_name
		is_method: true
		receiver:  ast.Parameter{
			name: 'r'
			typ:  ast.Expr(ast.Ident{
				name: recv_name
			})
		}
		typ:       ast.FnType{
			return_type: ast.Ident{
				name: 'string'
			}
		}
	})
}

fn make_plain_fn_decl(fn_name string) ast.Stmt {
	return ast.Stmt(ast.FnDecl{
		name:      fn_name
		is_method: false
		typ:       ast.FnType{}
	})
}

fn assert_maps_equal(legacy map[string]bool, fromflat map[string]bool) {
	assert legacy.len == fromflat.len
	for k, v in legacy {
		assert fromflat[k] == v
	}
	for k, v in fromflat {
		assert legacy[k] == v
	}
}

fn run_str_fn_names_parity(files []ast.File) {
	t := create_str_fn_names_test_transformer()
	flat := ast.flatten_files(files)
	legacy := t.explicit_str_method_fn_names(files)
	fromflat := t.explicit_str_method_fn_names_from_flat(&flat)
	assert_maps_equal(legacy, fromflat)
}

fn test_explicit_str_method_fn_names_from_flat_main_module_no_prefix() {
	// In `main` the recv_name is NOT module-prefixed.
	files := [
		ast.File{
			name:  'main.v'
			mod:   'main'
			stmts: [make_str_method_decl('Foo', 'str')]
		},
	]
	t := create_str_fn_names_test_transformer()
	flat := ast.flatten_files(files)
	legacy := t.explicit_str_method_fn_names(files)
	fromflat := t.explicit_str_method_fn_names_from_flat(&flat)
	assert legacy.len == 1
	assert legacy['Foo__str']
	assert_maps_equal(legacy, fromflat)
}

fn test_explicit_str_method_fn_names_from_flat_builtin_module_no_prefix() {
	// In `builtin` the recv_name is NOT module-prefixed either.
	files := [
		ast.File{
			name:  'builtin.v'
			mod:   'builtin'
			stmts: [make_str_method_decl('string', 'str')]
		},
	]
	t := create_str_fn_names_test_transformer()
	flat := ast.flatten_files(files)
	legacy := t.explicit_str_method_fn_names(files)
	fromflat := t.explicit_str_method_fn_names_from_flat(&flat)
	assert legacy.len == 1
	assert legacy['string__str']
	assert_maps_equal(legacy, fromflat)
}

fn test_explicit_str_method_fn_names_from_flat_named_module_gets_prefix() {
	// In a non-main/non-builtin module the recv_name is module-prefixed.
	files := [
		ast.File{
			name:  'time.v'
			mod:   'time'
			stmts: [make_str_method_decl('Duration', 'str')]
		},
	]
	t := create_str_fn_names_test_transformer()
	flat := ast.flatten_files(files)
	legacy := t.explicit_str_method_fn_names(files)
	fromflat := t.explicit_str_method_fn_names_from_flat(&flat)
	assert legacy.len == 1
	assert legacy['time__Duration__str']
	assert_maps_equal(legacy, fromflat)
}

fn test_explicit_str_method_fn_names_from_flat_dotted_module_replaced() {
	// Module 'os.windows' -> recv_name prefix 'os__windows__'.
	files := [
		ast.File{
			name:  'win.v'
			mod:   'os.windows'
			stmts: [make_str_method_decl('Handle', 'str')]
		},
	]
	run_str_fn_names_parity(files)
}

fn test_explicit_str_method_fn_names_from_flat_already_prefixed_skips_prefix() {
	// recv_name already containing `__` does NOT get a second prefix.
	files := [
		ast.File{
			name:  'time.v'
			mod:   'time'
			stmts: [make_str_method_decl('time__Duration', 'str')]
		},
	]
	t := create_str_fn_names_test_transformer()
	flat := ast.flatten_files(files)
	legacy := t.explicit_str_method_fn_names(files)
	fromflat := t.explicit_str_method_fn_names_from_flat(&flat)
	assert legacy.len == 1
	assert legacy['time__Duration__str']
	assert_maps_equal(legacy, fromflat)
}

fn test_explicit_str_method_fn_names_from_flat_non_str_method_ignored() {
	// is_method && name != 'str' must NOT be picked up.
	files := [
		ast.File{
			name:  'main.v'
			mod:   'main'
			stmts: [make_str_method_decl('Foo', 'other')]
		},
	]
	t := create_str_fn_names_test_transformer()
	flat := ast.flatten_files(files)
	legacy := t.explicit_str_method_fn_names(files)
	fromflat := t.explicit_str_method_fn_names_from_flat(&flat)
	assert legacy.len == 0
	assert_maps_equal(legacy, fromflat)
}

fn test_explicit_str_method_fn_names_from_flat_plain_fn_str_ignored() {
	// A plain (non-method) `fn str()` must NOT be picked up.
	files := [
		ast.File{
			name:  'main.v'
			mod:   'main'
			stmts: [make_plain_fn_decl('str')]
		},
	]
	t := create_str_fn_names_test_transformer()
	flat := ast.flatten_files(files)
	legacy := t.explicit_str_method_fn_names(files)
	fromflat := t.explicit_str_method_fn_names_from_flat(&flat)
	assert legacy.len == 0
	assert_maps_equal(legacy, fromflat)
}

fn test_explicit_str_method_fn_names_from_flat_multi_file_collects_all() {
	// Multiple files / modules — both paths must collect the same set.
	files := [
		ast.File{
			name:  'main.v'
			mod:   'main'
			stmts: [
				make_str_method_decl('Foo', 'str'),
				make_str_method_decl('Bar', 'other'),
			]
		},
		ast.File{
			name:  'time.v'
			mod:   'time'
			stmts: [
				make_str_method_decl('Duration', 'str'),
			]
		},
		ast.File{
			name:  'builtin.v'
			mod:   'builtin'
			stmts: [
				make_str_method_decl('string', 'str'),
			]
		},
	]
	t := create_str_fn_names_test_transformer()
	flat := ast.flatten_files(files)
	legacy := t.explicit_str_method_fn_names(files)
	fromflat := t.explicit_str_method_fn_names_from_flat(&flat)
	assert legacy.len == 3
	assert legacy['Foo__str']
	assert legacy['time__Duration__str']
	assert legacy['string__str']
	assert_maps_equal(legacy, fromflat)
}
