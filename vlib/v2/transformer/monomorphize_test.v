// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
// vtest build: !windows
module transformer

import os
import v2.ast
import v2.parser
import v2.pref as vpref
import v2.token
import v2.types

fn mono_test_transformer() &Transformer {
	env := &types.Environment{}
	return &Transformer{
		pref:                        &vpref.Preferences{}
		env:                         unsafe { env }
		needed_clone_fns:            map[string]string{}
		needed_array_contains_fns:   map[string]ArrayMethodInfo{}
		needed_array_index_fns:      map[string]ArrayMethodInfo{}
		needed_array_last_index_fns: map[string]ArrayMethodInfo{}
	}
}

fn test_get_expr_type_prefers_cloned_init_expr_type_over_stale_position_type() {
	mut env := types.Environment.new()
	pos := token.Pos{
		id: 7
	}
	env.set_expr_type(pos.id, types.Type(types.Struct{
		name: 'gitly__GitHubRepoInfo'
	}))
	mut t := mono_test_transformer()
	t.env = env
	t.cur_module = 'json2'
	mut config_scope := types.new_scope(unsafe { nil })
	config_scope.insert_type('Config', types.Type(types.Struct{
		name: 'config__Config'
	}))
	t.cached_scopes['config'] = config_scope
	expr := ast.Expr(ast.InitExpr{
		typ: ast.Expr(ast.SelectorExpr{
			lhs: ast.Expr(ast.Ident{
				name: 'config'
			})
			rhs: ast.Ident{
				name: 'Config'
			}
		})
		pos: pos
	})
	typ := t.get_expr_type(expr) or { panic('expected cloned init expression type') }
	assert typ.name() == 'config__Config'
}

// substitute_type: primitives

fn test_substitute_type_returns_same_type_when_bindings_empty() {
	original := types.Type(types.NamedType('T'))
	out := substitute_type(original, map[string]types.Type{})
	assert out.name() == 'T'
}

fn test_substitute_type_replaces_placeholder_named_type() {
	bindings := {
		'T': types.Type(types.int_)
	}
	out := substitute_type(types.Type(types.NamedType('T')), bindings)
	assert out.name() == 'int'
}

fn test_generic_bindings_from_method_call_args_skip_receiver_param() {
	mut t := mono_test_transformer()
	t.local_decl_types['file'] = types.Type(types.Struct{
		name: 'File'
	})
	info := CallFnInfo{
		param_types:                    [
			types.Type(types.Struct{
				name: 'Walker'
			}),
			types.Type(types.NamedType('T')),
		]
		generic_param_names_by_param:   ['', 'T']
		generic_param_indexes_by_param: [-1, 0]
		generic_params:                 ['T']
	}
	bindings := t.generic_bindings_from_call_args(info, [
		ast.Expr(ast.Ident{
			name: 'file'
		}),
	]) or { panic('expected binding') }
	concrete := bindings['T'] or { panic('missing T binding') }
	assert concrete.name() == 'File'
}

fn test_generic_bindings_from_call_args_use_ast_direct_generic_metadata() {
	mut t := mono_test_transformer()
	t.local_decl_types['file'] = types.Type(types.Struct{
		name: 'File'
	})
	info := CallFnInfo{
		param_types:                    [
			types.Type(types.void_),
		]
		generic_param_names_by_param:   ['T']
		generic_param_indexes_by_param: [0]
		generic_params:                 ['T']
	}
	bindings := t.generic_bindings_from_call_args(info, [
		ast.Expr(ast.Ident{
			name: 'file'
		}),
	]) or { panic('expected binding from direct generic parameter metadata') }
	concrete := bindings['T'] or { panic('missing T binding') }
	assert concrete.name() == 'File'
}

fn test_generic_call_info_lookup_does_not_match_unrelated_short_name() {
	mut t := mono_test_transformer()
	t.generic_fn_decl_index['write'] = ast.FnDecl{
		name: 'write'
		typ:  ast.FnType{
			generic_params: [
				ast.Expr(ast.Ident{
					name: 'T'
				}),
			]
		}
	}
	if _ := t.generic_fn_decl_for_call_info('v__Gen__write') {
		assert false, 'qualified non-generic methods must not resolve to unrelated generic functions'
	}
	if decl := t.generic_fn_decl_for_call_info('write') {
		assert decl.name == 'write'
	} else {
		assert false, 'expected exact generic function lookup to work'
	}
}

fn test_resolve_monomorphize_decl_key_prefers_module_suffix_over_short_name() {
	t := mono_test_transformer()
	decl := ast.FnDecl{
		name: 'uniq'
	}
	decl_node := {
		'uniq':         decl
		'arrays__uniq': decl
	}
	resolved := t.resolve_monomorphize_decl_key('http__arrays__uniq', decl_node) or {
		panic('missing resolved key')
	}
	assert resolved == 'arrays__uniq'
}

fn test_resolve_monomorphize_decl_key_rejects_unrelated_short_name() {
	t := mono_test_transformer()
	decl := ast.FnDecl{
		name: 'encode'
	}
	decl_node := {
		'encode':        decl
		'json2__encode': decl
	}
	if _ := t.resolve_monomorphize_decl_key('base64__encode', decl_node) {
		assert false, 'qualified generic lookup must not fall back to unrelated short names'
	}
	if _ := t.resolve_monomorphize_decl_key('base64.encode', decl_node) {
		assert false, 'dotted generic lookup must not fall back to unrelated short names'
	}
}

fn test_generic_call_info_bare_lookup_stays_in_current_module() {
	mut t := mono_test_transformer()
	t.cur_module = 'cbor'
	json_decl := ast.FnDecl{
		name: 'encode'
		typ:  ast.FnType{
			generic_params: [
				ast.Expr(ast.Ident{
					name: 'JsonT'
				}),
			]
		}
	}
	cbor_decl := ast.FnDecl{
		name: 'encode'
		typ:  ast.FnType{
			generic_params: [
				ast.Expr(ast.Ident{
					name: 'CborT'
				}),
			]
		}
	}
	t.generic_fn_decl_index['encode'] = json_decl
	t.generic_fn_decl_index['json2__encode'] = json_decl
	t.generic_fn_decl_index['cbor__encode'] = cbor_decl
	decl := t.generic_fn_decl_for_call_info('encode') or { panic('missing current module decl') }
	gp := decl.typ.generic_params[0] as ast.Ident
	assert gp.name == 'CborT'
}

fn test_transformer_pointer_guards_accept_low_canonical_pointers() {
	assert sumtype_payload_word_is_valid(1, 0x10000)
	assert transformer_data_ptr_has_valid_address(0x10000)
	assert !sumtype_payload_word_is_valid(1, 0)
	assert !sumtype_payload_word_is_valid(1, 3)
}

fn mono_transform_dijkstra_shape_for_test() []ast.File {
	source_path := os.join_path(os.temp_dir(), 'v2_mono_dijkstra_shape_${os.getpid()}.v')
	os.write_file(source_path, '
module main

struct NODE {
mut:
	data int
	priority int
}

fn push_pq[T](mut prior_queue []T, data int, priority int) {
	_ = prior_queue
	_ = data
	_ = priority
}

fn updating_priority[T](mut prior_queue []T, search_data int, new_priority int) {
	_ = prior_queue
	_ = search_data
	_ = new_priority
}

fn departure_priority[T](mut prior_queue []T) int {
	_ = prior_queue
	return 0
}

fn all_adjacents[T](g [][]T, v int) []int {
	mut temp := []int{}
	for i in 0 .. g.len {
		if g[v][i] > 0 {
			temp << i
		}
	}
	return temp
}

fn print_solution[T](dist []T) {
	_ = dist
}

fn print_paths_dist[T](path []T, dist []T) {
	_ = path
	_ = dist
}

fn dijkstra(g [][]int, s int) {
	mut pq_queue := []NODE{}
	push_pq(mut pq_queue, s, 0)
	mut n := g.len
	mut dist := []int{len: n, init: -1}
	mut path := []int{len: n, init: -1}
	dist[s] = 0
	for pq_queue.len != 0 {
		mut v := departure_priority(mut pq_queue)
		mut adjs_of_v := all_adjacents(g, v)
		mut new_dist := 0
		for w in adjs_of_v {
			new_dist = dist[v] + g[v][w]
			if dist[w] == -1 {
				dist[w] = new_dist
				push_pq(mut pq_queue, w, dist[w])
				path[w] = v
			}
			if dist[w] > new_dist {
				dist[w] = new_dist
				updating_priority(mut pq_queue, w, dist[w])
				path[w] = v
			}
		}
	}
	print_solution(dist)
	print_paths_dist(path, dist)
}
') or {
		panic('failed to write temp source')
	}
	defer {
		os.rm(source_path) or {}
	}
	prefs := &vpref.Preferences{
		backend:     .x64
		no_parallel: true
	}
	mut file_set := token.FileSet.new()
	mut par := parser.Parser.new(prefs)
	files := par.parse_files([source_path], mut file_set)
	mut env := types.Environment.new()
	mut checker := types.Checker.new(prefs, file_set, env)
	checker.check_files(files)
	mut trans := Transformer.new_with_pref(env, prefs)
	return trans.transform_files(files)
}

fn mono_collect_call_names_from_expr(expr ast.Expr, mut names []string) {
	match expr {
		ast.ArrayInitExpr {
			for nested in expr.exprs {
				mono_collect_call_names_from_expr(nested, mut names)
			}
		}
		ast.CallExpr {
			if expr.lhs is ast.Ident {
				names << expr.lhs.name
			}
			mono_collect_call_names_from_expr(expr.lhs, mut names)
			for arg in expr.args {
				mono_collect_call_names_from_expr(arg, mut names)
			}
		}
		ast.IfExpr {
			mono_collect_call_names_from_expr(expr.cond, mut names)
			for stmt in expr.stmts {
				mono_collect_call_names_from_stmt(stmt, mut names)
			}
			mono_collect_call_names_from_expr(expr.else_expr, mut names)
		}
		ast.InfixExpr {
			mono_collect_call_names_from_expr(expr.lhs, mut names)
			mono_collect_call_names_from_expr(expr.rhs, mut names)
		}
		ast.ModifierExpr {
			mono_collect_call_names_from_expr(expr.expr, mut names)
		}
		ast.ParenExpr {
			mono_collect_call_names_from_expr(expr.expr, mut names)
		}
		ast.PrefixExpr {
			mono_collect_call_names_from_expr(expr.expr, mut names)
		}
		ast.SelectorExpr {
			mono_collect_call_names_from_expr(expr.lhs, mut names)
		}
		else {}
	}
}

fn mono_collect_call_names_from_stmt(stmt ast.Stmt, mut names []string) {
	match stmt {
		ast.AssignStmt {
			for expr in stmt.rhs {
				mono_collect_call_names_from_expr(expr, mut names)
			}
		}
		ast.ExprStmt {
			mono_collect_call_names_from_expr(stmt.expr, mut names)
		}
		ast.ForStmt {
			mono_collect_call_names_from_expr(stmt.cond, mut names)
			for nested in stmt.stmts {
				mono_collect_call_names_from_stmt(nested, mut names)
			}
		}
		ast.ReturnStmt {
			for expr in stmt.exprs {
				mono_collect_call_names_from_expr(expr, mut names)
			}
		}
		else {}
	}
}

fn mono_call_names_for_fn(files []ast.File, fn_name string) []string {
	mut names := []string{}
	for file in files {
		for stmt in file.stmts {
			if stmt is ast.FnDecl && stmt.name == fn_name {
				for nested in stmt.stmts {
					mono_collect_call_names_from_stmt(nested, mut names)
				}
			}
		}
	}
	return names
}

fn mono_has_fn(files []ast.File, fn_name string) bool {
	for file in files {
		for stmt in file.stmts {
			if stmt is ast.FnDecl && stmt.name == fn_name {
				return true
			}
		}
	}
	return false
}

fn test_transform_dijkstra_shape_rewrites_inferred_generic_nested_array_call() {
	files := mono_transform_dijkstra_shape_for_test()
	call_names := mono_call_names_for_fn(files, 'dijkstra')
	assert 'all_adjacents_T_int' in call_names
	assert 'all_adjacents' !in call_names
	assert mono_has_fn(files, 'all_adjacents_T_int')
}

fn test_inferred_generic_call_name_for_nested_array_param() {
	mut t := mono_test_transformer()
	t.cur_module = 'main'
	t.generic_fn_decl_index['all_adjacents'] = ast.FnDecl{
		name: 'all_adjacents'
		typ:  ast.FnType{
			generic_params: [
				ast.Expr(ast.Ident{
					name: 'T'
				}),
			]
			params:         [
				ast.Parameter{
					name: 'g'
					typ:  ast.Expr(ast.Type(ast.ArrayType{
						elem_type: ast.Expr(ast.Type(ast.ArrayType{
							elem_type: ast.Expr(ast.Ident{
								name: 'T'
							})
						}))
					}))
				},
				ast.Parameter{
					name: 'v'
					typ:  ast.Expr(ast.Ident{
						name: 'int'
					})
				},
			]
		}
	}
	t.local_decl_types['g'] = types.Type(types.Array{
		elem_type: types.Type(types.Array{
			elem_type: types.Type(types.int_)
		})
	})
	info := CallFnInfo{
		param_types:                    [
			types.Type(types.Array{
				elem_type: types.Type(types.Array{
					elem_type: types.Type(types.NamedType('T'))
				})
			}),
			types.Type(types.int_),
		]
		generic_param_names_by_param:   ['T', '']
		generic_param_indexes_by_param: [0, -1]
		generic_params:                 ['T']
	}
	call_name := t.inferred_generic_call_name('all_adjacents', info, [
		ast.Expr(ast.Ident{
			name: 'g'
		}),
		ast.Expr(ast.Ident{
			name: 'v'
		}),
	]) or { panic('missing inferred generic call name') }
	assert call_name == 'all_adjacents_T_int'
}

fn test_generic_call_info_preserves_nested_array_generic_param() {
	mut t := mono_test_transformer()
	t.cur_module = 'main'
	t.generic_fn_decl_index['all_adjacents'] = ast.FnDecl{
		name: 'all_adjacents'
		typ:  ast.FnType{
			generic_params: [
				ast.Expr(ast.Ident{
					name: 'T'
				}),
			]
			params:         [
				ast.Parameter{
					name: 'g'
					typ:  ast.Expr(ast.Type(ast.ArrayType{
						elem_type: ast.Expr(ast.Type(ast.ArrayType{
							elem_type: ast.Expr(ast.Ident{
								name: 'T'
							})
						}))
					}))
				},
			]
		}
	}
	info := t.generic_call_info_for_decl('all_adjacents') or { panic('missing generic call info') }
	assert info.param_types.len == 1
	outer := info.param_types[0] as types.Array
	inner := outer.elem_type as types.Array
	assert inner.elem_type.name() == 'T'
}

fn test_transform_rewrites_inferred_generic_call_for_nested_array_param() {
	mut t := mono_test_transformer()
	t.cur_module = 'main'
	t.generic_fn_decl_index['all_adjacents'] = ast.FnDecl{
		name: 'all_adjacents'
		typ:  ast.FnType{
			generic_params: [
				ast.Expr(ast.Ident{
					name: 'T'
				}),
			]
			params:         [
				ast.Parameter{
					name: 'g'
					typ:  ast.Expr(ast.Type(ast.ArrayType{
						elem_type: ast.Expr(ast.Type(ast.ArrayType{
							elem_type: ast.Expr(ast.Ident{
								name: 'T'
							})
						}))
					}))
				},
				ast.Parameter{
					name: 'v'
					typ:  ast.Expr(ast.Ident{
						name: 'int'
					})
				},
			]
		}
	}
	t.local_decl_types['g'] = types.Type(types.Array{
		elem_type: types.Type(types.Array{
			elem_type: types.Type(types.int_)
		})
	})
	t.local_decl_types['v'] = types.Type(types.int_)
	out := t.transform_call_expr(ast.CallExpr{
		lhs:  ast.Ident{
			name: 'all_adjacents'
		}
		args: [
			ast.Expr(ast.Ident{
				name: 'g'
			}),
			ast.Expr(ast.Ident{
				name: 'v'
			}),
		]
	})
	assert out is ast.CallExpr
	call := out as ast.CallExpr
	assert call.lhs is ast.Ident
	assert (call.lhs as ast.Ident).name == 'all_adjacents_T_int'
}

fn test_clone_fn_decl_substitutes_fn_literal_signature() {
	mut t := mono_test_transformer()
	decl := ast.FnDecl{
		name:  'make'
		typ:   ast.FnType{
			generic_params: [
				ast.Expr(ast.Ident{
					name: 'T'
				}),
			]
		}
		stmts: [
			ast.Stmt(ast.ReturnStmt{
				exprs: [
					ast.Expr(ast.FnLiteral{
						typ:   ast.FnType{
							params:      [
								ast.Parameter{
									name: 'x'
									typ:  ast.Expr(ast.Ident{
										name: 'T'
									})
								},
							]
							return_type: ast.Expr(ast.Ident{
								name: 'T'
							})
						}
						stmts: [
							ast.Stmt(ast.ReturnStmt{
								exprs: [
									ast.Expr(ast.Ident{
										name: 'x'
									}),
								]
							}),
						]
					}),
				]
			}),
		]
	}
	cloned := t.clone_fn_decl_with_substitutions(decl, {
		'T': types.Type(types.int_)
	}, 'make_T_int', 'main', 'main')
	ret := cloned.stmts[0] as ast.ReturnStmt
	lit := ret.exprs[0] as ast.FnLiteral
	param_typ := lit.typ.params[0].typ as ast.Ident
	return_typ := lit.typ.return_type as ast.Ident

	assert param_typ.name == 'int'
	assert return_typ.name == 'int'
}

fn test_monomorphize_pass_keeps_imported_clone_in_declaring_file() {
	mut t := mono_test_transformer()
	t.env = types.Environment.new()
	bindings := {
		'T': types.Type(types.string_)
	}
	t.env.generic_types['m__wrap'] = [bindings]
	t.generic_spec_owner_file[generic_spec_owner_key('m__wrap', bindings)] = 1
	helper_decl := ast.FnDecl{
		name: 'helper'
		typ:  ast.FnType{
			return_type: ast.Expr(ast.Ident{
				name: 'int'
			})
		}
	}
	decl := ast.FnDecl{
		name:  'wrap'
		typ:   ast.FnType{
			generic_params: [
				ast.Expr(ast.Ident{
					name: 'T'
				}),
			]
			return_type:    ast.Expr(ast.Ident{
				name: 'int'
			})
		}
		stmts: [
			ast.Stmt(ast.ReturnStmt{
				exprs: [
					ast.Expr(ast.CallExpr{
						lhs: ast.Expr(ast.Ident{
							name: 'helper'
						})
					}),
				]
			}),
		]
	}
	files := [
		ast.File{
			mod:   'm'
			stmts: [ast.Stmt(helper_decl), ast.Stmt(decl)]
		},
		ast.File{
			mod: 'main'
		},
	]
	out := t.monomorphize_pass(files)
	assert out[0].stmts.len == 3
	assert out[1].stmts.len == 0
	assert out[0].stmts[2] is ast.FnDecl
	cloned := out[0].stmts[2] as ast.FnDecl
	assert cloned.name == 'm__wrap_T_string'
	ret := cloned.stmts[0] as ast.ReturnStmt
	call := ret.exprs[0] as ast.CallExpr
	assert call.lhs is ast.Ident
	assert (call.lhs as ast.Ident).name == 'helper'
}

fn test_register_generic_bindings_keeps_first_owner_for_duplicate_spec() {
	mut t := mono_test_transformer()
	t.env = types.Environment.new()
	bindings := {
		'T': types.Type(types.string_)
	}
	t.cur_generic_call_file_idx = 0
	t.register_generic_bindings('json2__Encoder__encode_value', bindings)
	t.cur_generic_call_file_idx = 1
	t.register_generic_bindings('json2__Encoder__encode_value', bindings)
	owner_key := generic_spec_owner_key('json2__Encoder__encode_value', bindings)
	owner := t.generic_spec_owner_file[owner_key] or { -1 }
	assert owner == 0
}

fn test_generic_call_concrete_return_type_prefers_substitution_over_stale_pos_type() {
	mut env := types.Environment.new()
	env.set_expr_type(77, types.Type(types.int_))
	mut t := mono_test_transformer()
	t.env = env
	t.generic_fn_decl_index['identity'] = ast.FnDecl{
		name: 'identity'
		typ:  ast.FnType{
			generic_params: [
				ast.Expr(ast.Ident{
					name: 'T'
				}),
			]
			params:         [
				ast.Parameter{
					name: 'x'
					typ:  ast.Expr(ast.Ident{
						name: 'T'
					})
				},
			]
			return_type:    ast.Expr(ast.Ident{
				name: 'T'
			})
		}
	}
	call := ast.Expr(ast.CallExpr{
		lhs:  ast.Expr(ast.Ident{
			name: 'identity'
		})
		args: [
			ast.Expr(ast.BasicLiteral{
				kind:  .number
				value: '0.0'
			}),
		]
		pos:  token.Pos{
			id: 77
		}
	})
	ret := t.get_expr_type(call) or { panic('missing generic call return type') }
	assert ret is types.Primitive
	assert (ret as types.Primitive).props.has(types.Properties.float)
}

fn test_generic_call_concrete_return_type_finds_current_module_bare_call() {
	mut t := mono_test_transformer()
	t.cur_module = 'math'
	t.generic_fn_decl_index['math__identity'] = ast.FnDecl{
		name: 'identity'
		typ:  ast.FnType{
			generic_params: [
				ast.Expr(ast.Ident{
					name: 'T'
				}),
			]
			params:         [
				ast.Parameter{
					name: 'x'
					typ:  ast.Expr(ast.Ident{
						name: 'T'
					})
				},
			]
			return_type:    ast.Expr(ast.Ident{
				name: 'T'
			})
		}
	}
	call := ast.Expr(ast.CallExpr{
		lhs:  ast.Expr(ast.Ident{
			name: 'identity'
		})
		args: [
			ast.Expr(ast.BasicLiteral{
				kind:  .number
				value: '0.0'
			}),
		]
	})
	ret := t.generic_call_concrete_return_type(call) or {
		panic('missing current-module generic return type')
	}
	assert ret is types.Primitive
	assert (ret as types.Primitive).props.has(types.Properties.float)
}

fn test_get_expr_type_prefers_local_decl_cache_over_stale_scope_type() {
	mut t := mono_test_transformer()
	mut scope := types.new_scope(unsafe { nil })
	scope.insert('ret', types.Type(types.int_))
	t.scope = scope
	t.local_decl_types['ret'] = types.Type(types.f64_)
	typ := t.get_expr_type(ast.Expr(ast.Ident{
		name: 'ret'
	})) or { panic('missing ident type') }
	assert typ.name() == 'f64'
}

fn test_clone_generic_type_name_selector_to_string_literal() {
	mut t := mono_test_transformer()
	expr := ast.Expr(ast.SelectorExpr{
		lhs: ast.Ident{
			name: 'T'
		}
		rhs: ast.Ident{
			name: 'name'
		}
		pos: token.Pos{
			id: 11
		}
	})
	out := t.clone_expr_with_bindings_and_fields(expr, {
		'T': types.Type(types.i64_)
	}, []CloneComptimeFieldCtx{})
	assert out is ast.StringLiteral
	assert (out as ast.StringLiteral).value == 'i64'
}

fn test_substitute_type_leaves_non_matching_named_type_alone() {
	bindings := {
		'T': types.Type(types.int_)
	}
	out := substitute_type(types.Type(types.NamedType('U')), bindings)
	assert out.name() == 'U'
}

// substitute_type: compound recursion

fn test_substitute_type_recurses_into_pointer_base() {
	bindings := {
		'T': types.Type(types.int_)
	}
	original := types.Type(types.Pointer{
		base_type: types.Type(types.NamedType('T'))
	})
	out := substitute_type(original, bindings)
	if out is types.Pointer {
		assert out.base_type.name() == 'int'
	} else {
		assert false, 'expected Pointer, got ${out.type_name()}'
	}
}

fn test_substitute_type_recurses_into_array_elem() {
	bindings := {
		'T': types.Type(types.int_)
	}
	original := types.Type(types.Array{
		elem_type: types.Type(types.NamedType('T'))
	})
	out := substitute_type(original, bindings)
	if out is types.Array {
		assert out.elem_type.name() == 'int'
	} else {
		assert false, 'expected Array, got ${out.type_name()}'
	}
}

fn test_substitute_type_recurses_into_map_key_and_value() {
	bindings := {
		'K': types.Type(types.string_)
		'V': types.Type(types.int_)
	}
	original := types.Type(types.Map{
		key_type:   types.Type(types.NamedType('K'))
		value_type: types.Type(types.NamedType('V'))
	})
	out := substitute_type(original, bindings)
	if out is types.Map {
		assert out.key_type.name() == 'string'
		assert out.value_type.name() == 'int'
	} else {
		assert false, 'expected Map, got ${out.type_name()}'
	}
}

fn test_substitute_type_recurses_into_option_base() {
	bindings := {
		'T': types.Type(types.int_)
	}
	original := types.Type(types.OptionType{
		base_type: types.Type(types.NamedType('T'))
	})
	out := substitute_type(original, bindings)
	if out is types.OptionType {
		assert out.base_type.name() == 'int'
	} else {
		assert false, 'expected OptionType, got ${out.type_name()}'
	}
}

fn test_substitute_type_recurses_into_nested_array_pointer() {
	bindings := {
		'T': types.Type(types.f64_)
	}
	original := types.Type(types.Array{
		elem_type: types.Type(types.Pointer{
			base_type: types.Type(types.NamedType('T'))
		})
	})
	out := substitute_type(original, bindings)
	if out is types.Array {
		elem := out.elem_type
		if elem is types.Pointer {
			assert elem.base_type.name() == 'f64'
		} else {
			assert false, 'expected nested Pointer, got ${elem.type_name()}'
		}
	} else {
		assert false, 'expected outer Array, got ${out.type_name()}'
	}
}

// substitute_type_in_expr: AST-level

fn test_substitute_type_in_expr_replaces_placeholder_ident() {
	mut t := mono_test_transformer()
	bindings := {
		'T': types.Type(types.int_)
	}
	expr := ast.Expr(ast.Ident{
		name: 'T'
	})
	out := t.substitute_type_in_expr(expr, bindings)
	if out is ast.Ident {
		assert out.name == 'int'
	} else {
		assert false, 'expected Ident, got ${out.type_name()}'
	}
}

fn test_substitute_type_in_expr_recurses_into_array_type() {
	mut t := mono_test_transformer()
	bindings := {
		'T': types.Type(types.int_)
	}
	expr := ast.Expr(ast.Type(ast.ArrayType{
		elem_type: ast.Expr(ast.Ident{
			name: 'T'
		})
	}))
	out := t.substitute_type_in_expr(expr, bindings)
	out_ty := out as ast.Type
	at := out_ty as ast.ArrayType
	elem := at.elem_type as ast.Ident
	assert elem.name == 'int'
}

fn test_substitute_type_in_expr_recurses_into_map_type() {
	mut t := mono_test_transformer()
	bindings := {
		'K': types.Type(types.string_)
		'V': types.Type(types.int_)
	}
	expr := ast.Expr(ast.Type(ast.MapType{
		key_type:   ast.Expr(ast.Ident{
			name: 'K'
		})
		value_type: ast.Expr(ast.Ident{
			name: 'V'
		})
	}))
	out := t.substitute_type_in_expr(expr, bindings)
	out_ty := out as ast.Type
	mt := out_ty as ast.MapType
	key := mt.key_type as ast.Ident
	val := mt.value_type as ast.Ident
	assert key.name == 'string'
	assert val.name == 'int'
}

fn test_substitute_type_in_expr_recurses_into_generic_index_type() {
	mut t := mono_test_transformer()
	bindings := {
		'T': types.Type(types.int_)
	}
	expr := ast.Expr(ast.GenericArgOrIndexExpr{
		lhs:  ast.Expr(ast.Ident{
			name: 'AtomicVal'
		})
		expr: ast.Expr(ast.Ident{
			name: 'T'
		})
	})
	out := t.substitute_type_in_expr(expr, bindings)
	gen := out as ast.GenericArgOrIndexExpr
	arg := gen.expr as ast.Ident
	assert arg.name == 'int'
}

// specialized_fn_name: matches cleanc's naming

fn test_specialized_fn_name_uses_all_placeholders_suffix_when_concrete_is_self() {
	t := mono_test_transformer()
	decl := ast.FnDecl{
		name: 'foo'
		typ:  ast.FnType{
			generic_params: [
				ast.Expr(ast.Ident{
					name: 'T'
				}),
			]
		}
	}
	bindings := {
		'T': types.Type(types.NamedType('T'))
	}
	assert t.specialized_fn_name(decl, bindings) == 'foo_T'
}

fn test_specialized_fn_name_uses_concrete_token_suffix() {
	t := mono_test_transformer()
	decl := ast.FnDecl{
		name: 'foo'
		typ:  ast.FnType{
			generic_params: [
				ast.Expr(ast.Ident{
					name: 'T'
				}),
			]
		}
	}
	bindings := {
		'T': types.Type(types.int_)
	}
	assert t.specialized_fn_name(decl, bindings) == 'foo_T_int'
}

fn test_specialized_fn_name_handles_multiple_params() {
	t := mono_test_transformer()
	decl := ast.FnDecl{
		name: 'pair'
		typ:  ast.FnType{
			generic_params: [
				ast.Expr(ast.Ident{
					name: 'K'
				}),
				ast.Expr(ast.Ident{
					name: 'V'
				}),
			]
		}
	}
	bindings := {
		'K': types.Type(types.string_)
		'V': types.Type(types.int_)
	}
	assert t.specialized_fn_name(decl, bindings) == 'pair_T_string_int'
}

fn test_clone_fn_decl_tracks_local_decl_types_for_interpolation_repair() {
	value_kind_type := types.Type(types.Enum{
		name: 'json2__ValueKind'
	})
	value_info_type := types.Type(types.Struct{
		name:   'json2__ValueInfo'
		fields: [
			types.Field{
				name: 'value_kind'
				typ:  value_kind_type
			},
		]
	})
	node_type := types.Type(types.Struct{
		name:   'json2__Node'
		fields: [
			types.Field{
				name: 'value'
				typ:  value_info_type
			},
		]
	})
	decoder_type := types.Type(types.Struct{
		name:   'json2__Decoder'
		fields: [
			types.Field{
				name: 'current_node'
				typ:  types.Type(types.Pointer{
					base_type: node_type
				})
			},
		]
	})
	mut t := mono_test_transformer()
	t.env = types.Environment.new()
	t.cur_module = 'json2'
	mut json2_scope := types.new_scope(unsafe { nil })
	json2_scope.insert_type('Decoder', decoder_type)
	json2_scope.insert_type('Node', node_type)
	json2_scope.insert_type('ValueInfo', value_info_type)
	t.cached_scopes['json2'] = json2_scope
	t.needed_str_fns = map[string]string{}
	t.needed_enum_str_fns = map[string]types.Enum{}
	value_kind_expr := ast.Expr(ast.SelectorExpr{
		lhs: ast.Expr(ast.Ident{
			name: 'string_info'
		})
		rhs: ast.Ident{
			name: 'value_kind'
		}
	})
	stale_arg := ast.Expr(ast.SelectorExpr{
		lhs: ast.Expr(ast.CallExpr{
			lhs:  ast.Expr(ast.Ident{
				name: 'string__str'
			})
			args: [value_kind_expr]
		})
		rhs: ast.Ident{
			name: 'str'
		}
	})
	decl := ast.FnDecl{
		name:      'decode_string'
		is_method: true
		receiver:  ast.Parameter{
			name: 'decoder'
			typ:  ast.Expr(ast.Ident{
				name: 'Decoder'
			})
		}
		typ:       ast.FnType{
			generic_params: [
				ast.Expr(ast.Ident{
					name: 'T'
				}),
			]
		}
		stmts:     [
			ast.Stmt(ast.AssignStmt{
				op:  .decl_assign
				lhs: [
					ast.Expr(ast.Ident{
						name: 'string_info'
					}),
				]
				rhs: [
					ast.Expr(ast.SelectorExpr{
						lhs: ast.Expr(ast.SelectorExpr{
							lhs: ast.Expr(ast.Ident{
								name: 'decoder'
							})
							rhs: ast.Ident{
								name: 'current_node'
							}
						})
						rhs: ast.Ident{
							name: 'value'
						}
					}),
				]
			}),
			ast.Stmt(ast.ExprStmt{
				expr: ast.Expr(ast.StringInterLiteral{
					values: [
						"'Expected string, but got ",
						"'",
					]
					inters: [
						ast.StringInter{
							expr: stale_arg
						},
					]
				})
			}),
		]
	}
	cloned := t.clone_fn_decl_with_substitutions(decl, {
		'T': types.Type(types.string_)
	}, 'decode_string_T_string', 'json2', 'main')
	_ = t.env.get_fn_scope('main', 'Decoder__decode_string_T_string') or {
		panic('missing target module cloned function scope')
	}
	expr_stmt := cloned.stmts[1] as ast.ExprStmt
	lit := expr_stmt.expr as ast.StringInterLiteral
	repaired := lit.inters[0].expr
	assert repaired is ast.SelectorExpr
	repaired_call := (repaired as ast.SelectorExpr).lhs
	assert repaired_call is ast.CallExpr
	repaired_lhs := (repaired_call as ast.CallExpr).lhs
	assert repaired_lhs is ast.Ident
	assert (repaired_lhs as ast.Ident).name == 'json2__ValueKind__str'
}

fn test_moved_clone_qualifies_source_module_type_positions() {
	mut t := mono_test_transformer()
	t.env = types.Environment.new()
	mut json2_scope := types.new_scope(unsafe { nil })
	json2_scope.insert_type('DecoderOptions', types.Type(types.Struct{
		name: 'json2__DecoderOptions'
	}))
	json2_scope.insert_type('Decoder', types.Type(types.Struct{
		name: 'json2__Decoder'
	}))
	json2_scope.insert_type('StructFieldInfo', types.Type(types.Struct{
		name: 'json2__StructFieldInfo'
	}))
	t.cached_scopes['json2'] = json2_scope
	decl := ast.FnDecl{
		name:  'decode'
		typ:   ast.FnType{
			generic_params: [
				ast.Expr(ast.Ident{
					name: 'T'
				}),
			]
			params:         [
				ast.Parameter{
					name: 'val'
					typ:  ast.Expr(ast.Ident{
						name: 'string'
					})
				},
				ast.Parameter{
					name: 'params'
					typ:  ast.Expr(ast.Ident{
						name: 'DecoderOptions'
					})
				},
			]
			return_type:    ast.Expr(ast.Type(ast.ResultType{
				base_type: ast.Expr(ast.Ident{
					name: 'T'
				})
			}))
		}
		stmts: [
			ast.Stmt(ast.AssignStmt{
				op:  .decl_assign
				lhs: [
					ast.Expr(ast.Ident{
						name: 'decoder'
					}),
				]
				rhs: [
					ast.Expr(ast.InitExpr{
						typ: ast.Expr(ast.Ident{
							name: 'Decoder'
						})
					}),
				]
			}),
			ast.Stmt(ast.ExprStmt{
				expr: ast.Expr(ast.ArrayInitExpr{
					typ: ast.Expr(ast.Type(ast.ArrayType{
						elem_type: ast.Expr(ast.Ident{
							name: 'StructFieldInfo'
						})
					}))
				})
			}),
		]
	}
	mut cloned := t.clone_fn_decl_with_substitutions(decl, {
		'T': types.Type(types.Struct{
			name: 'api__ApiBranchCount'
		})
	}, 'json2__decode_T_api_ApiBranchCount', 'json2', 'main')
	cloned = t.qualify_moved_clone_source_module_types(cloned, 'json2')
	options_type := cloned.typ.params[1].typ as ast.Ident
	assert options_type.name == 'json2__DecoderOptions'
	assign := cloned.stmts[0] as ast.AssignStmt
	init := assign.rhs[0] as ast.InitExpr
	init_type := init.typ as ast.Ident
	assert init_type.name == 'json2__Decoder'
	expr_stmt := cloned.stmts[1] as ast.ExprStmt
	array_init := expr_stmt.expr as ast.ArrayInitExpr
	array_type := array_init.typ as ast.Type
	array := array_type as ast.ArrayType
	elem := array.elem_type as ast.Ident
	assert elem.name == 'json2__StructFieldInfo'
}

fn test_moved_clone_qualifies_source_module_generic_function_value() {
	mut t := mono_test_transformer()
	handler_decl := ast.FnDecl{
		name: 'parallel_request_handler'
		typ:  ast.FnType{
			generic_params: [
				ast.Expr(ast.Ident{
					name: 'A'
				}),
				ast.Expr(ast.Ident{
					name: 'X'
				}),
			]
		}
	}
	t.generic_fn_decl_index['parallel_request_handler'] = handler_decl
	t.generic_fn_decl_index['json2__parallel_request_handler'] = handler_decl
	decl := ast.FnDecl{
		name:  'run_new'
		typ:   ast.FnType{
			generic_params: [
				ast.Expr(ast.Ident{
					name: 'A'
				}),
				ast.Expr(ast.Ident{
					name: 'X'
				}),
			]
		}
		stmts: [
			ast.Stmt(ast.ExprStmt{
				expr: ast.Expr(ast.InitExpr{
					typ:    ast.Expr(ast.Ident{
						name: 'ServerConfig'
					})
					fields: [
						ast.FieldInit{
							name:  'handler'
							value: ast.Expr(ast.GenericArgs{
								lhs:  ast.Expr(ast.Ident{
									name: 'parallel_request_handler'
								})
								args: [
									ast.Expr(ast.Ident{
										name: 'A'
									}),
									ast.Expr(ast.Ident{
										name: 'X'
									}),
								]
							})
						},
					]
				})
			}),
		]
	}
	mut cloned := t.clone_fn_decl_with_substitutions(decl, {
		'A': types.Type(types.Struct{
			name: 'main__App'
		})
		'X': types.Type(types.Struct{
			name: 'main__Context'
		})
	}, 'json2__run_new_T_App_Context', 'json2', 'main')
	cloned = t.qualify_moved_clone_source_module_types(cloned, 'json2')
	expr_stmt := cloned.stmts[0] as ast.ExprStmt
	init := expr_stmt.expr as ast.InitExpr
	handler := init.fields[0].value as ast.Ident
	assert handler.name == 'json2__parallel_request_handler_T_main_App_main_Context'
}

fn test_moved_clone_qualifies_source_module_generic_function_value_index_expr() {
	mut t := mono_test_transformer()
	handler_decl := ast.FnDecl{
		name: 'parallel_request_handler'
		typ:  ast.FnType{
			generic_params: [
				ast.Expr(ast.Ident{
					name: 'A'
				}),
				ast.Expr(ast.Ident{
					name: 'X'
				}),
			]
		}
	}
	t.generic_fn_decl_index['parallel_request_handler'] = handler_decl
	t.generic_fn_decl_index['json2__parallel_request_handler'] = handler_decl
	decl := ast.FnDecl{
		name:  'run_new'
		typ:   ast.FnType{
			generic_params: [
				ast.Expr(ast.Ident{
					name: 'A'
				}),
				ast.Expr(ast.Ident{
					name: 'X'
				}),
			]
		}
		stmts: [
			ast.Stmt(ast.ExprStmt{
				expr: ast.Expr(ast.InitExpr{
					typ:    ast.Expr(ast.Ident{
						name: 'ServerConfig'
					})
					fields: [
						ast.FieldInit{
							name:  'handler'
							value: ast.Expr(ast.IndexExpr{
								lhs:  ast.Expr(ast.GenericArgOrIndexExpr{
									lhs:  ast.Expr(ast.Ident{
										name: 'parallel_request_handler'
									})
									expr: ast.Expr(ast.Ident{
										name: 'A'
									})
								})
								expr: ast.Expr(ast.Ident{
									name: 'X'
								})
							})
						},
					]
				})
			}),
		]
	}
	mut cloned := t.clone_fn_decl_with_substitutions(decl, {
		'A': types.Type(types.Struct{
			name: 'main__App'
		})
		'X': types.Type(types.Struct{
			name: 'main__Context'
		})
	}, 'json2__run_new_T_App_Context', 'json2', 'main')
	cloned = t.qualify_moved_clone_source_module_types(cloned, 'json2')
	expr_stmt := cloned.stmts[0] as ast.ExprStmt
	init := expr_stmt.expr as ast.InitExpr
	handler := init.fields[0].value as ast.Ident
	assert handler.name == 'json2__parallel_request_handler_T_main_App_main_Context'
}

fn test_collect_generic_struct_field_type_resolves_prefix_pointer_generic_arg() {
	value_kind_type := types.Type(types.Enum{
		name: 'json2__ValueKind'
	})
	value_info_type := types.Type(types.Struct{
		name:   'json2__ValueInfo'
		fields: [
			types.Field{
				name: 'value_kind'
				typ:  value_kind_type
			},
		]
	})
	node_type := types.Type(types.Struct{
		name:           'json2__Node'
		generic_params: ['T']
		fields:         [
			types.Field{
				name: 'value'
				typ:  types.Type(types.NamedType('T'))
			},
		]
	})
	mut t := mono_test_transformer()
	t.env = types.Environment.new()
	t.cur_module = 'json2'
	mut json2_scope := types.new_scope(unsafe { nil })
	json2_scope.insert_type('Node', node_type)
	json2_scope.insert_type('ValueInfo', value_info_type)
	t.cached_scopes['json2'] = json2_scope
	t.collect_struct_decl_generic_field_types(ast.StructDecl{
		name:   'Decoder'
		fields: [
			ast.FieldDecl{
				name: 'current_node'
				typ:  ast.Expr(ast.PrefixExpr{
					op:   .amp
					expr: ast.Expr(ast.GenericArgOrIndexExpr{
						lhs:  ast.Expr(ast.Ident{
							name: 'Node'
						})
						expr: ast.Expr(ast.Ident{
							name: 'ValueInfo'
						})
					})
				})
			},
		]
	}, 'json2')
	field_typ := t.lookup_struct_field_generic_decl_type('json2__Decoder', 'current_node') or {
		panic('missing concrete current_node type')
	}
	assert field_typ is types.Pointer
	node_concrete := (field_typ as types.Pointer).base_type
	assert node_concrete is types.Struct
	value_field := struct_field_by_name(node_concrete as types.Struct, 'value') or {
		panic('missing Node.value field')
	}
	assert value_field.typ.name() == 'json2__ValueInfo'
}

// clone_fn_decl_with_substitutions: end-to-end on a trivial generic fn

fn test_clone_fn_decl_renames_and_substitutes_param_and_return_types() {
	mut t := mono_test_transformer()
	bindings := {
		'T': types.Type(types.int_)
	}
	body_pos := token.Pos{
		id: 1
	}
	decl := ast.FnDecl{
		name:  'identity'
		typ:   ast.FnType{
			generic_params: [
				ast.Expr(ast.Ident{
					name: 'T'
				}),
			]
			params:         [
				ast.Parameter{
					name: 'x'
					typ:  ast.Expr(ast.Ident{
						name: 'T'
					})
				},
			]
			return_type:    ast.Expr(ast.Ident{
				name: 'T'
			})
		}
		stmts: [
			ast.Stmt(ast.ReturnStmt{
				exprs: [
					ast.Expr(ast.Ident{
						name: 'x'
						pos:  body_pos
					}),
				]
			}),
		]
	}
	cloned := t.clone_fn_decl_with_substitutions(decl, bindings, 'identity_T_int', '', '')
	assert cloned.name == 'identity_T_int'
	assert cloned.typ.generic_params.len == 0
	assert cloned.typ.params.len == 1
	param_typ := cloned.typ.params[0].typ as ast.Ident
	assert param_typ.name == 'int'
	ret_typ := cloned.typ.return_type as ast.Ident
	assert ret_typ.name == 'int'
	// Body is preserved structurally (x is a value identifier, not a type — stays 'x')
	assert cloned.stmts.len == 1
	if cloned.stmts[0] is ast.ReturnStmt {
		ret_stmt := cloned.stmts[0] as ast.ReturnStmt
		ret_ident := ret_stmt.exprs[0] as ast.Ident
		assert ret_ident.name == 'x'
	} else {
		assert false, 'expected ReturnStmt'
	}
}

fn test_clone_fn_decl_substitutes_inside_array_param_type() {
	mut t := mono_test_transformer()
	bindings := {
		'T': types.Type(types.int_)
	}
	decl := ast.FnDecl{
		name: 'first'
		typ:  ast.FnType{
			generic_params: [
				ast.Expr(ast.Ident{
					name: 'T'
				}),
			]
			params:         [
				ast.Parameter{
					name: 'arr'
					typ:  ast.Expr(ast.Type(ast.ArrayType{
						elem_type: ast.Expr(ast.Ident{
							name: 'T'
						})
					}))
				},
			]
			return_type:    ast.Expr(ast.Ident{
				name: 'T'
			})
		}
	}
	cloned := t.clone_fn_decl_with_substitutions(decl, bindings, 'first_T_int', '', '')
	assert cloned.name == 'first_T_int'
	param_typ := cloned.typ.params[0].typ as ast.Type
	arr_typ := param_typ as ast.ArrayType
	elem := arr_typ.elem_type as ast.Ident
	assert elem.name == 'int'
}

fn test_clone_fn_decl_registers_array_index_decl_type_in_clone_scope() {
	mut t := mono_test_transformer()
	value_pos := token.Pos{
		id: 801
	}
	index_pos := token.Pos{
		id: 802
	}
	first_lhs_pos := token.Pos{
		id: 803
	}
	t.env.set_expr_type(value_pos.id, types.Type(types.Array{
		elem_type: types.Type(types.f64_)
	}))
	t.env.set_expr_type(index_pos.id, types.Type(types.f64_))
	decl := ast.FnDecl{
		name:  'first'
		typ:   ast.FnType{
			generic_params: [
				ast.Expr(ast.Ident{
					name: 'T'
				}),
			]
			params:         [
				ast.Parameter{
					name: 'value'
					typ:  ast.Expr(ast.Type(ast.ArrayType{
						elem_type: ast.Expr(ast.Ident{
							name: 'T'
						})
					}))
				},
			]
		}
		stmts: [
			ast.Stmt(ast.AssignStmt{
				op:  .decl_assign
				lhs: [
					ast.Expr(ast.Ident{
						name: 'first'
						pos:  first_lhs_pos
					}),
				]
				rhs: [
					ast.Expr(ast.IndexExpr{
						lhs:  ast.Expr(ast.Ident{
							name: 'value'
							pos:  value_pos
						})
						expr: ast.Expr(ast.BasicLiteral{
							kind:  .number
							value: '0'
						})
						pos:  index_pos
					}),
				]
			}),
		]
	}
	cloned := t.clone_fn_decl_with_substitutions(decl, {
		'T': types.Type(types.string_)
	}, 'first_T_string', 'orm', 'main')
	scope := t.env.get_fn_scope('orm', 'first_T_string') or {
		panic('missing cloned function scope')
	}
	first_type := scope.lookup_var_type('first') or { panic('missing first type') }
	assert first_type is types.String
	stmt := cloned.stmts[0] as ast.AssignStmt
	lhs := stmt.lhs[0] as ast.Ident
	lhs_type := t.synth_types[lhs.pos.id] or { panic('missing first synth type') }
	assert lhs_type is types.String
}

fn test_clone_fn_decl_registers_nested_array_index_decl_type_in_clone_scope() {
	mut t := mono_test_transformer()
	value_pos := token.Pos{
		id: 821
	}
	index_pos := token.Pos{
		id: 822
	}
	item_lhs_pos := token.Pos{
		id: 823
	}
	t.env.set_expr_type(value_pos.id, types.Type(types.Array{
		elem_type: types.Type(types.f64_)
	}))
	t.env.set_expr_type(index_pos.id, types.Type(types.f64_))
	decl := ast.FnDecl{
		name:  'first_nested'
		typ:   ast.FnType{
			generic_params: [
				ast.Expr(ast.Ident{
					name: 'T'
				}),
			]
			params:         [
				ast.Parameter{
					name: 'value'
					typ:  ast.Expr(ast.Type(ast.ArrayType{
						elem_type: ast.Expr(ast.Ident{
							name: 'T'
						})
					}))
				},
			]
		}
		stmts: [
			ast.Stmt(ast.ForStmt{
				stmts: [
					ast.Stmt(ast.AssignStmt{
						op:  .decl_assign
						lhs: [
							ast.Expr(ast.Ident{
								name: 'item'
								pos:  item_lhs_pos
							}),
						]
						rhs: [
							ast.Expr(ast.IndexExpr{
								lhs:  ast.Expr(ast.Ident{
									name: 'value'
									pos:  value_pos
								})
								expr: ast.Expr(ast.BasicLiteral{
									kind:  .number
									value: '0'
								})
								pos:  index_pos
							}),
						]
					}),
				]
			}),
		]
	}
	cloned := t.clone_fn_decl_with_substitutions(decl, {
		'T': types.Type(types.string_)
	}, 'first_nested_T_string', 'orm', 'main')
	scope := t.env.get_fn_scope('orm', 'first_nested_T_string') or {
		panic('missing cloned function scope')
	}
	item_type := scope.lookup_var_type('item') or { panic('missing item type') }
	assert item_type is types.String
	for_stmt := cloned.stmts[0] as ast.ForStmt
	stmt := for_stmt.stmts[0] as ast.AssignStmt
	lhs := stmt.lhs[0] as ast.Ident
	lhs_type := t.synth_types[lhs.pos.id] or { panic('missing item synth type') }
	assert lhs_type is types.String
}

fn test_clone_fn_decl_preserves_unrelated_stmts() {
	mut t := mono_test_transformer()
	bindings := {
		'T': types.Type(types.int_)
	}
	decl := ast.FnDecl{
		name:  'noop'
		typ:   ast.FnType{
			generic_params: [
				ast.Expr(ast.Ident{
					name: 'T'
				}),
			]
		}
		stmts: [
			ast.Stmt(ast.ExprStmt{
				expr: ast.Expr(ast.BasicLiteral{
					kind:  .number
					value: '42'
				})
			}),
		]
	}
	cloned := t.clone_fn_decl_with_substitutions(decl, bindings, 'noop_T_int', '', '')
	assert cloned.stmts.len == 1
	es := cloned.stmts[0] as ast.ExprStmt
	lit := es.expr as ast.BasicLiteral
	assert lit.value == '42'
}
