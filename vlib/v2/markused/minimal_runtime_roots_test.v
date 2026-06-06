module markused

import v2.ast
import v2.token
import v2.types

fn minimal_pos(id int) token.Pos {
	return token.Pos{
		offset: id
		id:     id
	}
}

fn minimal_ident(name string, id int) ast.Ident {
	return ast.Ident{
		name: name
		pos:  minimal_pos(id)
	}
}

fn minimal_selector(lhs string, rhs string, id int) ast.SelectorExpr {
	return ast.SelectorExpr{
		lhs: minimal_ident(lhs, id)
		rhs: minimal_ident(rhs, id + 1)
		pos: minimal_pos(id)
	}
}

fn minimal_c_selector(name string, id int) ast.SelectorExpr {
	return minimal_selector('C', name, id)
}

fn minimal_c_call(name string, id int, args []ast.Expr) ast.CallExpr {
	return ast.CallExpr{
		lhs:  minimal_c_selector(name, id)
		args: args
		pos:  minimal_pos(id)
	}
}

fn mark_used_flat_minimal(files []ast.File, env &types.Environment) map[string]bool {
	flat := ast.flatten_files(files)
	return mark_used_flat_with_options(&flat, env, MarkUsedOptions{
		minimal_runtime_roots: true
	})
}

fn local_call_minimal_files() []ast.File {
	return [
		ast.File{
			mod:   'main'
			name:  'local_calls.v'
			stmts: [
				ast.Stmt(ast.FnDecl{
					name:  'main'
					typ:   ast.FnType{}
					pos:   minimal_pos(10)
					stmts: [
						ast.Stmt(ast.ExprStmt{
							expr: ast.CallExpr{
								lhs: ast.Expr(minimal_ident('foo', 11))
								pos: minimal_pos(11)
							}
						}),
					]
				}),
				ast.Stmt(ast.FnDecl{
					name:  'foo'
					typ:   ast.FnType{}
					pos:   minimal_pos(12)
					stmts: [
						ast.Stmt(ast.ExprStmt{
							expr: ast.CallExpr{
								lhs: ast.Expr(minimal_ident('status', 13))
								pos: minimal_pos(13)
							}
						}),
					]
				}),
				ast.Stmt(ast.FnDecl{
					name: 'status'
					typ:  ast.FnType{}
					pos:  minimal_pos(14)
				}),
				ast.Stmt(ast.FnDecl{
					name: 'dead'
					typ:  ast.FnType{}
					pos:  minimal_pos(15)
				}),
			]
		},
	]
}

fn assert_local_call_minimal_used(used map[string]bool, files []ast.File, env &types.Environment, label string) {
	main_key := decl_key('main', files[0].stmts[0] as ast.FnDecl, env)
	foo_key := decl_key('main', files[0].stmts[1] as ast.FnDecl, env)
	status_key := decl_key('main', files[0].stmts[2] as ast.FnDecl, env)
	dead_key := decl_key('main', files[0].stmts[3] as ast.FnDecl, env)

	assert used[main_key], '${label}: main root was not kept'
	assert used[foo_key], '${label}: local foo call was not marked'
	assert used[status_key], '${label}: transitive local status call was not marked'
	assert !used[dead_key], '${label}: unused local function should stay pruned'
}

fn test_minimal_runtime_roots_pointer_guards_accept_low_canonical_pointers() {
	assert sumtype_payload_word_is_valid(1, 0x10000)
	assert !sumtype_payload_word_is_valid(1, 0)
	assert !sumtype_payload_word_is_valid(1, 3)
	assert string_ok('foo')
	assert string_ok('status')
}

fn test_minimal_runtime_roots_keep_local_calls_legacy() {
	mut env := types.Environment.new()
	files := local_call_minimal_files()
	used := mark_used_with_options(files, env, MarkUsedOptions{
		minimal_runtime_roots: true
	})
	assert_local_call_minimal_used(used, files, env, 'legacy')
}

fn test_minimal_runtime_roots_keep_local_calls_flat() {
	mut env := types.Environment.new()
	files := local_call_minimal_files()
	used := mark_used_flat_minimal(files, env)
	assert_local_call_minimal_used(used, files, env, 'flat')
}

fn selective_import_minimal_files() []ast.File {
	return [
		ast.File{
			mod:     'main'
			name:    'examples/submodule/main.v'
			imports: [
				ast.ImportStmt{
					name:    'mymodules'
					alias:   'mymodules'
					symbols: [ast.Expr(minimal_ident('add_xy', 60))]
				},
				ast.ImportStmt{
					name:    'mymodules.submodule'
					alias:   'submodule'
					symbols: [ast.Expr(minimal_ident('sub_xy', 61))]
				},
			]
			stmts:   [
				ast.Stmt(ast.FnDecl{
					name:  'main'
					typ:   ast.FnType{}
					pos:   minimal_pos(62)
					stmts: [
						ast.Stmt(ast.ExprStmt{
							expr: ast.CallExpr{
								lhs: ast.Expr(minimal_ident('add_xy', 63))
								pos: minimal_pos(63)
							}
						}),
						ast.Stmt(ast.ExprStmt{
							expr: ast.CallExpr{
								lhs: ast.Expr(minimal_ident('sub_xy', 64))
								pos: minimal_pos(64)
							}
						}),
					]
				}),
			]
		},
		ast.File{
			mod:   'mymodules'
			name:  'examples/submodule/mymodules/main_functions.v'
			stmts: [
				ast.Stmt(ast.FnDecl{
					name: 'add_xy'
					typ:  ast.FnType{}
					pos:  minimal_pos(65)
				}),
				ast.Stmt(ast.FnDecl{
					name: 'unused_add'
					typ:  ast.FnType{}
					pos:  minimal_pos(66)
				}),
			]
		},
		ast.File{
			mod:   'submodule'
			name:  'examples/submodule/mymodules/submodule/sub_functions.v'
			stmts: [
				ast.Stmt(ast.FnDecl{
					name: 'sub_xy'
					typ:  ast.FnType{}
					pos:  minimal_pos(67)
				}),
				ast.Stmt(ast.FnDecl{
					name: 'unused_sub'
					typ:  ast.FnType{}
					pos:  minimal_pos(68)
				}),
			]
		},
	]
}

fn assert_selective_import_minimal_used(used map[string]bool, files []ast.File, env &types.Environment, label string) {
	main_key := decl_key('main', files[0].stmts[0] as ast.FnDecl, env)
	add_key := decl_key('mymodules', files[1].stmts[0] as ast.FnDecl, env)
	unused_add_key := decl_key('mymodules', files[1].stmts[1] as ast.FnDecl, env)
	sub_key := decl_key('submodule', files[2].stmts[0] as ast.FnDecl, env)
	unused_sub_key := decl_key('submodule', files[2].stmts[1] as ast.FnDecl, env)

	assert used[main_key], '${label}: main root was not kept'
	assert used[add_key], '${label}: selective import add_xy was not resolved to mymodules__add_xy'
	assert used[sub_key], '${label}: selective import sub_xy was not resolved to submodule__sub_xy'
	assert !used[unused_add_key], '${label}: unused mymodules function should stay pruned'
	assert !used[unused_sub_key], '${label}: unused submodule function should stay pruned'
}

fn test_minimal_runtime_roots_keep_selective_imported_function_calls_legacy() {
	mut env := types.Environment.new()
	files := selective_import_minimal_files()
	used := mark_used_with_options(files, env, MarkUsedOptions{
		minimal_runtime_roots: true
	})
	assert_selective_import_minimal_used(used, files, env, 'legacy')
}

fn test_minimal_runtime_roots_keep_selective_imported_function_calls_flat() {
	mut env := types.Environment.new()
	files := selective_import_minimal_files()
	used := mark_used_flat_minimal(files, env)
	assert_selective_import_minimal_used(used, files, env, 'flat')
}

fn selective_import_shadow_minimal_files(use_function_value bool) []ast.File {
	mut main_body := []ast.Stmt{}
	if use_function_value {
		main_body << ast.Stmt(ast.AssignStmt{
			op:  .decl_assign
			lhs: [ast.Expr(minimal_ident('f', 150))]
			rhs: [ast.Expr(minimal_ident('add_xy', 151))]
			pos: minimal_pos(150)
		})
	} else {
		main_body << ast.Stmt(ast.ExprStmt{
			expr: ast.CallExpr{
				lhs: ast.Expr(minimal_ident('add_xy', 152))
				pos: minimal_pos(152)
			}
		})
	}
	return [
		ast.File{
			mod:     'main'
			name:    'shadow/main.v'
			imports: [
				ast.ImportStmt{
					name:    'mymodules'
					alias:   'mymodules'
					symbols: [ast.Expr(minimal_ident('add_xy', 153))]
				},
			]
			stmts:   [
				ast.Stmt(ast.FnDecl{
					name:  'main'
					typ:   ast.FnType{}
					pos:   minimal_pos(154)
					stmts: main_body
				}),
				ast.Stmt(ast.FnDecl{
					name: 'add_xy'
					typ:  ast.FnType{}
					pos:  minimal_pos(155)
				}),
			]
		},
		ast.File{
			mod:   'mymodules'
			name:  'shadow/mymodules/main_functions.v'
			stmts: [
				ast.Stmt(ast.FnDecl{
					name: 'add_xy'
					typ:  ast.FnType{}
					pos:  minimal_pos(156)
				}),
				ast.Stmt(ast.FnDecl{
					name: 'unused_remote'
					typ:  ast.FnType{}
					pos:  minimal_pos(157)
				}),
			]
		},
		ast.File{
			mod:   'othermod'
			name:  'shadow/othermod/main_functions.v'
			stmts: [
				ast.Stmt(ast.FnDecl{
					name: 'add_xy'
					typ:  ast.FnType{}
					pos:  minimal_pos(158)
				}),
			]
		},
	]
}

fn assert_selective_import_shadow_uses_local(used map[string]bool, files []ast.File, env &types.Environment, label string) {
	main_key := decl_key('main', files[0].stmts[0] as ast.FnDecl, env)
	local_add_key := decl_key('main', files[0].stmts[1] as ast.FnDecl, env)
	imported_add_key := decl_key('mymodules', files[1].stmts[0] as ast.FnDecl, env)
	unused_remote_key := decl_key('mymodules', files[1].stmts[1] as ast.FnDecl, env)
	other_add_key := decl_key('othermod', files[2].stmts[0] as ast.FnDecl, env)

	assert used[main_key], '${label}: main root was not kept'
	assert used[local_add_key], '${label}: local add_xy should shadow the selective import'
	assert !used[imported_add_key], '${label}: shadowed selective import should stay pruned'
	assert !used[unused_remote_key], '${label}: unused imported-module function should stay pruned'
	assert !used[other_add_key], '${label}: bare global add_xy fallback marked a colliding module function'
}

fn test_minimal_runtime_roots_selective_import_call_uses_local_shadow_legacy() {
	mut env := types.Environment.new()
	files := selective_import_shadow_minimal_files(false)
	used := mark_used_with_options(files, env, MarkUsedOptions{
		minimal_runtime_roots: true
	})
	assert_selective_import_shadow_uses_local(used, files, env, 'legacy call')
}

fn test_minimal_runtime_roots_selective_import_call_uses_local_shadow_flat() {
	mut env := types.Environment.new()
	files := selective_import_shadow_minimal_files(false)
	used := mark_used_flat_minimal(files, env)
	assert_selective_import_shadow_uses_local(used, files, env, 'flat call')
}

fn test_minimal_runtime_roots_selective_import_function_value_uses_local_shadow_legacy() {
	mut env := types.Environment.new()
	files := selective_import_shadow_minimal_files(true)
	used := mark_used_with_options(files, env, MarkUsedOptions{
		minimal_runtime_roots: true
	})
	assert_selective_import_shadow_uses_local(used, files, env, 'legacy function value')
}

fn test_minimal_runtime_roots_selective_import_function_value_uses_local_shadow_flat() {
	mut env := types.Environment.new()
	files := selective_import_shadow_minimal_files(true)
	used := mark_used_flat_minimal(files, env)
	assert_selective_import_shadow_uses_local(used, files, env, 'flat function value')
}

fn selective_import_function_value_minimal_files() []ast.File {
	return [
		ast.File{
			mod:     'main'
			name:    'fnvalue/main.v'
			imports: [
				ast.ImportStmt{
					name:    'mymodules'
					alias:   'mymodules'
					symbols: [ast.Expr(minimal_ident('add_xy', 160))]
				},
			]
			stmts:   [
				ast.Stmt(ast.FnDecl{
					name:  'main'
					typ:   ast.FnType{}
					pos:   minimal_pos(161)
					stmts: [
						ast.Stmt(ast.AssignStmt{
							op:  .decl_assign
							lhs: [ast.Expr(minimal_ident('f', 162))]
							rhs: [ast.Expr(minimal_ident('add_xy', 163))]
							pos: minimal_pos(162)
						}),
					]
				}),
			]
		},
		ast.File{
			mod:   'mymodules'
			name:  'fnvalue/mymodules/main_functions.v'
			stmts: [
				ast.Stmt(ast.FnDecl{
					name: 'add_xy'
					typ:  ast.FnType{}
					pos:  minimal_pos(164)
				}),
				ast.Stmt(ast.FnDecl{
					name: 'unused_remote'
					typ:  ast.FnType{}
					pos:  minimal_pos(165)
				}),
			]
		},
		ast.File{
			mod:   'othermod'
			name:  'fnvalue/othermod/main_functions.v'
			stmts: [
				ast.Stmt(ast.FnDecl{
					name: 'add_xy'
					typ:  ast.FnType{}
					pos:  minimal_pos(166)
				}),
			]
		},
	]
}

fn assert_selective_import_function_value_uses_imported_target(used map[string]bool, files []ast.File, env &types.Environment, label string) {
	main_key := decl_key('main', files[0].stmts[0] as ast.FnDecl, env)
	imported_add_key := decl_key('mymodules', files[1].stmts[0] as ast.FnDecl, env)
	unused_remote_key := decl_key('mymodules', files[1].stmts[1] as ast.FnDecl, env)
	other_add_key := decl_key('othermod', files[2].stmts[0] as ast.FnDecl, env)

	assert used[main_key], '${label}: main root was not kept'
	assert used[imported_add_key], '${label}: selective import function value did not resolve to mymodules__add_xy'
	assert !used[unused_remote_key], '${label}: unused imported-module function should stay pruned'
	assert !used[other_add_key], '${label}: bare global add_xy fallback marked a colliding module function'
}

fn test_minimal_runtime_roots_keep_selective_imported_function_values_legacy() {
	mut env := types.Environment.new()
	files := selective_import_function_value_minimal_files()
	used := mark_used_with_options(files, env, MarkUsedOptions{
		minimal_runtime_roots: true
	})
	assert_selective_import_function_value_uses_imported_target(used, files, env,
		'legacy function value')
}

fn test_minimal_runtime_roots_keep_selective_imported_function_values_flat() {
	mut env := types.Environment.new()
	files := selective_import_function_value_minimal_files()
	used := mark_used_flat_minimal(files, env)
	assert_selective_import_function_value_uses_imported_target(used, files, env,
		'flat function value')
}

fn test_minimal_runtime_roots_selective_import_add_fn_name_indices_legacy() {
	mut env := types.Environment.new()
	files := selective_import_function_value_minimal_files()
	mut w := new_walker(files, env, MarkUsedOptions{
		minimal_runtime_roots: true
	})
	w.collect_defs()
	w.cur_fn_file = files[0].name
	mut indices := []int{}
	w.add_fn_name_indices('add_xy', 'main', mut indices)
	target_key := decl_key('mymodules', files[1].stmts[0] as ast.FnDecl, env)

	assert indices.len == 1
	assert w.fns[indices[0]].key == target_key
}

fn test_minimal_runtime_roots_selective_import_add_fn_name_indices_flat() {
	mut env := types.Environment.new()
	files := selective_import_function_value_minimal_files()
	flat := ast.flatten_files(files)
	mut w := new_walker(files, env, MarkUsedOptions{
		minimal_runtime_roots: true
	})
	w.collect_defs_from_flat(&flat)
	w.cur_fn_file = files[0].name
	mut indices := []int{}
	w.add_fn_name_indices('add_xy', 'main', mut indices)
	target_key := decl_key('mymodules', files[1].stmts[0] as ast.FnDecl, env)

	assert indices.len == 1
	assert w.fns[indices[0]].key == target_key
}

fn test_minimal_runtime_roots_keep_explicit_calls_only() {
	mut env := types.Environment.new()
	files := [
		ast.File{
			mod:   'main'
			name:  'main.v'
			stmts: [
				ast.Stmt(ast.FnDecl{
					name:  'main'
					typ:   ast.FnType{}
					pos:   minimal_pos(120)
					stmts: [
						ast.Stmt(ast.ExprStmt{
							expr: ast.CallExpr{
								lhs: ast.Ident{
									name: 'println'
									pos:  minimal_pos(121)
								}
								pos: minimal_pos(121)
							}
						}),
					]
				}),
			]
		},
		ast.File{
			mod:   'builtin'
			name:  'vlib/builtin/builtin.v'
			stmts: [
				ast.Stmt(ast.FnDecl{
					name:  'println'
					typ:   ast.FnType{}
					pos:   minimal_pos(122)
					stmts: [
						ast.Stmt(ast.ExprStmt{
							expr: ast.CallExpr{
								lhs: ast.Ident{
									name: 'write_stdout'
									pos:  minimal_pos(123)
								}
								pos: minimal_pos(123)
							}
						}),
					]
				}),
				ast.Stmt(ast.FnDecl{
					name: 'write_stdout'
					typ:  ast.FnType{}
					pos:  minimal_pos(124)
				}),
				ast.Stmt(ast.FnDecl{
					name: 'print_backtrace'
					typ:  ast.FnType{}
					pos:  minimal_pos(125)
				}),
				ast.Stmt(ast.FnDecl{
					name: 'builtin_keep_helper'
					typ:  ast.FnType{}
					pos:  minimal_pos(126)
				}),
			]
		},
		ast.File{
			mod:   'time'
			name:  'time.v'
			stmts: [
				ast.Stmt(ast.StructDecl{
					name: 'Time'
				}),
				ast.Stmt(ast.FnDecl{
					is_method: true
					receiver:  ast.Parameter{
						name: 't'
						typ:  ast.Ident{
							name: 'Time'
							pos:  minimal_pos(127)
						}
						pos:  minimal_pos(127)
					}
					name:      'str'
					typ:       ast.FnType{}
					pos:       minimal_pos(128)
				}),
			]
		},
		ast.File{
			mod:   'dep'
			name:  'dep.v'
			stmts: [
				ast.Stmt(ast.FnDecl{
					name: 'init'
					typ:  ast.FnType{}
					pos:  minimal_pos(129)
				}),
			]
		},
	]
	used := mark_used_with_options(files, env, MarkUsedOptions{
		minimal_runtime_roots: true
	})
	main_key := decl_key('main', files[0].stmts[0] as ast.FnDecl, env)
	println_key := decl_key('builtin', files[1].stmts[0] as ast.FnDecl, env)
	write_stdout_key := decl_key('builtin', files[1].stmts[1] as ast.FnDecl, env)
	print_backtrace_key := decl_key('builtin', files[1].stmts[2] as ast.FnDecl, env)
	builtin_keep_key := decl_key('builtin', files[1].stmts[3] as ast.FnDecl, env)
	time_str_key := decl_key('time', files[2].stmts[1] as ast.FnDecl, env)
	dep_init_key := decl_key('dep', files[3].stmts[0] as ast.FnDecl, env)

	assert used[main_key]
	assert used[println_key]
	assert used[write_stdout_key]
	assert !used[print_backtrace_key]
	assert !used[builtin_keep_key]
	assert !used[time_str_key]
	assert !used[dep_init_key]
}

fn test_minimal_runtime_roots_keep_functions_used_as_values() {
	mut env := types.Environment.new()
	files := [
		ast.File{
			mod:   'main'
			name:  'main.v'
			stmts: [
				ast.Stmt(ast.FnDecl{
					name:  'main'
					typ:   ast.FnType{}
					pos:   minimal_pos(180)
					stmts: [
						ast.Stmt(ast.AssignStmt{
							op:  .decl_assign
							lhs: [ast.Expr(minimal_ident('f', 181))]
							rhs: [ast.Expr(minimal_ident('assigned_cleanup', 182))]
							pos: minimal_pos(181)
						}),
						ast.Stmt(ast.ExprStmt{
							expr: ast.CallExpr{
								lhs:  minimal_ident('run', 183)
								args: [ast.Expr(minimal_ident('argument_cleanup', 184))]
								pos:  minimal_pos(183)
							}
						}),
						ast.Stmt(ast.AssignStmt{
							op:  .decl_assign
							lhs: [ast.Expr(minimal_ident('g', 185))]
							rhs: [
								ast.Expr(ast.CallExpr{
									lhs: minimal_ident('choose_cleanup', 186)
									pos: minimal_pos(186)
								}),
							]
							pos: minimal_pos(185)
						}),
					]
				}),
				ast.Stmt(ast.FnDecl{
					name: 'run'
					typ:  ast.FnType{}
					pos:  minimal_pos(187)
				}),
				ast.Stmt(ast.FnDecl{
					name:  'choose_cleanup'
					typ:   ast.FnType{}
					pos:   minimal_pos(188)
					stmts: [
						ast.Stmt(ast.ReturnStmt{
							exprs: [
								ast.Expr(minimal_ident('returned_cleanup', 189)),
							]
						}),
					]
				}),
				ast.Stmt(ast.FnDecl{
					name: 'assigned_cleanup'
					typ:  ast.FnType{}
					pos:  minimal_pos(190)
				}),
				ast.Stmt(ast.FnDecl{
					name: 'argument_cleanup'
					typ:  ast.FnType{}
					pos:  minimal_pos(191)
				}),
				ast.Stmt(ast.FnDecl{
					name: 'returned_cleanup'
					typ:  ast.FnType{}
					pos:  minimal_pos(192)
				}),
				ast.Stmt(ast.FnDecl{
					name: 'unused_cleanup'
					typ:  ast.FnType{}
					pos:  minimal_pos(193)
				}),
			]
		},
	]
	used := mark_used_with_options(files, env, MarkUsedOptions{
		minimal_runtime_roots: true
	})
	main_key := decl_key('main', files[0].stmts[0] as ast.FnDecl, env)
	run_key := decl_key('main', files[0].stmts[1] as ast.FnDecl, env)
	choose_key := decl_key('main', files[0].stmts[2] as ast.FnDecl, env)
	assigned_key := decl_key('main', files[0].stmts[3] as ast.FnDecl, env)
	argument_key := decl_key('main', files[0].stmts[4] as ast.FnDecl, env)
	returned_key := decl_key('main', files[0].stmts[5] as ast.FnDecl, env)
	unused_key := decl_key('main', files[0].stmts[6] as ast.FnDecl, env)

	assert used[main_key]
	assert used[run_key]
	assert used[choose_key]
	assert used[assigned_key]
	assert used[argument_key]
	assert used[returned_key]
	assert !used[unused_key]
}

fn test_minimal_runtime_roots_keep_functions_used_as_const_values() {
	mut env := types.Environment.new()
	files := [
		ast.File{
			mod:   'main'
			name:  'main.v'
			stmts: [
				ast.Stmt(ast.ConstDecl{
					fields: [
						ast.FieldInit{
							name:  'cb'
							value: ast.Expr(minimal_ident('abc', 200))
						},
					]
				}),
				ast.Stmt(ast.FnDecl{
					name:  'main'
					typ:   ast.FnType{}
					pos:   minimal_pos(201)
					stmts: [
						ast.Stmt(ast.ExprStmt{
							expr: ast.CallExpr{
								lhs: minimal_ident('cb', 202)
								pos: minimal_pos(202)
							}
						}),
					]
				}),
				ast.Stmt(ast.FnDecl{
					name: 'abc'
					typ:  ast.FnType{}
					pos:  minimal_pos(203)
				}),
				ast.Stmt(ast.FnDecl{
					name: 'unused'
					typ:  ast.FnType{}
					pos:  minimal_pos(204)
				}),
			]
		},
	]
	used := mark_used_with_options(files, env, MarkUsedOptions{
		minimal_runtime_roots: true
	})
	main_key := decl_key('main', files[0].stmts[1] as ast.FnDecl, env)
	abc_key := decl_key('main', files[0].stmts[2] as ast.FnDecl, env)
	unused_key := decl_key('main', files[0].stmts[3] as ast.FnDecl, env)

	assert used[main_key]
	assert used[abc_key]
	assert !used[unused_key]
}

fn test_minimal_runtime_roots_flat_keeps_const_alias_return_and_const_field_function_values() {
	mut env := types.Environment.new()
	files := [
		ast.File{
			mod:   'main'
			name:  'main.v'
			stmts: [
				ast.Stmt(ast.ConstDecl{
					fields: [
						ast.FieldInit{
							name:  'cb'
							value: ast.Expr(minimal_ident('alias_cleanup', 300))
						},
					]
				}),
				ast.Stmt(ast.FnDecl{
					name:  'main'
					typ:   ast.FnType{}
					pos:   minimal_pos(301)
					stmts: [
						ast.Stmt(ast.ExprStmt{
							expr: ast.CallExpr{
								lhs: minimal_ident('cb', 302)
								pos: minimal_pos(302)
							}
						}),
						ast.Stmt(ast.ExprStmt{
							expr: ast.CallExpr{
								lhs: minimal_ident('choose_cleanup', 303)
								pos: minimal_pos(303)
							}
						}),
						ast.Stmt(ast.ConstDecl{
							fields: [
								ast.FieldInit{
									name:  'local_cb'
									value: ast.Expr(minimal_ident('field_cleanup', 304))
								},
							]
						}),
					]
				}),
				ast.Stmt(ast.FnDecl{
					name:  'choose_cleanup'
					typ:   ast.FnType{}
					pos:   minimal_pos(305)
					stmts: [
						ast.Stmt(ast.ReturnStmt{
							exprs: [
								ast.Expr(minimal_ident('returned_cleanup', 306)),
							]
						}),
					]
				}),
				ast.Stmt(ast.FnDecl{
					name: 'alias_cleanup'
					typ:  ast.FnType{}
					pos:  minimal_pos(307)
				}),
				ast.Stmt(ast.FnDecl{
					name: 'returned_cleanup'
					typ:  ast.FnType{}
					pos:  minimal_pos(308)
				}),
				ast.Stmt(ast.FnDecl{
					name: 'field_cleanup'
					typ:  ast.FnType{}
					pos:  minimal_pos(309)
				}),
				ast.Stmt(ast.FnDecl{
					name: 'unused_cleanup'
					typ:  ast.FnType{}
					pos:  minimal_pos(310)
				}),
			]
		},
	]
	used := mark_used_flat_minimal(files, env)
	main_key := decl_key('main', files[0].stmts[1] as ast.FnDecl, env)
	choose_key := decl_key('main', files[0].stmts[2] as ast.FnDecl, env)
	alias_key := decl_key('main', files[0].stmts[3] as ast.FnDecl, env)
	returned_key := decl_key('main', files[0].stmts[4] as ast.FnDecl, env)
	field_key := decl_key('main', files[0].stmts[5] as ast.FnDecl, env)
	unused_key := decl_key('main', files[0].stmts[6] as ast.FnDecl, env)

	assert used[main_key]
	assert used[choose_key]
	assert used[alias_key]
	assert used[returned_key]
	assert used[field_key]
	assert !used[unused_key]
}

fn test_minimal_runtime_roots_keep_qualified_const_function_aliases() {
	mut env := types.Environment.new()
	files := [
		ast.File{
			mod:     'main'
			name:    'main.v'
			imports: [
				ast.ImportStmt{
					name:       'dep'
					alias:      'd'
					is_aliased: true
				},
				ast.ImportStmt{
					name: 'tools'
				},
			]
			stmts:   [
				ast.Stmt(ast.ConstDecl{
					fields: [
						ast.FieldInit{
							name:  'dep_cb'
							value: ast.Expr(minimal_selector('d', 'cleanup', 210))
						},
						ast.FieldInit{
							name:  'tools_cb'
							value: ast.Expr(minimal_selector('tools', 'teardown', 213))
						},
						ast.FieldInit{
							name:  'c_cb'
							value: ast.Expr(minimal_c_selector('cleanup', 216))
						},
						ast.FieldInit{
							name:  'field_cb'
							value: ast.Expr(minimal_selector('hooks', 'cleanup', 219))
						},
					]
				}),
				ast.Stmt(ast.FnDecl{
					name:  'main'
					typ:   ast.FnType{}
					pos:   minimal_pos(220)
					stmts: [
						ast.Stmt(ast.ExprStmt{
							expr: ast.CallExpr{
								lhs: minimal_ident('dep_cb', 221)
								pos: minimal_pos(221)
							}
						}),
						ast.Stmt(ast.ExprStmt{
							expr: ast.CallExpr{
								lhs: minimal_ident('tools_cb', 222)
								pos: minimal_pos(222)
							}
						}),
						ast.Stmt(ast.ExprStmt{
							expr: ast.CallExpr{
								lhs: minimal_ident('c_cb', 223)
								pos: minimal_pos(223)
							}
						}),
						ast.Stmt(ast.ExprStmt{
							expr: ast.CallExpr{
								lhs: minimal_ident('field_cb', 224)
								pos: minimal_pos(224)
							}
						}),
					]
				}),
				ast.Stmt(ast.FnDecl{
					name: 'cleanup'
					typ:  ast.FnType{}
					pos:  minimal_pos(225)
				}),
			]
		},
		ast.File{
			mod:   'dep'
			name:  'dep.v'
			stmts: [
				ast.Stmt(ast.FnDecl{
					name: 'cleanup'
					typ:  ast.FnType{}
					pos:  minimal_pos(226)
				}),
				ast.Stmt(ast.FnDecl{
					name: 'unused'
					typ:  ast.FnType{}
					pos:  minimal_pos(227)
				}),
			]
		},
		ast.File{
			mod:   'tools'
			name:  'tools.v'
			stmts: [
				ast.Stmt(ast.FnDecl{
					name: 'teardown'
					typ:  ast.FnType{}
					pos:  minimal_pos(228)
				}),
				ast.Stmt(ast.FnDecl{
					name: 'unused'
					typ:  ast.FnType{}
					pos:  minimal_pos(229)
				}),
			]
		},
	]
	used := mark_used_with_options(files, env, MarkUsedOptions{
		minimal_runtime_roots: true
	})
	main_key := decl_key('main', files[0].stmts[1] as ast.FnDecl, env)
	main_cleanup_key := decl_key('main', files[0].stmts[2] as ast.FnDecl, env)
	dep_cleanup_key := decl_key('dep', files[1].stmts[0] as ast.FnDecl, env)
	dep_unused_key := decl_key('dep', files[1].stmts[1] as ast.FnDecl, env)
	tools_teardown_key := decl_key('tools', files[2].stmts[0] as ast.FnDecl, env)
	tools_unused_key := decl_key('tools', files[2].stmts[1] as ast.FnDecl, env)

	assert used[main_key]
	assert used[dep_cleanup_key]
	assert used[tools_teardown_key]
	assert !used[main_cleanup_key]
	assert !used[dep_unused_key]
	assert !used[tools_unused_key]
}

fn test_minimal_runtime_roots_keep_functions_used_in_composite_literals() {
	mut env := types.Environment.new()
	files := [
		ast.File{
			mod:   'main'
			name:  'main.v'
			stmts: [
				ast.Stmt(ast.StructDecl{
					name:   'Hooks'
					fields: [
						ast.FieldDecl{
							name: 'cb'
						},
					]
				}),
				ast.Stmt(ast.FnDecl{
					name:  'main'
					typ:   ast.FnType{}
					pos:   minimal_pos(220)
					stmts: [
						ast.Stmt(ast.AssignStmt{
							op:  .decl_assign
							lhs: [ast.Expr(minimal_ident('hook', 221))]
							rhs: [
								ast.Expr(ast.InitExpr{
									typ:    ast.Expr(minimal_ident('Hooks', 222))
									fields: [
										ast.FieldInit{
											name:  'cb'
											value: ast.Expr(minimal_ident('struct_cleanup', 223))
										},
									]
									pos:    minimal_pos(222)
								}),
							]
							pos: minimal_pos(221)
						}),
						ast.Stmt(ast.AssignStmt{
							op:  .decl_assign
							lhs: [
								ast.Expr(minimal_ident('items', 224)),
							]
							rhs: [
								ast.Expr(ast.ArrayInitExpr{
									exprs: [
										ast.Expr(minimal_ident('array_cleanup', 225)),
									]
									pos:   minimal_pos(224)
								}),
							]
							pos: minimal_pos(224)
						}),
						ast.Stmt(ast.AssignStmt{
							op:  .decl_assign
							lhs: [
								ast.Expr(minimal_ident('init_items', 226)),
							]
							rhs: [
								ast.Expr(ast.ArrayInitExpr{
									init: ast.Expr(minimal_ident('init_cleanup', 227))
									pos:  minimal_pos(226)
								}),
							]
							pos: minimal_pos(226)
						}),
						ast.Stmt(ast.AssignStmt{
							op:  .decl_assign
							lhs: [
								ast.Expr(minimal_ident('tuple', 228)),
							]
							rhs: [
								ast.Expr(ast.Tuple{
									exprs: [
										ast.Expr(minimal_ident('tuple_cleanup', 229)),
									]
									pos:   minimal_pos(228)
								}),
							]
							pos: minimal_pos(228)
						}),
						ast.Stmt(ast.AssignStmt{
							op:  .decl_assign
							lhs: [
								ast.Expr(minimal_ident('assoc_hook', 230)),
							]
							rhs: [
								ast.Expr(ast.AssocExpr{
									typ:    ast.Expr(minimal_ident('Hooks', 231))
									expr:   ast.Expr(minimal_ident('hook', 232))
									fields: [
										ast.FieldInit{
											name:  'cb'
											value: ast.Expr(minimal_ident('assoc_cleanup', 233))
										},
									]
									pos:    minimal_pos(230)
								}),
							]
							pos: minimal_pos(230)
						}),
						ast.Stmt(ast.AssignStmt{
							op:  .decl_assign
							lhs: [
								ast.Expr(minimal_ident('lookup', 234)),
							]
							rhs: [
								ast.Expr(ast.MapInitExpr{
									keys: [
										ast.Expr(ast.StringLiteral{
											value: 'cleanup'
											pos:   minimal_pos(235)
										}),
									]
									vals: [
										ast.Expr(minimal_ident('map_cleanup', 236)),
									]
									pos:  minimal_pos(234)
								}),
							]
							pos: minimal_pos(234)
						}),
					]
				}),
				ast.Stmt(ast.FnDecl{
					name: 'struct_cleanup'
					typ:  ast.FnType{}
					pos:  minimal_pos(237)
				}),
				ast.Stmt(ast.FnDecl{
					name: 'array_cleanup'
					typ:  ast.FnType{}
					pos:  minimal_pos(238)
				}),
				ast.Stmt(ast.FnDecl{
					name: 'init_cleanup'
					typ:  ast.FnType{}
					pos:  minimal_pos(239)
				}),
				ast.Stmt(ast.FnDecl{
					name: 'tuple_cleanup'
					typ:  ast.FnType{}
					pos:  minimal_pos(240)
				}),
				ast.Stmt(ast.FnDecl{
					name: 'assoc_cleanup'
					typ:  ast.FnType{}
					pos:  minimal_pos(241)
				}),
				ast.Stmt(ast.FnDecl{
					name: 'map_cleanup'
					typ:  ast.FnType{}
					pos:  minimal_pos(242)
				}),
				ast.Stmt(ast.FnDecl{
					name: 'unused_cleanup'
					typ:  ast.FnType{}
					pos:  minimal_pos(243)
				}),
			]
		},
	]
	used := mark_used_with_options(files, env, MarkUsedOptions{
		minimal_runtime_roots: true
	})
	main_key := decl_key('main', files[0].stmts[1] as ast.FnDecl, env)
	struct_key := decl_key('main', files[0].stmts[2] as ast.FnDecl, env)
	array_key := decl_key('main', files[0].stmts[3] as ast.FnDecl, env)
	init_key := decl_key('main', files[0].stmts[4] as ast.FnDecl, env)
	tuple_key := decl_key('main', files[0].stmts[5] as ast.FnDecl, env)
	assoc_key := decl_key('main', files[0].stmts[6] as ast.FnDecl, env)
	map_key := decl_key('main', files[0].stmts[7] as ast.FnDecl, env)
	unused_key := decl_key('main', files[0].stmts[8] as ast.FnDecl, env)

	assert used[main_key]
	assert used[struct_key]
	assert used[array_key]
	assert used[init_key]
	assert used[tuple_key]
	assert used[assoc_key]
	assert used[map_key]
	assert !used[unused_key]
}

fn test_minimal_runtime_roots_do_not_treat_c_globals_as_v_functions() {
	mut env := types.Environment.new()
	files := [
		ast.File{
			mod:   'main'
			name:  'main.v'
			stmts: [
				ast.Stmt(ast.FnDecl{
					name:  'main'
					typ:   ast.FnType{}
					pos:   minimal_pos(140)
					stmts: [
						ast.Stmt(ast.ExprStmt{
							expr: ast.CallExpr{
								lhs: ast.Ident{
									name: 'touch_c_stdio_globals'
									pos:  minimal_pos(141)
								}
								pos: minimal_pos(141)
							}
						}),
					]
				}),
			]
		},
		ast.File{
			mod:   'builtin'
			name:  'vlib/builtin/stdio_globals.v'
			stmts: [
				ast.Stmt(ast.FnDecl{
					name:  'touch_c_stdio_globals'
					typ:   ast.FnType{}
					pos:   minimal_pos(142)
					stmts: [
						ast.Stmt(ast.ExprStmt{
							expr: ast.SelectorExpr{
								lhs: ast.Ident{
									name: 'C'
									pos:  minimal_pos(143)
								}
								rhs: ast.Ident{
									name: 'stdout'
									pos:  minimal_pos(144)
								}
								pos: minimal_pos(144)
							}
						}),
						ast.Stmt(ast.ExprStmt{
							expr: ast.SelectorExpr{
								lhs: ast.Ident{
									name: 'C'
									pos:  minimal_pos(145)
								}
								rhs: ast.Ident{
									name: 'stderr'
									pos:  minimal_pos(146)
								}
								pos: minimal_pos(146)
							}
						}),
					]
				}),
			]
		},
		ast.File{
			mod:   'os'
			name:  'vlib/os/file.c.v'
			stmts: [
				ast.Stmt(ast.FnDecl{
					name: 'stdout'
					typ:  ast.FnType{}
					pos:  minimal_pos(147)
				}),
				ast.Stmt(ast.FnDecl{
					name: 'stderr'
					typ:  ast.FnType{}
					pos:  minimal_pos(148)
				}),
			]
		},
	]
	used := mark_used_with_options(files, env, MarkUsedOptions{
		minimal_runtime_roots: true
	})
	main_key := decl_key('main', files[0].stmts[0] as ast.FnDecl, env)
	touch_key := decl_key('builtin', files[1].stmts[0] as ast.FnDecl, env)
	os_stdout_key := decl_key('os', files[2].stmts[0] as ast.FnDecl, env)
	os_stderr_key := decl_key('os', files[2].stmts[1] as ast.FnDecl, env)

	assert used[main_key]
	assert used[touch_key]
	assert !used[os_stdout_key]
	assert !used[os_stderr_key]
}

fn test_minimal_runtime_roots_do_not_keep_unreached_windows_stdin_crt_helpers() {
	mut env := types.Environment.new()
	files := [
		ast.File{
			mod:   'main'
			name:  'main.v'
			stmts: [
				ast.Stmt(ast.FnDecl{
					name:  'main'
					typ:   ast.FnType{}
					pos:   minimal_pos(160)
					stmts: [
						ast.Stmt(ast.ExprStmt{
							expr: ast.CallExpr{
								lhs: minimal_ident('println', 161)
								pos: minimal_pos(161)
							}
						}),
					]
				}),
			]
		},
		ast.File{
			mod:   'builtin'
			name:  'vlib/builtin/builtin_windows.c.v'
			stmts: [
				ast.Stmt(ast.FnDecl{
					name:  'println'
					typ:   ast.FnType{}
					pos:   minimal_pos(162)
					stmts: [
						ast.Stmt(ast.ExprStmt{
							expr: ast.CallExpr{
								lhs: minimal_ident('write_stdout', 163)
								pos: minimal_pos(163)
							}
						}),
					]
				}),
				ast.Stmt(ast.FnDecl{
					name: 'write_stdout'
					typ:  ast.FnType{}
					pos:  minimal_pos(164)
				}),
				ast.Stmt(ast.FnDecl{
					name:  'read_from_std_input_handle'
					typ:   ast.FnType{}
					pos:   minimal_pos(165)
					stmts: [
						ast.Stmt(ast.ExprStmt{
							expr: minimal_c_call('GetStdHandle', 166, [
								ast.Expr(minimal_c_selector('STD_INPUT_HANDLE', 167)),
							])
						}),
					]
				}),
				ast.Stmt(ast.FnDecl{
					name:  'fd_from_crt_handle'
					typ:   ast.FnType{}
					pos:   minimal_pos(168)
					stmts: [
						ast.Stmt(ast.ExprStmt{
							expr: minimal_c_call('_get_osfhandle', 169, [])
						}),
					]
				}),
				ast.Stmt(ast.FnDecl{
					name:  'touch_c_stdin_global'
					typ:   ast.FnType{}
					pos:   minimal_pos(170)
					stmts: [
						ast.Stmt(ast.ExprStmt{
							expr: minimal_c_selector('stdin', 171)
						}),
					]
				}),
			]
		},
		ast.File{
			mod:   'os'
			name:  'vlib/os/file.c.v'
			stmts: [
				ast.Stmt(ast.FnDecl{
					name:  'fread'
					typ:   ast.FnType{}
					pos:   minimal_pos(172)
					stmts: [
						ast.Stmt(ast.ExprStmt{
							expr: minimal_c_call('fread', 173, [])
						}),
					]
				}),
				ast.Stmt(ast.FnDecl{
					name:  'read_file_chunk'
					typ:   ast.FnType{}
					pos:   minimal_pos(174)
					stmts: [
						ast.Stmt(ast.ExprStmt{
							expr: ast.CallExpr{
								lhs: minimal_ident('fread', 175)
								pos: minimal_pos(175)
							}
						}),
					]
				}),
				ast.Stmt(ast.FnDecl{
					name: 'stdin'
					typ:  ast.FnType{}
					pos:  minimal_pos(176)
				}),
			]
		},
	]
	used := mark_used_with_options(files, env, MarkUsedOptions{
		minimal_runtime_roots: true
	})
	main_key := decl_key('main', files[0].stmts[0] as ast.FnDecl, env)
	println_key := decl_key('builtin', files[1].stmts[0] as ast.FnDecl, env)
	write_stdout_key := decl_key('builtin', files[1].stmts[1] as ast.FnDecl, env)
	std_input_handle_key := decl_key('builtin', files[1].stmts[2] as ast.FnDecl, env)
	crt_handle_key := decl_key('builtin', files[1].stmts[3] as ast.FnDecl, env)
	stdin_global_key := decl_key('builtin', files[1].stmts[4] as ast.FnDecl, env)
	fread_key := decl_key('os', files[2].stmts[0] as ast.FnDecl, env)
	read_file_chunk_key := decl_key('os', files[2].stmts[1] as ast.FnDecl, env)
	os_stdin_key := decl_key('os', files[2].stmts[2] as ast.FnDecl, env)

	assert used[main_key]
	assert used[println_key]
	assert used[write_stdout_key]
	assert !used[std_input_handle_key]
	assert !used[crt_handle_key]
	assert !used[stdin_global_key]
	assert !used[fread_key]
	assert !used[read_file_chunk_key]
	assert !used[os_stdin_key]
}

fn test_minimal_runtime_roots_keep_array_push_noscan_wrapper() {
	mut env := types.Environment.new()
	files := [
		ast.File{
			mod:   'main'
			name:  'main.v'
			stmts: [
				ast.Stmt(ast.FnDecl{
					name:  'main'
					typ:   ast.FnType{}
					pos:   minimal_pos(320)
					stmts: [
						ast.Stmt(ast.ExprStmt{
							expr: ast.CallExpr{
								lhs: ast.Ident{
									name: 'builtin__array_push_noscan'
									pos:  minimal_pos(321)
								}
								pos: minimal_pos(321)
							}
						}),
					]
				}),
			]
		},
		ast.File{
			mod:   'builtin'
			name:  'vlib/builtin/array_notd_gcboehm_opt.v'
			stmts: [
				ast.Stmt(ast.FnDecl{
					is_method: true
					receiver:  ast.Parameter{
						name: 'a'
						typ:  ast.Ident{
							name: 'array'
							pos:  minimal_pos(322)
						}
						pos:  minimal_pos(322)
					}
					name:      'push_noscan'
					typ:       ast.FnType{}
					pos:       minimal_pos(323)
					stmts:     [
						ast.Stmt(ast.ExprStmt{
							expr: ast.CallExpr{
								lhs: ast.SelectorExpr{
									lhs: ast.Ident{
										name: 'a'
										pos:  minimal_pos(324)
									}
									rhs: ast.Ident{
										name: 'push'
										pos:  minimal_pos(327)
									}
									pos: minimal_pos(327)
								}
								pos: minimal_pos(327)
							}
						}),
					]
				}),
				ast.Stmt(ast.FnDecl{
					is_method: true
					receiver:  ast.Parameter{
						name: 'a'
						typ:  ast.Ident{
							name: 'array'
							pos:  minimal_pos(325)
						}
						pos:  minimal_pos(325)
					}
					name:      'push'
					typ:       ast.FnType{}
					pos:       minimal_pos(326)
				}),
			]
		},
	]
	used := mark_used_with_options(files, env, MarkUsedOptions{
		minimal_runtime_roots: true
	})
	push_noscan_key := decl_key('builtin', files[1].stmts[0] as ast.FnDecl, env)
	push_key := decl_key('builtin', files[1].stmts[1] as ast.FnDecl, env)
	assert used[push_noscan_key]
	assert used[push_key]
}

fn minimal_array_eq_runtime_dependency_files() []ast.File {
	return [
		ast.File{
			mod:   'main'
			name:  'array_eq_transformed.v'
			stmts: [
				ast.Stmt(ast.FnDecl{
					name:  'main'
					typ:   ast.FnType{}
					pos:   minimal_pos(360)
					stmts: [
						ast.Stmt(ast.ExprStmt{
							expr: ast.CallExpr{
								lhs:  ast.Expr(minimal_ident('array__eq', 360))
								args: [
									ast.Expr(minimal_ident('path', 361)),
									ast.Expr(minimal_ident('expected', 362)),
								]
								pos:  minimal_pos(363)
							}
						}),
					]
				}),
			]
		},
		ast.File{
			mod:   'builtin'
			name:  'vlib/builtin/map.v'
			stmts: [
				ast.Stmt(ast.FnDecl{
					name: 'map_map_eq'
					typ:  ast.FnType{}
					pos:  minimal_pos(364)
				}),
				ast.Stmt(ast.FnDecl{
					name: 'map_clone_string'
					typ:  ast.FnType{}
					pos:  minimal_pos(365)
				}),
			]
		},
	]
}

fn minimal_array_eq_runtime_dependency_env() &types.Environment {
	return types.Environment.new()
}

fn test_minimal_runtime_roots_transformed_array_eq_keeps_map_map_eq_dependency_legacy() {
	mut env := minimal_array_eq_runtime_dependency_env()
	files := minimal_array_eq_runtime_dependency_files()
	used := mark_used_with_options(files, env, MarkUsedOptions{
		minimal_runtime_roots: true
	})
	map_map_eq_key := decl_key('builtin', files[1].stmts[0] as ast.FnDecl, env)
	unused_key := decl_key('builtin', files[1].stmts[1] as ast.FnDecl, env)

	assert used[map_map_eq_key]
	assert !used[unused_key]
}

fn test_minimal_runtime_roots_transformed_array_eq_keeps_map_map_eq_dependency_flat() {
	mut env := minimal_array_eq_runtime_dependency_env()
	files := minimal_array_eq_runtime_dependency_files()
	used := mark_used_flat_minimal(files, env)
	map_map_eq_key := decl_key('builtin', files[1].stmts[0] as ast.FnDecl, env)
	unused_key := decl_key('builtin', files[1].stmts[1] as ast.FnDecl, env)

	assert used[map_map_eq_key]
	assert !used[unused_key]
}

fn test_minimal_runtime_roots_do_not_seed_array_rune_string_helper_or_generic_array_methods() {
	mut env := types.Environment.new()
	array_rune_type := ast.Expr(ast.Type(ast.ArrayType{
		elem_type: ast.Expr(ast.Ident{
			name: 'rune'
			pos:  minimal_pos(430)
		})
	}))
	files := [
		ast.File{
			mod:   'main'
			name:  'main.v'
			stmts: [
				ast.Stmt(ast.FnDecl{
					name: 'main'
					typ:  ast.FnType{}
					pos:  minimal_pos(431)
				}),
			]
		},
		ast.File{
			mod:   'builtin'
			name:  'vlib/builtin/rune.v'
			stmts: [
				ast.Stmt(ast.FnDecl{
					is_method: true
					receiver:  ast.Parameter{
						name: 'arr'
						typ:  ast.Expr(ast.Ident{
							name: 'array'
							pos:  minimal_pos(440)
						})
						pos:  minimal_pos(441)
					}
					name:      'string'
					typ:       ast.FnType{}
					pos:       minimal_pos(442)
				}),
				ast.Stmt(ast.FnDecl{
					is_method: true
					receiver:  ast.Parameter{
						name: 'ra'
						typ:  array_rune_type
						pos:  minimal_pos(432)
					}
					name:      'string'
					typ:       ast.FnType{}
					pos:       minimal_pos(433)
					stmts:     [
						ast.Stmt(ast.ExprStmt{
							expr: ast.CallExpr{
								lhs: ast.Ident{
									name: 'strings__new_builder'
									pos:  minimal_pos(434)
								}
								pos: minimal_pos(434)
							}
						}),
					]
				}),
				ast.Stmt(ast.FnDecl{
					is_method: true
					receiver:  ast.Parameter{
						name: 'sa'
						typ:  ast.Expr(ast.Type(ast.ArrayType{
							elem_type: ast.Expr(ast.Ident{
								name: 'string'
								pos:  minimal_pos(443)
							})
						}))
						pos:  minimal_pos(444)
					}
					name:      'string'
					typ:       ast.FnType{}
					pos:       minimal_pos(445)
				}),
				ast.Stmt(ast.FnDecl{
					is_method: true
					receiver:  ast.Parameter{
						name: 'sa'
						typ:  ast.Expr(ast.Type(ast.ArrayType{
							elem_type: ast.Expr(ast.Ident{
								name: 'string'
								pos:  minimal_pos(446)
							})
						}))
						pos:  minimal_pos(447)
					}
					name:      'str'
					typ:       ast.FnType{}
					pos:       minimal_pos(448)
				}),
				ast.Stmt(ast.FnDecl{
					name: 'unused_rune_helper'
					typ:  ast.FnType{}
					pos:  minimal_pos(435)
				}),
			]
		},
		ast.File{
			mod:   'strings'
			name:  'vlib/strings/builder.c.v'
			stmts: [
				ast.Stmt(ast.FnDecl{
					name:  'new_builder'
					typ:   ast.FnType{}
					pos:   minimal_pos(436)
					stmts: [
						ast.Stmt(ast.ExprStmt{
							expr: ast.CallExpr{
								lhs: ast.Ident{
									name: 'prepare_builder'
									pos:  minimal_pos(437)
								}
								pos: minimal_pos(437)
							}
						}),
					]
				}),
				ast.Stmt(ast.FnDecl{
					name: 'prepare_builder'
					typ:  ast.FnType{}
					pos:  minimal_pos(438)
				}),
				ast.Stmt(ast.FnDecl{
					name: 'unused_builder'
					typ:  ast.FnType{}
					pos:  minimal_pos(439)
				}),
			]
		},
	]
	used := mark_used_with_options(files, env, MarkUsedOptions{
		minimal_runtime_roots: true
	})
	main_key := decl_key('main', files[0].stmts[0] as ast.FnDecl, env)
	generic_array_string_key := decl_key('builtin', files[1].stmts[0] as ast.FnDecl, env)
	array_rune_string_key := decl_key('builtin', files[1].stmts[1] as ast.FnDecl, env)
	array_string_string_key := decl_key('builtin', files[1].stmts[2] as ast.FnDecl, env)
	array_string_str_key := decl_key('builtin', files[1].stmts[3] as ast.FnDecl, env)
	unused_rune_helper_key := decl_key('builtin', files[1].stmts[4] as ast.FnDecl, env)
	new_builder_key := decl_key('strings', files[2].stmts[0] as ast.FnDecl, env)
	prepare_builder_key := decl_key('strings', files[2].stmts[1] as ast.FnDecl, env)
	unused_builder_key := decl_key('strings', files[2].stmts[2] as ast.FnDecl, env)

	assert used[main_key]
	assert !used[generic_array_string_key]
	assert !used[array_rune_string_key]
	assert !used[array_string_string_key]
	assert !used[array_string_str_key]
	assert !used[new_builder_key]
	assert !used[prepare_builder_key]
	assert !used[unused_rune_helper_key]
	assert !used[unused_builder_key]
}

fn array_rune_string_free_exact_target_files() []ast.File {
	return [
		ast.File{
			mod:   'main'
			name:  'main.v'
			stmts: [
				ast.Stmt(ast.FnDecl{
					name:  'main'
					typ:   ast.FnType{}
					pos:   minimal_pos(500)
					stmts: [
						ast.Stmt(ast.ExprStmt{
							expr: ast.CallExpr{
								lhs: ast.Ident{
									name: 'builtin__Array_rune__string'
									pos:  minimal_pos(501)
								}
								pos: minimal_pos(501)
							}
						}),
					]
				}),
			]
		},
		ast.File{
			mod:   'builtin'
			name:  'vlib/builtin/rune.v'
			stmts: [
				ast.Stmt(ast.FnDecl{
					is_method: true
					receiver:  ast.Parameter{
						name: 'ra'
						typ:  ast.Expr(ast.Type(ast.ArrayType{
							elem_type: ast.Expr(ast.Ident{
								name: 'rune'
								pos:  minimal_pos(502)
							})
						}))
						pos:  minimal_pos(503)
					}
					name:      'string'
					typ:       ast.FnType{}
					pos:       minimal_pos(504)
					stmts:     [
						ast.Stmt(ast.ExprStmt{
							expr: ast.CallExpr{
								lhs: ast.Ident{
									name: 'strings__Builder__free'
									pos:  minimal_pos(505)
								}
								pos: minimal_pos(505)
							}
						}),
					]
				}),
				ast.Stmt(ast.FnDecl{
					is_method: true
					receiver:  ast.Parameter{
						name: 'a'
						typ:  ast.Expr(ast.Ident{
							name: 'array'
							pos:  minimal_pos(506)
						})
						pos:  minimal_pos(507)
					}
					name:      'free'
					typ:       ast.FnType{}
					pos:       minimal_pos(508)
				}),
			]
		},
		ast.File{
			mod:   'strings'
			name:  'vlib/strings/builder.v'
			stmts: [
				ast.Stmt(ast.StructDecl{
					name: 'Builder'
				}),
				ast.Stmt(ast.FnDecl{
					is_method: true
					receiver:  ast.Parameter{
						name: 'b'
						typ:  ast.Expr(ast.SelectorExpr{
							lhs: ast.Ident{
								name: 'strings'
								pos:  minimal_pos(509)
							}
							rhs: ast.Ident{
								name: 'Builder'
								pos:  minimal_pos(510)
							}
							pos: minimal_pos(509)
						})
						pos:  minimal_pos(511)
					}
					name:      'free'
					typ:       ast.FnType{}
					pos:       minimal_pos(512)
				}),
			]
		},
	]
}

fn assert_array_rune_string_marks_exact_builder_free(used map[string]bool, files []ast.File, env &types.Environment, label string) {
	main_key := decl_key('main', files[0].stmts[0] as ast.FnDecl, env)
	array_rune_string_key := decl_key('builtin', files[1].stmts[0] as ast.FnDecl, env)
	array_free_key := decl_key('builtin', files[1].stmts[1] as ast.FnDecl, env)
	builder_free_key := decl_key('strings', files[2].stmts[1] as ast.FnDecl, env)

	assert used[main_key], '${label}: main root was not kept'
	assert used[array_rune_string_key], '${label}: Array_rune.string was not reached'
	assert used[builder_free_key], '${label}: exact strings__Builder__free target was pruned'
	assert !used[array_free_key], '${label}: array__free alias replaced the exact Builder.free target'
}

fn test_minimal_runtime_roots_array_rune_string_keeps_exact_builder_free_legacy() {
	mut env := types.Environment.new()
	files := array_rune_string_free_exact_target_files()
	used := mark_used_with_options(files, env, MarkUsedOptions{
		minimal_runtime_roots: true
	})
	assert_array_rune_string_marks_exact_builder_free(used, files, env, 'legacy')
}

fn test_minimal_runtime_roots_array_rune_string_keeps_exact_builder_free_flat() {
	mut env := types.Environment.new()
	files := array_rune_string_free_exact_target_files()
	used := mark_used_flat_minimal(files, env)
	assert_array_rune_string_marks_exact_builder_free(used, files, env, 'flat')
}

fn test_minimal_runtime_roots_keep_synthesized_import_init_calls() {
	mut env := types.Environment.new()
	files := [
		ast.File{
			mod:     'main'
			name:    'main.v'
			imports: [
				ast.ImportStmt{
					name: 'dep'
				},
			]
			stmts:   [
				ast.Stmt(ast.FnDecl{
					name:  'main'
					typ:   ast.FnType{}
					pos:   minimal_pos(220)
					stmts: [
						ast.Stmt(ast.ExprStmt{
							expr: ast.CallExpr{
								lhs: ast.Ident{
									name: 'dep__init'
									pos:  minimal_pos(221)
								}
								pos: minimal_pos(221)
							}
						}),
						ast.Stmt(ast.ExprStmt{
							expr: ast.CallExpr{
								lhs: ast.Ident{
									name: 'dep____v_init_consts_dep'
									pos:  minimal_pos(222)
								}
								pos: minimal_pos(222)
							}
						}),
					]
				}),
			]
		},
		ast.File{
			mod:   'dep'
			name:  'dep.v'
			stmts: [
				ast.Stmt(ast.FnDecl{
					name:  'init'
					typ:   ast.FnType{}
					pos:   minimal_pos(223)
					stmts: [
						ast.Stmt(ast.ExprStmt{
							expr: ast.CallExpr{
								lhs: ast.Ident{
									name: 'touch'
									pos:  minimal_pos(224)
								}
								pos: minimal_pos(224)
							}
						}),
					]
				}),
				ast.Stmt(ast.FnDecl{
					name:  '__v_init_consts_dep'
					typ:   ast.FnType{}
					pos:   minimal_pos(225)
					stmts: [
						ast.Stmt(ast.ExprStmt{
							expr: ast.CallExpr{
								lhs: ast.Ident{
									name: 'make_value'
									pos:  minimal_pos(226)
								}
								pos: minimal_pos(226)
							}
						}),
					]
				}),
				ast.Stmt(ast.FnDecl{
					name: 'touch'
					typ:  ast.FnType{}
					pos:  minimal_pos(227)
				}),
				ast.Stmt(ast.FnDecl{
					name: 'make_value'
					typ:  ast.FnType{}
					pos:  minimal_pos(228)
				}),
				ast.Stmt(ast.FnDecl{
					name: 'unused'
					typ:  ast.FnType{}
					pos:  minimal_pos(229)
				}),
			]
		},
	]
	used := mark_used_with_options(files, env, MarkUsedOptions{
		minimal_runtime_roots: true
	})
	main_key := decl_key('main', files[0].stmts[0] as ast.FnDecl, env)
	dep_init_key := decl_key('dep', files[1].stmts[0] as ast.FnDecl, env)
	dep_const_init_key := decl_key('dep', files[1].stmts[1] as ast.FnDecl, env)
	touch_key := decl_key('dep', files[1].stmts[2] as ast.FnDecl, env)
	make_value_key := decl_key('dep', files[1].stmts[3] as ast.FnDecl, env)
	unused_key := decl_key('dep', files[1].stmts[4] as ast.FnDecl, env)

	assert used[main_key]
	assert used[dep_init_key]
	assert used[dep_const_init_key]
	assert used[touch_key]
	assert used[make_value_key]
	assert !used[unused_key]
}

fn test_minimal_runtime_roots_do_not_seed_forbidden_imports() {
	mut env := types.Environment.new()
	forbidden_modules := ['os', 'time', 'io', 'dl', 'sha256', 'binary']
	mut imports := []ast.ImportStmt{cap: forbidden_modules.len}
	for mod_name in forbidden_modules {
		imports << ast.ImportStmt{
			name: mod_name
		}
	}
	mut files := [
		ast.File{
			mod:     'main'
			name:    'main.v'
			imports: imports
			stmts:   [
				ast.Stmt(ast.FnDecl{
					name: 'main'
					typ:  ast.FnType{}
					pos:  minimal_pos(320)
				}),
			]
		},
	]
	for i, mod_name in forbidden_modules {
		files << ast.File{
			mod:   mod_name
			name:  '${mod_name}.v'
			stmts: [
				ast.Stmt(ast.FnDecl{
					name: 'init'
					typ:  ast.FnType{}
					pos:  minimal_pos(330 + i * 10)
				}),
				ast.Stmt(ast.FnDecl{
					name: '__v_init_consts_${mod_name}'
					typ:  ast.FnType{}
					pos:  minimal_pos(331 + i * 10)
				}),
				ast.Stmt(ast.FnDecl{
					name: 'runtime_entry'
					typ:  ast.FnType{}
					pos:  minimal_pos(332 + i * 10)
				}),
			]
		}
	}
	used := mark_used_with_options(files, env, MarkUsedOptions{
		minimal_runtime_roots: true
	})

	main_key := decl_key('main', files[0].stmts[0] as ast.FnDecl, env)
	assert used[main_key]
	for file_idx in 1 .. files.len {
		mod_name := files[file_idx].mod
		init_key := decl_key(mod_name, files[file_idx].stmts[0] as ast.FnDecl, env)
		const_init_key := decl_key(mod_name, files[file_idx].stmts[1] as ast.FnDecl, env)
		runtime_entry_key := decl_key(mod_name, files[file_idx].stmts[2] as ast.FnDecl, env)
		assert !used[init_key], '${mod_name}.init must not be a minimal root'
		assert !used[const_init_key], '${mod_name} const init must not be a minimal root'
		assert !used[runtime_entry_key], '${mod_name}.runtime_entry must not be a minimal root'
	}
}
