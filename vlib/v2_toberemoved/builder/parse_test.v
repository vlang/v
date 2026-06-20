// vtest build: macos
module builder

import os
import v2.ast
import v2.pref

fn parse_files_for_parse_test(mut b Builder, paths []string) []ast.File {
	b.parse_files(paths)
	b.flat = b.flat_builder.flat
	return b.flat.to_files()
}

fn test_parse_files_keeps_single_file_inputs_isolated() {
	tmp_dir := os.join_path(os.temp_dir(), 'v2_builder_parse_single_${os.getpid()}')
	os.rmdir_all(tmp_dir) or {}
	os.mkdir_all(tmp_dir) or { panic(err) }
	defer {
		os.rmdir_all(tmp_dir) or {}
	}

	entry_file := os.join_path(tmp_dir, 'hello.v')
	sibling_file := os.join_path(tmp_dir, 'extra.v')
	os.write_file(entry_file, 'module main\nfn main() {}\n') or { panic(err) }
	os.write_file(sibling_file, 'module main\nfn extra() {}\n') or { panic(err) }

	mut prefs := pref.new_preferences()
	prefs.skip_builtin = true
	prefs.skip_imports = true
	mut b := new_builder(&prefs)
	files := parse_files_for_parse_test(mut b, [entry_file])

	assert files.len == 1
}

fn test_parse_files_expands_directory_inputs() {
	tmp_dir := os.join_path(os.temp_dir(), 'v2_builder_parse_dir_${os.getpid()}')
	os.rmdir_all(tmp_dir) or {}
	os.mkdir_all(tmp_dir) or { panic(err) }
	defer {
		os.rmdir_all(tmp_dir) or {}
	}

	entry_file := os.join_path(tmp_dir, 'hello.v')
	sibling_file := os.join_path(tmp_dir, 'extra.v')
	os.write_file(entry_file, 'module main\nfn main() {}\n') or { panic(err) }
	os.write_file(sibling_file, 'module main\nfn extra() {}\n') or { panic(err) }

	mut prefs := pref.new_preferences()
	prefs.skip_builtin = true
	prefs.skip_imports = true
	mut b := new_builder(&prefs)
	files := parse_files_for_parse_test(mut b, [tmp_dir])

	assert files.len == 2
}

fn test_parse_files_expands_same_module_subdirectories() {
	tmp_dir := os.join_path(os.temp_dir(), 'v2_builder_parse_dir_recursive_${os.getpid()}')
	os.rmdir_all(tmp_dir) or {}
	os.mkdir_all(os.join_path(tmp_dir, 'user')) or { panic(err) }
	os.mkdir_all(os.join_path(tmp_dir, 'api')) or { panic(err) }
	os.mkdir_all(os.join_path(tmp_dir, 'nested_app')) or { panic(err) }
	defer {
		os.rmdir_all(tmp_dir) or {}
	}

	entry_file := os.join_path(tmp_dir, 'main.v')
	user_file := os.join_path(tmp_dir, 'user', 'user.v')
	api_file := os.join_path(tmp_dir, 'api', 'api.v')
	nested_file := os.join_path(tmp_dir, 'nested_app', 'main.v')
	os.write_file(entry_file, 'module main\nfn main() {}\n') or { panic(err) }
	os.write_file(user_file, 'module main\nstruct User {}\n') or { panic(err) }
	os.write_file(api_file, 'module api\nstruct Payload {}\n') or { panic(err) }
	os.write_file(os.join_path(tmp_dir, 'nested_app', 'v.mod'), 'Module { name: "nested" }\n') or {
		panic(err)
	}
	os.write_file(nested_file, 'module main\nfn nested_main() {}\n') or { panic(err) }

	mut prefs := pref.new_preferences()
	prefs.skip_builtin = true
	prefs.skip_imports = true
	mut b := new_builder(&prefs)
	files := parse_files_for_parse_test(mut b, [tmp_dir])
	names := files.map(os.file_name(it.name))

	assert files.len == 2
	assert 'main.v' in names
	assert 'user.v' in names
}

fn test_parse_files_expands_implicit_main_subdirectories() {
	tmp_dir := os.join_path(os.temp_dir(), 'v2_builder_parse_dir_implicit_main_${os.getpid()}')
	os.rmdir_all(tmp_dir) or {}
	os.mkdir_all(os.join_path(tmp_dir, 'heroes')) or { panic(err) }
	defer {
		os.rmdir_all(tmp_dir) or {}
	}

	entry_file := os.join_path(tmp_dir, 'main.v')
	hero_file := os.join_path(tmp_dir, 'heroes', 'hero.v')
	os.write_file(entry_file, 'fn main() {}\n') or { panic(err) }
	os.write_file(hero_file, 'struct HeroEffect {}\n') or { panic(err) }

	mut prefs := pref.new_preferences()
	prefs.skip_builtin = true
	prefs.skip_imports = true
	mut b := new_builder(&prefs)
	files := parse_files_for_parse_test(mut b, [tmp_dir])
	names := files.map(os.file_name(it.name))

	assert files.len == 2
	assert 'main.v' in names
	assert 'hero.v' in names
}

fn test_parse_files_skips_block_commented_non_main_subdirectories() {
	tmp_dir := os.join_path(os.temp_dir(), 'v2_builder_parse_dir_block_comment_mod_${os.getpid()}')
	os.rmdir_all(tmp_dir) or {}
	os.mkdir_all(os.join_path(tmp_dir, 'api')) or { panic(err) }
	defer {
		os.rmdir_all(tmp_dir) or {}
	}

	entry_file := os.join_path(tmp_dir, 'main.v')
	api_file := os.join_path(tmp_dir, 'api', 'api.v')
	os.write_file(entry_file, 'module main\nfn main() {}\n') or { panic(err) }
	os.write_file(api_file, '/* license header */\nmodule api\nstruct Payload {}\n') or {
		panic(err)
	}

	mut prefs := pref.new_preferences()
	prefs.skip_builtin = true
	prefs.skip_imports = true
	mut b := new_builder(&prefs)
	files := parse_files_for_parse_test(mut b, [tmp_dir])
	names := files.map(os.file_name(it.name))

	assert files.len == 1
	assert 'main.v' in names
	assert 'api.v' !in names
}

fn test_virtual_main_modules_skip_groups_with_executable_main_from_paths() {
	tmp_dir := os.join_path(os.temp_dir(), 'v2_builder_virtual_paths_main_${os.getpid()}')
	os.rmdir_all(tmp_dir) or {}
	os.mkdir_all(os.join_path(tmp_dir, 'admin')) or { panic(err) }
	os.mkdir_all(os.join_path(tmp_dir, 'tests')) or { panic(err) }
	defer {
		os.rmdir_all(tmp_dir) or {}
	}

	main_file := os.join_path(tmp_dir, 'main.v')
	admin_file := os.join_path(tmp_dir, 'admin', 'admin.v')
	test_file := os.join_path(tmp_dir, 'tests', 'first_run.v')
	os.write_file(main_file, 'module main\nfn main() {}\n') or { panic(err) }
	os.write_file(admin_file, 'module main\nfn admin_route() {}\n') or { panic(err) }
	os.write_file(test_file, 'module main\nfn main() {}\n') or { panic(err) }

	mut prefs := pref.new_preferences()
	prefs.skip_builtin = true
	prefs.skip_imports = true
	mut b := new_builder(&prefs)
	b.user_files = [tmp_dir]
	groups := b.collect_virtual_main_modules_from_paths([main_file, admin_file, test_file])
	names := groups.map(it.name)

	assert names == ['admin']
}

fn test_virtual_main_modules_skip_groups_with_attributed_executable_main_from_paths() {
	tmp_dir := os.join_path(os.temp_dir(),
		'v2_builder_virtual_paths_attributed_main_${os.getpid()}')
	os.rmdir_all(tmp_dir) or {}
	os.mkdir_all(os.join_path(tmp_dir, 'admin')) or { panic(err) }
	defer {
		os.rmdir_all(tmp_dir) or {}
	}

	main_file := os.join_path(tmp_dir, 'main.v')
	admin_file := os.join_path(tmp_dir, 'admin', 'admin.v')
	os.write_file(main_file, 'module main\nfn main() {}\n') or { panic(err) }
	os.write_file(admin_file, 'module main\n@[console] fn main() {}\n') or { panic(err) }

	mut prefs := pref.new_preferences()
	prefs.skip_builtin = true
	prefs.skip_imports = true
	mut b := new_builder(&prefs)
	b.user_files = [tmp_dir]
	groups := b.collect_virtual_main_modules_from_paths([main_file, admin_file])

	assert groups.len == 0
}

fn test_virtual_main_modules_skip_groups_with_executable_main_from_ast() {
	tmp_dir := os.join_path(os.temp_dir(), 'v2_builder_virtual_ast_main_${os.getpid()}')
	os.rmdir_all(tmp_dir) or {}
	os.mkdir_all(os.join_path(tmp_dir, 'admin')) or { panic(err) }
	os.mkdir_all(os.join_path(tmp_dir, 'tests')) or { panic(err) }
	defer {
		os.rmdir_all(tmp_dir) or {}
	}

	os.write_file(os.join_path(tmp_dir, 'main.v'), 'module main\nfn main() {}\n') or { panic(err) }
	os.write_file(os.join_path(tmp_dir, 'admin', 'admin.v'), 'module main\nfn admin_route() {}\n') or {
		panic(err)
	}
	os.write_file(os.join_path(tmp_dir, 'tests', 'first_run.v'), 'module main\nfn main() {}\n') or {
		panic(err)
	}

	mut prefs := pref.new_preferences()
	prefs.skip_builtin = true
	prefs.skip_imports = true
	mut b := new_builder(&prefs)
	b.user_files = [tmp_dir]
	b.files = parse_files_for_parse_test(mut b, [tmp_dir])
	groups := b.collect_virtual_main_modules()
	names := groups.map(it.name)

	assert names == ['admin']
}

fn test_parse_files_expands_non_main_module_files() {
	tmp_dir := os.join_path(os.temp_dir(), 'v2_builder_parse_module_${os.getpid()}')
	os.rmdir_all(tmp_dir) or {}
	os.mkdir_all(tmp_dir) or { panic(err) }
	defer {
		os.rmdir_all(tmp_dir) or {}
	}

	entry_file := os.join_path(tmp_dir, 'math_test.v')
	sibling_file := os.join_path(tmp_dir, 'math.v')
	os.write_file(entry_file, 'module math\nconst x = twice(21)\n') or { panic(err) }
	os.write_file(sibling_file, 'module math\nfn twice(v int) int { return v * 2 }\n') or {
		panic(err)
	}

	mut prefs := pref.new_preferences()
	prefs.skip_builtin = true
	prefs.skip_imports = true
	mut b := new_builder(&prefs)
	files := parse_files_for_parse_test(mut b, [entry_file])

	assert files.len == 2
}

fn test_parse_files_resolves_project_root_sibling_import_from_nested_module_file() {
	tmp_dir := os.join_path(os.temp_dir(), 'v2_builder_parse_nested_import_${os.getpid()}')
	flags_dir := os.join_path(tmp_dir, 'core', 'flags')
	ignore_dir := os.join_path(tmp_dir, 'ignore')
	os.rmdir_all(tmp_dir) or {}
	os.mkdir_all(flags_dir) or { panic(err) }
	os.mkdir_all(ignore_dir) or { panic(err) }
	defer {
		os.rmdir_all(tmp_dir) or {}
	}

	entry_file := os.join_path(flags_dir, 'complete_test.v')
	ignore_file := os.join_path(ignore_dir, 'lib.v')
	os.write_file(os.join_path(tmp_dir, 'v.mod'), "Module { name: 'nested_import' }\n") or {
		panic(err)
	}
	os.write_file(entry_file, 'module flags\nimport ignore\nfn test_use_import() {}\n') or {
		panic(err)
	}
	os.write_file(ignore_file, 'module ignore\npub struct Marker {}\n') or { panic(err) }

	mut prefs := pref.new_preferences()
	prefs.skip_builtin = true
	mut b := new_builder(&prefs)
	files := parse_files_for_parse_test(mut b, [entry_file])
	names := files.map(os.file_name(it.name))

	assert 'complete_test.v' in names
	assert 'lib.v' in names
}

fn test_parse_files_prefers_project_root_module_over_vlib_module() {
	tmp_dir := os.join_path(os.temp_dir(), 'v2_builder_parse_local_shadow_${os.getpid()}')
	app_dir := os.join_path(tmp_dir, 'app')
	regex_dir := os.join_path(tmp_dir, 'regex')
	os.rmdir_all(tmp_dir) or {}
	os.mkdir_all(app_dir) or { panic(err) }
	os.mkdir_all(regex_dir) or { panic(err) }
	defer {
		os.rmdir_all(tmp_dir) or {}
	}

	entry_file := os.join_path(app_dir, 'main.v')
	local_regex_file := os.join_path(regex_dir, 'local_regex.v')
	os.write_file(os.join_path(tmp_dir, 'v.mod'), "Module { name: 'local_shadow' }\n") or {
		panic(err)
	}
	os.write_file(entry_file, 'module main\nimport regex\nfn main() {}\n') or { panic(err) }
	os.write_file(local_regex_file, 'module regex\npub struct RegexMatcher {}\n') or { panic(err) }

	mut prefs := pref.new_preferences()
	prefs.skip_builtin = true
	mut b := new_builder(&prefs)
	files := parse_files_for_parse_test(mut b, [entry_file])

	assert files.any(it.name == local_regex_file)
}

fn test_active_file_imports_follow_comptime_else_branch() {
	file := ast.File{
		mod:   'main'
		name:  'main.v'
		stmts: [
			ast.Stmt(ast.ExprStmt{
				expr: ast.ComptimeExpr{
					expr: ast.IfExpr{
						cond:      ast.Ident{
							name: 'linux'
						}
						stmts:     [
							ast.Stmt(ast.ImportStmt{
								name:  'net.openssl'
								alias: 'openssl'
							}),
						]
						else_expr: ast.IfExpr{
							cond:  ast.empty_expr
							stmts: [
								ast.Stmt(ast.ImportStmt{
									name:  'net.mbedtls'
									alias: 'mbedtls'
								}),
							]
						}
					}
				}
			}),
		]
	}
	imports := active_file_imports(file, [], 'mac')

	assert imports.len == 1
	assert imports[0].name == 'net.mbedtls'
}

fn test_active_file_imports_skips_sync_for_channel_in_inactive_comptime_branch() {
	file := ast.File{
		mod:   'main'
		name:  'main.v'
		stmts: [
			ast.Stmt(ast.ExprStmt{
				expr: ast.ComptimeExpr{
					expr: ast.IfExpr{
						cond:  ast.Ident{
							name: 'windows'
						}
						stmts: [
							ast.Stmt(ast.ExprStmt{
								expr: ast.Type(ast.ChannelType{
									elem_type: ast.Expr(ast.Ident{
										name: 'int'
									})
								})
							}),
						]
					}
				}
			}),
		]
	}
	linux_imports := active_file_imports(file, [], 'linux').map(it.name)
	windows_imports := active_file_imports(file, [], 'windows').map(it.name)

	assert 'sync' !in linux_imports
	assert 'sync' in windows_imports
}

fn test_active_file_imports_skips_sync_for_channel_in_sync_module() {
	file := ast.File{
		mod:   'sync'
		name:  'cond.v'
		stmts: [
			ast.Stmt(ast.ModuleStmt{
				name: 'sync'
			}),
			ast.Stmt(ast.StructDecl{
				name:   'Cond'
				fields: [
					ast.FieldDecl{
						name: 'waiters'
						typ:  ast.Type(ast.ArrayType{
							elem_type: ast.Expr(ast.Type(ast.ChannelType{
								elem_type: ast.Expr(ast.Ident{
									name: 'bool'
								})
							}))
						})
					},
				]
			}),
		]
	}
	imports := active_file_imports(file, [], 'mac').map(it.name)
	mut flat_builder := ast.new_flat_builder()
	flat_builder.append_file(file)
	flat_imports := active_file_imports_from_flat(&flat_builder.flat, flat_builder.flat.files[0],
		[], [], 'mac').map(it.name)

	assert 'sync' !in imports
	assert 'sync' !in flat_imports
}

fn test_active_file_imports_adds_sync_for_channel_in_if_condition() {
	file := ast.File{
		mod:   'main'
		name:  'main.v'
		stmts: [
			ast.Stmt(ast.ExprStmt{
				expr: ast.IfExpr{
					cond: ast.CallExpr{
						lhs:  ast.Expr(ast.Ident{
							name: 'takes_chan'
						})
						args: [
							ast.Expr(ast.Type(ast.ChannelType{
								elem_type: ast.Expr(ast.Ident{
									name: 'int'
								})
							})),
						]
					}
				}
			}),
		]
	}
	imports := active_file_imports(file, [], 'mac')

	assert imports.any(it.name == 'sync')
}

fn test_active_file_imports_adds_sync_for_channel_in_for_condition() {
	file := ast.File{
		mod:   'main'
		name:  'main.v'
		stmts: [
			ast.Stmt(ast.ForStmt{
				cond: ast.CallExpr{
					lhs:  ast.Expr(ast.Ident{
						name: 'takes_chan'
					})
					args: [
						ast.Expr(ast.Type(ast.ChannelType{
							elem_type: ast.Expr(ast.Ident{
								name: 'int'
							})
						})),
					]
				}
			}),
		]
	}
	imports := active_file_imports(file, [], 'mac')

	assert imports.any(it.name == 'sync')
}

fn test_active_file_imports_adds_sync_for_channel_in_assert_expr() {
	file := ast.File{
		mod:   'main'
		name:  'main.v'
		stmts: [
			ast.Stmt(ast.AssertStmt{
				expr: ast.CallExpr{
					lhs:  ast.Expr(ast.Ident{
						name: 'takes_chan'
					})
					args: [
						ast.Expr(ast.Type(ast.ChannelType{
							elem_type: ast.Expr(ast.Ident{
								name: 'int'
							})
						})),
					]
				}
			}),
		]
	}
	imports := active_file_imports(file, [], 'mac')

	assert imports.any(it.name == 'sync')
}

fn test_active_file_imports_adds_sync_for_channel_in_array_literal() {
	file := ast.File{
		mod:   'main'
		name:  'main.v'
		stmts: [
			ast.Stmt(ast.ExprStmt{
				expr: ast.ArrayInitExpr{
					exprs: [
						ast.Expr(ast.Type(ast.ChannelType{
							elem_type: ast.Expr(ast.Ident{
								name: 'int'
							})
						})),
					]
				}
			}),
		]
	}
	imports := active_file_imports(file, [], 'mac')

	assert imports.any(it.name == 'sync')
}

fn test_active_file_imports_adds_sync_for_channel_in_match_condition() {
	file := ast.File{
		mod:   'main'
		name:  'main.v'
		stmts: [
			ast.Stmt(ast.ExprStmt{
				expr: ast.MatchExpr{
					expr:     ast.Expr(ast.Ident{
						name: 'value'
					})
					branches: [
						ast.MatchBranch{
							cond: [
								ast.Expr(ast.Type(ast.ChannelType{
									elem_type: ast.Expr(ast.Ident{
										name: 'int'
									})
								})),
							]
						},
					]
				}
			}),
		]
	}
	imports := active_file_imports(file, [], 'mac')

	assert imports.any(it.name == 'sync')
}

fn test_active_file_imports_adds_sync_for_nested_channel_in_infix_expr() {
	file := ast.File{
		mod:   'main'
		name:  'main.v'
		stmts: [
			ast.Stmt(ast.ExprStmt{
				expr: ast.IfExpr{
					cond: ast.InfixExpr{
						op:  .and
						lhs: ast.Expr(ast.Ident{
							name: 'ok'
						})
						rhs: ast.Expr(ast.CallExpr{
							lhs:  ast.Expr(ast.Ident{
								name: 'takes_chan'
							})
							args: [
								ast.Expr(ast.Type(ast.ChannelType{
									elem_type: ast.Expr(ast.Ident{
										name: 'int'
									})
								})),
							]
						})
					}
				}
			}),
		]
	}
	imports := active_file_imports(file, [], 'mac')

	assert imports.any(it.name == 'sync')
}

fn test_active_file_imports_adds_sync_for_channel_in_call_callee_generic_args() {
	file := ast.File{
		mod:   'main'
		name:  'main.v'
		stmts: [
			ast.Stmt(ast.ExprStmt{
				expr: ast.CallExpr{
					lhs: ast.Expr(ast.GenericArgs{
						lhs:  ast.Expr(ast.Ident{
							name: 'make_value'
						})
						args: [
							ast.Expr(ast.Type(ast.ChannelType{
								elem_type: ast.Expr(ast.Ident{
									name: 'int'
								})
							})),
						]
					})
				}
			}),
		]
	}
	imports := active_file_imports(file, [], 'mac')

	assert imports.any(it.name == 'sync')
}

fn test_active_file_imports_adds_sync_for_channel_in_modifier_expr_operand() {
	file := ast.File{
		mod:   'main'
		name:  'main.v'
		stmts: [
			ast.Stmt(ast.ExprStmt{
				expr: ast.CallExpr{
					lhs:  ast.Expr(ast.Ident{
						name: 'use'
					})
					args: [
						ast.Expr(ast.ModifierExpr{
							kind: .key_mut
							expr: ast.IndexExpr{
								lhs:  ast.Expr(ast.Ident{
									name: 'arr'
								})
								expr: ast.Expr(ast.CallExpr{
									lhs:  ast.Expr(ast.Ident{
										name: 'idx'
									})
									args: [
										ast.Expr(ast.Type(ast.ChannelType{
											elem_type: ast.Expr(ast.Ident{
												name: 'int'
											})
										})),
									]
								})
							}
						}),
					]
				}
			}),
		]
	}
	imports := active_file_imports(file, [], 'mac')

	assert imports.any(it.name == 'sync')
}

fn test_active_file_imports_adds_sync_for_channel_in_assign_lhs() {
	file := ast.File{
		mod:   'main'
		name:  'main.v'
		stmts: [
			ast.Stmt(ast.AssignStmt{
				lhs: [
					ast.Expr(ast.IndexExpr{
						lhs:  ast.Expr(ast.Ident{
							name: 'arr'
						})
						expr: ast.Expr(ast.CallExpr{
							lhs:  ast.Expr(ast.Ident{
								name: 'idx'
							})
							args: [
								ast.Expr(ast.Type(ast.ChannelType{
									elem_type: ast.Expr(ast.Ident{
										name: 'int'
									})
								})),
							]
						})
					}),
				]
				rhs: [
					ast.Expr(ast.BasicLiteral{
						kind:  .number
						value: '0'
					}),
				]
			}),
		]
	}
	imports := active_file_imports(file, [], 'mac')

	assert imports.any(it.name == 'sync')
}

fn test_active_file_imports_adds_sync_for_channel_in_labelled_loop() {
	file := ast.File{
		mod:   'main'
		name:  'main.v'
		stmts: [
			ast.Stmt(ast.LabelStmt{
				name: 'outer'
				stmt: ast.Stmt(ast.ForStmt{
					stmts: [
						ast.Stmt(ast.AssignStmt{
							lhs: [
								ast.Expr(ast.Ident{
									name: '_'
								}),
							]
							rhs: [
								ast.Expr(ast.Type(ast.ChannelType{
									elem_type: ast.Expr(ast.Ident{
										name: 'int'
									})
								})),
							]
						}),
					]
				})
			}),
		]
	}
	imports := active_file_imports(file, [], 'mac')

	assert imports.any(it.name == 'sync')
}

fn test_active_file_imports_ignores_channel_in_inactive_attributed_fn_body() {
	file := ast.File{
		mod:   'main'
		name:  'main.v'
		stmts: [
			ast.Stmt(ast.FnDecl{
				name:       'debug'
				typ:        ast.FnType{}
				attributes: [
					ast.Attribute{
						name:          'if'
						comptime_cond: ast.Expr(ast.Ident{
							name: 'windows'
						})
					},
				]
				stmts:      [
					ast.Stmt(ast.AssignStmt{
						lhs: [
							ast.Expr(ast.Ident{
								name: '_'
							}),
						]
						rhs: [
							ast.Expr(ast.Type(ast.ChannelType{
								elem_type: ast.Expr(ast.Ident{
									name: 'int'
								})
							})),
						]
					}),
				]
			}),
		]
	}
	imports := active_file_imports(file, [], 'mac')

	assert !imports.any(it.name == 'sync')
}

fn test_active_file_imports_skip_pkgconfig_branch_when_disabled() {
	if !pref.comptime_pkgconfig_value('sqlite3') {
		return
	}
	file := ast.File{
		mod:   'main'
		name:  'main.v'
		stmts: [
			ast.Stmt(ast.ExprStmt{
				expr: ast.ComptimeExpr{
					expr: ast.IfExpr{
						cond:  ast.CallExpr{
							lhs:  ast.Expr(ast.Ident{
								name: 'pkgconfig'
							})
							args: [
								ast.Expr(ast.StringLiteral{
									kind:  .v
									value: "'sqlite3'"
								}),
							]
						}
						stmts: [
							ast.Stmt(ast.ImportStmt{
								name: 'db.sqlite'
							}),
						]
					}
				}
			}),
		]
	}
	native_imports := active_file_imports_with_options(file, [], [], 'mac', true).map(it.name)
	cross_imports := active_file_imports_with_options(file, [], [], 'mac', false).map(it.name)

	assert 'db.sqlite' in native_imports
	assert 'db.sqlite' !in cross_imports
}
