// vtest build: macos
module markused

import v2.ast
import v2.token
import v2.types

fn pos(id int) token.Pos {
	return token.Pos{
		offset: id
		id:     id
	}
}

fn test_interface_name_from_type_handles_unresolved_alias_base_type() {
	mut env := types.Environment.new()
	w := new_walker([]ast.File{}, env, MarkUsedOptions{})
	assert w.interface_name_from_type(types.Type(types.Alias{
		name: 'UnresolvedAlias'
	})) == ''
}

fn test_mark_used_tracks_transitive_function_calls() {
	mut env := types.Environment.new()
	files := [
		ast.File{
			mod:   'main'
			name:  'main.v'
			stmts: [
				ast.Stmt(ast.FnDecl{
					name:  'main'
					typ:   ast.FnType{}
					pos:   pos(1)
					stmts: [
						ast.Stmt(ast.ExprStmt{
							expr: ast.CallExpr{
								lhs: ast.Ident{
									name: 'foo'
									pos:  pos(2)
								}
								pos: pos(2)
							}
						}),
					]
				}),
				ast.Stmt(ast.FnDecl{
					name:  'foo'
					typ:   ast.FnType{}
					pos:   pos(3)
					stmts: [
						ast.Stmt(ast.ExprStmt{
							expr: ast.CallExpr{
								lhs: ast.Ident{
									name: 'bar'
									pos:  pos(4)
								}
								pos: pos(4)
							}
						}),
					]
				}),
				ast.Stmt(ast.FnDecl{
					name: 'bar'
					typ:  ast.FnType{}
					pos:  pos(5)
				}),
				ast.Stmt(ast.FnDecl{
					name: 'dead'
					typ:  ast.FnType{}
					pos:  pos(6)
				}),
			]
		},
	]
	used := mark_used(files, env)
	main_key := decl_key('main', files[0].stmts[0] as ast.FnDecl, env)
	foo_key := decl_key('main', files[0].stmts[1] as ast.FnDecl, env)
	bar_key := decl_key('main', files[0].stmts[2] as ast.FnDecl, env)
	dead_key := decl_key('main', files[0].stmts[3] as ast.FnDecl, env)
	assert used[main_key]
	assert used[foo_key]
	assert used[bar_key]
	assert !used[dead_key]
}

fn test_mark_used_walks_duplicate_declaration_with_body() {
	mut env := types.Environment.new()
	files := [
		ast.File{
			mod:   'main'
			name:  'main.v'
			stmts: [
				ast.Stmt(ast.FnDecl{
					name:  'main'
					typ:   ast.FnType{}
					pos:   pos(20)
					stmts: [
						ast.Stmt(ast.ExprStmt{
							expr: ast.CallExpr{
								lhs: ast.Ident{
									name: 'foo'
									pos:  pos(21)
								}
								pos: pos(21)
							}
						}),
					]
				}),
				ast.Stmt(ast.FnDecl{
					name: 'foo'
					typ:  ast.FnType{}
					pos:  pos(22)
				}),
				ast.Stmt(ast.FnDecl{
					name:  'foo'
					typ:   ast.FnType{}
					pos:   pos(23)
					stmts: [
						ast.Stmt(ast.ExprStmt{
							expr: ast.CallExpr{
								lhs: ast.Ident{
									name: 'bar'
									pos:  pos(24)
								}
								pos: pos(24)
							}
						}),
					]
				}),
				ast.Stmt(ast.FnDecl{
					name: 'bar'
					typ:  ast.FnType{}
					pos:  pos(25)
				}),
			]
		},
	]
	used := mark_used(files, env)
	foo_key := decl_key('main', files[0].stmts[1] as ast.FnDecl, env)
	bar_key := decl_key('main', files[0].stmts[3] as ast.FnDecl, env)
	assert used[foo_key]
	assert used[bar_key]
}

fn test_mark_used_tracks_method_calls_with_env_types() {
	mut env := types.Environment.new()
	env.set_expr_type(12, types.Struct{
		name: 'Widget'
	})
	files := [
		ast.File{
			mod:   'main'
			name:  'main.v'
			stmts: [
				ast.Stmt(ast.FnDecl{
					name:  'main'
					typ:   ast.FnType{}
					pos:   pos(10)
					stmts: [
						ast.Stmt(ast.ExprStmt{
							expr: ast.CallExpr{
								lhs: ast.SelectorExpr{
									lhs: ast.Ident{
										name: 'w'
										pos:  pos(12)
									}
									rhs: ast.Ident{
										name: 'ping'
										pos:  pos(13)
									}
									pos: pos(13)
								}
								pos: pos(13)
							}
						}),
					]
				}),
				ast.Stmt(ast.FnDecl{
					is_method: true
					receiver:  ast.Parameter{
						name: 'w'
						typ:  ast.Ident{
							name: 'Widget'
							pos:  pos(14)
						}
						pos:  pos(14)
					}
					name:      'ping'
					typ:       ast.FnType{}
					pos:       pos(15)
				}),
				ast.Stmt(ast.FnDecl{
					is_method: true
					receiver:  ast.Parameter{
						name: 'w'
						typ:  ast.Ident{
							name: 'Widget'
							pos:  pos(16)
						}
						pos:  pos(16)
					}
					name:      'unused'
					typ:       ast.FnType{}
					pos:       pos(17)
				}),
			]
		},
	]
	used := mark_used(files, env)
	main_key := decl_key('main', files[0].stmts[0] as ast.FnDecl, env)
	ping_key := decl_key('main', files[0].stmts[1] as ast.FnDecl, env)
	unused_key := decl_key('main', files[0].stmts[2] as ast.FnDecl, env)
	assert used[main_key]
	assert used[ping_key]
	assert !used[unused_key]
}

fn test_mark_used_tracks_current_receiver_method_calls() {
	mut env := types.Environment.new()
	env.set_expr_type(32, types.Struct{
		name: 'Builder'
	})
	builder_ptr := ast.Type(ast.PointerType{
		base_type: ast.Ident{
			name: 'Builder'
			pos:  pos(33)
		}
	})
	files := [
		ast.File{
			mod:   'main'
			name:  'main.v'
			stmts: [
				ast.Stmt(ast.FnDecl{
					name:  'main'
					typ:   ast.FnType{}
					pos:   pos(30)
					stmts: [
						ast.Stmt(ast.ExprStmt{
							expr: ast.CallExpr{
								lhs: ast.SelectorExpr{
									lhs: ast.Ident{
										name: 'b'
										pos:  pos(32)
									}
									rhs: ast.Ident{
										name: 'build'
										pos:  pos(34)
									}
									pos: pos(34)
								}
								pos: pos(34)
							}
						}),
					]
				}),
				ast.Stmt(ast.FnDecl{
					is_method: true
					receiver:  ast.Parameter{
						name: 'b'
						typ:  builder_ptr
						pos:  pos(35)
					}
					name:      'build'
					typ:       ast.FnType{}
					pos:       pos(36)
					stmts:     [
						ast.Stmt(ast.ExprStmt{
							expr: ast.CallExpr{
								lhs: ast.SelectorExpr{
									lhs: ast.Ident{
										name: 'b'
										pos:  pos(37)
									}
									rhs: ast.Ident{
										name: 'helper'
										pos:  pos(38)
									}
									pos: pos(38)
								}
								pos: pos(38)
							}
						}),
					]
				}),
				ast.Stmt(ast.FnDecl{
					is_method: true
					receiver:  ast.Parameter{
						name: 'b'
						typ:  builder_ptr
						pos:  pos(39)
					}
					name:      'helper'
					typ:       ast.FnType{}
					pos:       pos(40)
				}),
				ast.Stmt(ast.FnDecl{
					is_method: true
					receiver:  ast.Parameter{
						name: 'b'
						typ:  builder_ptr
						pos:  pos(41)
					}
					name:      'unused'
					typ:       ast.FnType{}
					pos:       pos(42)
				}),
			]
		},
	]
	used := mark_used(files, env)
	main_key := decl_key('main', files[0].stmts[0] as ast.FnDecl, env)
	build_key := decl_key('main', files[0].stmts[1] as ast.FnDecl, env)
	helper_key := decl_key('main', files[0].stmts[2] as ast.FnDecl, env)
	unused_key := decl_key('main', files[0].stmts[3] as ast.FnDecl, env)
	assert used[main_key]
	assert used[build_key]
	assert used[helper_key]
	assert !used[unused_key]
}

fn test_mark_used_walks_codegen_required_str_methods() {
	mut env := types.Environment.new()
	files := [
		ast.File{
			mod:   'main'
			name:  'main.v'
			stmts: [
				ast.Stmt(ast.FnDecl{
					name: 'main'
					typ:  ast.FnType{}
					pos:  pos(90)
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
							pos:  pos(91)
						}
						pos:  pos(91)
					}
					name:      'str'
					typ:       ast.FnType{}
					pos:       pos(92)
					stmts:     [
						ast.Stmt(ast.ReturnStmt{
							exprs: [
								ast.Expr(ast.CallExpr{
									lhs: ast.SelectorExpr{
										lhs: ast.Ident{
											name: 't'
											pos:  pos(93)
										}
										rhs: ast.Ident{
											name: 'format_ss'
											pos:  pos(94)
										}
										pos: pos(94)
									}
									pos: pos(94)
								}),
							]
						}),
					]
				}),
				ast.Stmt(ast.FnDecl{
					is_method: true
					receiver:  ast.Parameter{
						name: 't'
						typ:  ast.Ident{
							name: 'Time'
							pos:  pos(95)
						}
						pos:  pos(95)
					}
					name:      'format_ss'
					typ:       ast.FnType{}
					pos:       pos(96)
				}),
			]
		},
	]
	used := mark_used(files, env)
	main_key := decl_key('main', files[0].stmts[0] as ast.FnDecl, env)
	str_key := decl_key('time', files[1].stmts[1] as ast.FnDecl, env)
	format_key := decl_key('time', files[1].stmts[2] as ast.FnDecl, env)
	assert used[main_key]
	assert used[str_key]
	assert used[format_key]
}

fn test_mark_used_walks_codegen_required_sync_method_roots() {
	mut env := types.Environment.new()
	spin_lock_ptr := ast.Expr(ast.PrefixExpr{
		pos:  pos(110)
		op:   .amp
		expr: ast.Ident{
			name: 'SpinLock'
			pos:  pos(111)
		}
	})
	files := [
		ast.File{
			mod:   'main'
			name:  'main.v'
			stmts: [
				ast.Stmt(ast.FnDecl{
					name: 'main'
					typ:  ast.FnType{}
					pos:  pos(112)
				}),
			]
		},
		ast.File{
			mod:   'sync'
			name:  'channels.c.v'
			stmts: [
				ast.Stmt(ast.StructDecl{
					name: 'SpinLock'
				}),
				ast.Stmt(ast.StructDecl{
					name:   'Channel'
					fields: [
						ast.FieldDecl{
							name: 'read_sub_mtx'
							typ:  spin_lock_ptr
						},
					]
				}),
				ast.Stmt(ast.FnDecl{
					is_method: true
					receiver:  ast.Parameter{
						name: 'ch'
						typ:  ast.Ident{
							name: 'Channel'
							pos:  pos(113)
						}
						pos:  pos(113)
					}
					name:      'try_wait'
					typ:       ast.FnType{}
					pos:       pos(114)
					stmts:     [
						ast.Stmt(ast.ExprStmt{
							expr: ast.CallExpr{
								lhs: ast.SelectorExpr{
									lhs: ast.SelectorExpr{
										lhs: ast.Ident{
											name: 'ch'
											pos:  pos(115)
										}
										rhs: ast.Ident{
											name: 'read_sub_mtx'
											pos:  pos(116)
										}
										pos: pos(116)
									}
									rhs: ast.Ident{
										name: 'lock'
										pos:  pos(117)
									}
									pos: pos(117)
								}
								pos: pos(117)
							}
						}),
					]
				}),
				ast.Stmt(ast.FnDecl{
					is_method: true
					receiver:  ast.Parameter{
						name: 's'
						typ:  spin_lock_ptr
						pos:  pos(118)
					}
					name:      'lock'
					typ:       ast.FnType{}
					pos:       pos(119)
				}),
			]
		},
	]
	used := mark_used(files, env)
	main_key := decl_key('main', files[0].stmts[0] as ast.FnDecl, env)
	try_wait_key := decl_key('sync', files[1].stmts[2] as ast.FnDecl, env)
	lock_key := decl_key('sync', files[1].stmts[3] as ast.FnDecl, env)
	assert used[main_key]
	assert used[try_wait_key]
	assert used[lock_key]
}

fn test_mark_used_tracks_embedded_methods_for_interface_conversions() {
	mut env := types.Environment.new()
	files := [
		ast.File{
			mod:   'main'
			name:  'main.v'
			stmts: [
				ast.Stmt(ast.InterfaceDecl{
					name:   'Logger'
					fields: [
						ast.FieldDecl{
							name:                'get_level'
							typ:                 ast.Type(ast.FnType{})
							is_interface_method: true
						},
					]
				}),
				ast.Stmt(ast.GlobalDecl{
					fields: [
						ast.FieldDecl{
							name: 'logger'
							typ:  ast.Type(ast.PointerType{
								base_type: ast.Ident{
									name: 'Logger'
									pos:  pos(60)
								}
							})
						},
					]
				}),
				ast.Stmt(ast.StructDecl{
					name: 'Log'
				}),
				ast.Stmt(ast.StructDecl{
					name:     'ThreadSafeLog'
					embedded: [
						ast.Expr(ast.Ident{
							name: 'Log'
							pos:  pos(61)
						}),
					]
				}),
				ast.Stmt(ast.FnDecl{
					is_method: true
					receiver:  ast.Parameter{
						name: 'l'
						typ:  ast.Ident{
							name: 'Log'
							pos:  pos(62)
						}
						pos:  pos(62)
					}
					name:      'get_level'
					typ:       ast.FnType{}
					pos:       pos(63)
				}),
				ast.Stmt(ast.FnDecl{
					is_method: true
					receiver:  ast.Parameter{
						name: 'l'
						typ:  ast.Ident{
							name: 'Log'
							pos:  pos(64)
						}
						pos:  pos(64)
					}
					name:      'unused'
					typ:       ast.FnType{}
					pos:       pos(65)
				}),
				ast.Stmt(ast.FnDecl{
					name:  'main'
					typ:   ast.FnType{}
					pos:   pos(66)
					stmts: [
						ast.Stmt(ast.AssignStmt{
							lhs: [
								ast.Expr(ast.Ident{
									name: 'logger'
									pos:  pos(67)
								}),
							]
							rhs: [
								ast.Expr(ast.InitExpr{
									typ: ast.Ident{
										name: 'ThreadSafeLog'
										pos:  pos(68)
									}
									pos: pos(68)
								}),
							]
						}),
					]
				}),
			]
		},
	]
	used := mark_used(files, env)
	main_key := decl_key('main', files[0].stmts[6] as ast.FnDecl, env)
	get_level_key := decl_key('main', files[0].stmts[4] as ast.FnDecl, env)
	unused_key := decl_key('main', files[0].stmts[5] as ast.FnDecl, env)
	assert used[main_key]
	assert used[get_level_key]
	assert !used[unused_key]
}

fn test_mark_used_tracks_embedded_interface_methods_for_interface_conversions() {
	mut env := types.Environment.new()
	files := [
		ast.File{
			mod:   'main'
			name:  'main.v'
			stmts: [
				ast.Stmt(ast.InterfaceDecl{
					name:   'Base'
					fields: [
						ast.FieldDecl{
							name:                'base_required'
							typ:                 ast.Type(ast.FnType{})
							is_interface_method: true
						},
					]
				}),
				ast.Stmt(ast.InterfaceDecl{
					name:     'Child'
					embedded: [
						ast.Expr(ast.Ident{
							name: 'Base'
							pos:  pos(90)
						}),
					]
				}),
				ast.Stmt(ast.GlobalDecl{
					fields: [
						ast.FieldDecl{
							name: 'child'
							typ:  ast.Ident{
								name: 'Child'
								pos:  pos(91)
							}
						},
					]
				}),
				ast.Stmt(ast.StructDecl{
					name: 'ConcreteChild'
				}),
				ast.Stmt(ast.FnDecl{
					is_method: true
					receiver:  ast.Parameter{
						name: 'c'
						typ:  ast.Ident{
							name: 'ConcreteChild'
							pos:  pos(92)
						}
						pos:  pos(92)
					}
					name:      'base_required'
					typ:       ast.FnType{}
					pos:       pos(93)
				}),
				ast.Stmt(ast.FnDecl{
					is_method: true
					receiver:  ast.Parameter{
						name: 'c'
						typ:  ast.Ident{
							name: 'ConcreteChild'
							pos:  pos(94)
						}
						pos:  pos(94)
					}
					name:      'unused'
					typ:       ast.FnType{}
					pos:       pos(95)
				}),
				ast.Stmt(ast.FnDecl{
					name:  'main'
					typ:   ast.FnType{}
					pos:   pos(96)
					stmts: [
						ast.Stmt(ast.AssignStmt{
							lhs: [
								ast.Expr(ast.Ident{
									name: 'child'
									pos:  pos(97)
								}),
							]
							rhs: [
								ast.Expr(ast.InitExpr{
									typ: ast.Ident{
										name: 'ConcreteChild'
										pos:  pos(98)
									}
									pos: pos(98)
								}),
							]
						}),
					]
				}),
			]
		},
	]
	used := mark_used(files, env)
	main_key := decl_key('main', files[0].stmts[6] as ast.FnDecl, env)
	base_required_key := decl_key('main', files[0].stmts[4] as ast.FnDecl, env)
	unused_key := decl_key('main', files[0].stmts[5] as ast.FnDecl, env)
	assert used[main_key]
	assert used[base_required_key]
	assert !used[unused_key]
}

fn test_mark_used_tracks_direct_and_embedded_interface_methods_for_interface_conversions() {
	mut env := types.Environment.new()
	files := [
		ast.File{
			mod:   'main'
			name:  'main.v'
			stmts: [
				ast.Stmt(ast.InterfaceDecl{
					name:   'Base'
					fields: [
						ast.FieldDecl{
							name:                'base_required'
							typ:                 ast.Type(ast.FnType{})
							is_interface_method: true
						},
					]
				}),
				ast.Stmt(ast.InterfaceDecl{
					name:     'Child'
					embedded: [
						ast.Expr(ast.Ident{
							name: 'Base'
							pos:  pos(100)
						}),
					]
					fields:   [
						ast.FieldDecl{
							name:                'child_required'
							typ:                 ast.Type(ast.FnType{})
							is_interface_method: true
						},
					]
				}),
				ast.Stmt(ast.GlobalDecl{
					fields: [
						ast.FieldDecl{
							name: 'child'
							typ:  ast.Ident{
								name: 'Child'
								pos:  pos(101)
							}
						},
					]
				}),
				ast.Stmt(ast.StructDecl{
					name: 'ConcreteUnion'
				}),
				ast.Stmt(ast.FnDecl{
					is_method: true
					receiver:  ast.Parameter{
						name: 'c'
						typ:  ast.Ident{
							name: 'ConcreteUnion'
							pos:  pos(102)
						}
						pos:  pos(102)
					}
					name:      'base_required'
					typ:       ast.FnType{}
					pos:       pos(103)
				}),
				ast.Stmt(ast.FnDecl{
					is_method: true
					receiver:  ast.Parameter{
						name: 'c'
						typ:  ast.Ident{
							name: 'ConcreteUnion'
							pos:  pos(104)
						}
						pos:  pos(104)
					}
					name:      'child_required'
					typ:       ast.FnType{}
					pos:       pos(105)
				}),
				ast.Stmt(ast.FnDecl{
					is_method: true
					receiver:  ast.Parameter{
						name: 'c'
						typ:  ast.Ident{
							name: 'ConcreteUnion'
							pos:  pos(106)
						}
						pos:  pos(106)
					}
					name:      'unused'
					typ:       ast.FnType{}
					pos:       pos(107)
				}),
				ast.Stmt(ast.FnDecl{
					name:  'main'
					typ:   ast.FnType{}
					pos:   pos(108)
					stmts: [
						ast.Stmt(ast.AssignStmt{
							lhs: [
								ast.Expr(ast.Ident{
									name: 'child'
									pos:  pos(109)
								}),
							]
							rhs: [
								ast.Expr(ast.InitExpr{
									typ: ast.Ident{
										name: 'ConcreteUnion'
										pos:  pos(110)
									}
									pos: pos(110)
								}),
							]
						}),
					]
				}),
			]
		},
	]
	used := mark_used(files, env)
	main_key := decl_key('main', files[0].stmts[7] as ast.FnDecl, env)
	base_required_key := decl_key('main', files[0].stmts[4] as ast.FnDecl, env)
	child_required_key := decl_key('main', files[0].stmts[5] as ast.FnDecl, env)
	unused_key := decl_key('main', files[0].stmts[6] as ast.FnDecl, env)
	assert used[main_key]
	assert used[base_required_key]
	assert used[child_required_key]
	assert !used[unused_key]
}

fn test_mark_used_does_not_treat_interface_fn_field_as_method() {
	mut env := types.Environment.new()
	files := [
		ast.File{
			mod:   'main'
			name:  'main.v'
			stmts: [
				ast.Stmt(ast.InterfaceDecl{
					name:   'HandlerBox'
					fields: [
						ast.FieldDecl{
							name: 'handler'
							typ:  ast.Type(ast.FnType{
								params: [
									ast.Parameter{
										name: 'value'
										typ:  ast.Ident{
											name: 'int'
											pos:  pos(70)
										}
										pos:  pos(70)
									},
								]
							})
						},
					]
				}),
				ast.Stmt(ast.GlobalDecl{
					fields: [
						ast.FieldDecl{
							name: 'box'
							typ:  ast.Ident{
								name: 'HandlerBox'
								pos:  pos(71)
							}
						},
					]
				}),
				ast.Stmt(ast.StructDecl{
					name: 'ConcreteHandler'
				}),
				ast.Stmt(ast.FnDecl{
					is_method: true
					receiver:  ast.Parameter{
						name: 'h'
						typ:  ast.Ident{
							name: 'ConcreteHandler'
							pos:  pos(72)
						}
						pos:  pos(72)
					}
					name:      'handler'
					typ:       ast.FnType{}
					pos:       pos(73)
				}),
				ast.Stmt(ast.FnDecl{
					name:  'main'
					typ:   ast.FnType{}
					pos:   pos(74)
					stmts: [
						ast.Stmt(ast.AssignStmt{
							lhs: [
								ast.Expr(ast.Ident{
									name: 'box'
									pos:  pos(75)
								}),
							]
							rhs: [
								ast.Expr(ast.InitExpr{
									typ: ast.Ident{
										name: 'ConcreteHandler'
										pos:  pos(76)
									}
									pos: pos(76)
								}),
							]
						}),
					]
				}),
			]
		},
	]
	used := mark_used(files, env)
	handler_key := decl_key('main', files[0].stmts[3] as ast.FnDecl, env)
	main_key := decl_key('main', files[0].stmts[4] as ast.FnDecl, env)
	assert used[main_key]
	assert !used[handler_key]
}

fn test_mark_used_tracks_function_pointers_in_top_level_const_arrays() {
	mut env := types.Environment.new()
	files := [
		ast.File{
			mod:   'main'
			name:  'main.v'
			stmts: [
				ast.Stmt(ast.ConstDecl{
					fields: [
						ast.FieldInit{
							name:  'validators'
							value: ast.ArrayInitExpr{
								exprs: [
									ast.Expr(ast.Ident{
										name: 'accept'
										pos:  pos(51)
									}),
								]
								pos:   pos(50)
							}
						},
					]
				}),
				ast.Stmt(ast.FnDecl{
					name:  'main'
					typ:   ast.FnType{}
					pos:   pos(52)
					stmts: []ast.Stmt{}
				}),
				ast.Stmt(ast.FnDecl{
					name: 'accept'
					typ:  ast.FnType{}
					pos:  pos(53)
				}),
				ast.Stmt(ast.FnDecl{
					name: 'dead'
					typ:  ast.FnType{}
					pos:  pos(54)
				}),
			]
		},
	]
	used := mark_used(files, env)
	main_key := decl_key('main', files[0].stmts[1] as ast.FnDecl, env)
	accept_key := decl_key('main', files[0].stmts[2] as ast.FnDecl, env)
	dead_key := decl_key('main', files[0].stmts[3] as ast.FnDecl, env)
	assert used[main_key]
	assert used[accept_key]
	assert !used[dead_key]
}

fn test_mark_used_tracks_module_function_values_as_call_args() {
	mut env := types.Environment.new()
	callback_type := ast.Type(ast.FnType{
		params:      [
			ast.Parameter{
				typ: ast.Ident{
					name: 'string'
					pos:  pos(70)
				}
			},
		]
		return_type: ast.Ident{
			name: 'string'
			pos:  pos(71)
		}
	})
	files := [
		ast.File{
			mod:     'main'
			name:    'main.v'
			imports: [
				ast.ImportStmt{
					name: 'term'
				},
			]
			stmts:   [
				ast.Stmt(ast.FnDecl{
					name:  'main'
					typ:   ast.FnType{}
					pos:   pos(72)
					stmts: [
						ast.Stmt(ast.ExprStmt{
							expr: ast.CallExpr{
								lhs:  ast.SelectorExpr{
									lhs: ast.Ident{
										name: 'term'
										pos:  pos(73)
									}
									rhs: ast.Ident{
										name: 'colorize'
										pos:  pos(74)
									}
									pos: pos(74)
								}
								args: [
									ast.Expr(ast.SelectorExpr{
										lhs: ast.Ident{
											name: 'term'
											pos:  pos(75)
										}
										rhs: ast.Ident{
											name: 'green'
											pos:  pos(76)
										}
										pos: pos(76)
									}),
									ast.Expr(ast.BasicLiteral{
										kind:  .string
										value: 'ok'
										pos:   pos(77)
									}),
								]
								pos:  pos(74)
							}
						}),
					]
				}),
			]
		},
		ast.File{
			mod:   'term'
			name:  'term.v'
			stmts: [
				ast.Stmt(ast.FnDecl{
					name: 'colorize'
					typ:  ast.FnType{
						params:      [
							ast.Parameter{
								name: 'cfn'
								typ:  callback_type
								pos:  pos(78)
							},
							ast.Parameter{
								name: 's'
								typ:  ast.Ident{
									name: 'string'
									pos:  pos(79)
								}
								pos:  pos(79)
							},
						]
						return_type: ast.Ident{
							name: 'string'
							pos:  pos(80)
						}
					}
					pos:  pos(81)
				}),
				ast.Stmt(ast.FnDecl{
					name: 'green'
					typ:  ast.FnType{}
					pos:  pos(82)
				}),
				ast.Stmt(ast.FnDecl{
					name: 'red'
					typ:  ast.FnType{}
					pos:  pos(83)
				}),
			]
		},
	]
	used := mark_used(files, env)
	main_key := decl_key('main', files[0].stmts[0] as ast.FnDecl, env)
	colorize_key := decl_key('term', files[1].stmts[0] as ast.FnDecl, env)
	green_key := decl_key('term', files[1].stmts[1] as ast.FnDecl, env)
	red_key := decl_key('term', files[1].stmts[2] as ast.FnDecl, env)
	assert used[main_key]
	assert used[colorize_key]
	assert used[green_key]
	assert !used[red_key]
}

fn test_mark_used_keeps_all_functions_when_no_entry_root_exists() {
	mut env := types.Environment.new()
	files := [
		ast.File{
			mod:   'mylib'
			name:  'lib.v'
			stmts: [
				ast.Stmt(ast.FnDecl{
					name: 'a'
					typ:  ast.FnType{}
					pos:  pos(21)
				}),
				ast.Stmt(ast.FnDecl{
					name: 'b'
					typ:  ast.FnType{}
					pos:  pos(22)
				}),
			]
		},
	]
	used := mark_used(files, env)
	a_key := decl_key('mylib', files[0].stmts[0] as ast.FnDecl, env)
	b_key := decl_key('mylib', files[0].stmts[1] as ast.FnDecl, env)
	assert used[a_key]
	assert used[b_key]
}
