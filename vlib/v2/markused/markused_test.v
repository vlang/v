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
