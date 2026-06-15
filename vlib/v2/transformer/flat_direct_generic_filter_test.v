module transformer

import v2.ast
import v2.pref as vpref
import v2.types

fn test_flat_direct_filter_skips_open_generic_fn_decls_only() {
	env := &types.Environment{}
	t := &Transformer{
		pref: &vpref.Preferences{}
		env:  unsafe { env }
	}
	file := ast.File{
		mod:            'main'
		name:           'generic_filter.v'
		selector_names: {
			101: 'main.Counter.value'
			202: 'main.Queue.is_empty'
			303: 'datatypes.Queue.is_empty'
		}
		stmts:          [
			ast.Stmt(ast.FnDecl{
				name:      'BSTree__insert'
				is_method: true
				receiver:  ast.Parameter{
					name: 'tree'
					typ:  ast.Type(ast.GenericType{
						name:   ast.Ident{
							name: 'BSTree'
						}
						params: [
							ast.Expr(ast.Ident{
								name: 'T'
							}),
						]
					})
				}
			}),
			ast.Stmt(ast.FnDecl{
				name:      'Queue__is_empty'
				is_method: true
				receiver:  ast.Parameter{
					name: 'queue'
					typ:  ast.Type(ast.GenericType{
						name:   ast.Ident{
							name: 'Queue'
						}
						params: [
							ast.Expr(ast.Ident{
								name: 'T'
							}),
						]
					})
				}
			}),
			ast.Stmt(ast.FnDecl{
				name:      'datatypes__Queue__is_empty'
				is_method: true
				receiver:  ast.Parameter{
					name: 'queue'
					typ:  ast.Type(ast.GenericType{
						name:   ast.Ident{
							name: 'datatypes__Queue'
						}
						params: [
							ast.Expr(ast.Ident{
								name: 'T'
							}),
						]
					})
				}
			}),
			ast.Stmt(ast.FnDecl{
				name:      'Queue_T_Array_string__is_empty'
				is_method: true
				receiver:  ast.Parameter{
					name: 'queue'
					typ:  ast.Ident{
						name: 'Queue_T_Array_string'
					}
				}
			}),
			ast.Stmt(ast.FnDecl{
				name:      'Counter__value'
				is_method: true
				receiver:  ast.Parameter{
					name: 'counter'
					typ:  ast.Ident{
						name: 'Counter'
					}
				}
			}),
			ast.Stmt(ast.FnDecl{
				name: 'identity'
				typ:  ast.FnType{
					generic_params: [
						ast.Expr(ast.Ident{
							name: 'T'
						}),
					]
				}
			}),
			ast.Stmt(ast.FnDecl{
				name: 'main'
			}),
		]
	}

	filtered := t.file_without_open_generic_fn_decls(file)

	assert filtered.selector_names[101] == 'main.Counter.value'
	assert filtered.selector_names[202] == 'main.Queue.is_empty'
	assert filtered.selector_names[303] == 'datatypes.Queue.is_empty'
	assert filtered.selector_names.len == 3
	mut kept_names := []string{}
	for stmt in filtered.stmts {
		assert stmt is ast.FnDecl
		kept_names << (stmt as ast.FnDecl).name
	}
	assert kept_names == ['Queue_T_Array_string__is_empty', 'Counter__value', 'main']
	assert 'Queue__is_empty' !in kept_names
	assert 'datatypes__Queue__is_empty' !in kept_names
}

fn test_flat_input_direct_skips_open_generic_fn_decls_only() {
	env := &types.Environment{}
	mut t := Transformer{
		pref: &vpref.Preferences{}
		env:  unsafe { env }
	}
	file := ast.File{
		mod:   'main'
		name:  'generic_filter_flat_input.v'
		stmts: [
			ast.Stmt(ast.FnDecl{
				name:      'Queue__is_empty'
				is_method: true
				receiver:  ast.Parameter{
					name: 'queue'
					typ:  ast.Type(ast.GenericType{
						name:   ast.Ident{
							name: 'Queue'
						}
						params: [
							ast.Expr(ast.Ident{
								name: 'T'
							}),
						]
					})
				}
			}),
			ast.Stmt(ast.FnDecl{
				name:      'datatypes__Queue__is_empty'
				is_method: true
				receiver:  ast.Parameter{
					name: 'queue'
					typ:  ast.Type(ast.GenericType{
						name:   ast.Ident{
							name: 'datatypes__Queue'
						}
						params: [
							ast.Expr(ast.Ident{
								name: 'T'
							}),
						]
					})
				}
			}),
			ast.Stmt(ast.FnDecl{
				name: 'identity'
				typ:  ast.FnType{
					generic_params: [
						ast.Expr(ast.Ident{
							name: 'T'
						}),
					]
				}
			}),
			ast.Stmt(ast.FnDecl{
				name:      'Queue_T_Array_string__is_empty'
				is_method: true
				receiver:  ast.Parameter{
					name: 'queue'
					typ:  ast.Ident{
						name: 'Queue_T_Array_string'
					}
				}
			}),
			ast.Stmt(ast.FnDecl{
				name: 'main'
			}),
		]
	}
	flat := ast.flatten_files([file])
	out_files := t.transform_flat_to_flat_direct(&flat, []).to_files()
	mut fn_names := []string{}
	for out_file in out_files {
		for stmt in out_file.stmts {
			if stmt is ast.FnDecl {
				fn_names << stmt.name
			}
		}
	}

	assert 'Queue_T_Array_string__is_empty' in fn_names
	assert 'main' in fn_names
	assert 'Queue__is_empty' !in fn_names
	assert 'datatypes__Queue__is_empty' !in fn_names
	assert 'identity' !in fn_names
}
