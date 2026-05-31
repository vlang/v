// vtest build: macos
module cleanc

import os
import v2.ast
import v2.parser
import v2.pref as vpref
import v2.token
import v2.transformer
import v2.types

fn cleanc_csrc_for_test_source(name string, source string) string {
	tmp_file := '/tmp/v2_cleanc_${name}_${os.getpid()}.v'
	os.write_file(tmp_file, source) or { panic('failed to write temp file') }
	defer {
		os.rm(tmp_file) or {}
	}
	prefs := &vpref.Preferences{
		backend:     .cleanc
		no_parallel: true
	}
	mut file_set := token.FileSet.new()
	mut par := parser.Parser.new(prefs)
	files := par.parse_files([tmp_file], mut file_set)
	mut env := types.Environment.new()
	mut checker := types.Checker.new(prefs, file_set, env)
	checker.check_files(files)
	mut trans := transformer.Transformer.new_with_pref(env, prefs)
	trans.set_file_set(file_set)
	transformed_files := trans.transform_files(files)
	mut gen := Gen.new_with_env_and_pref(transformed_files, env, prefs)
	return gen.gen()
}

fn test_c_string_literal_content_to_c_single_line() {
	out := c_string_literal_content_to_c('hello')
	assert out == '"hello"'
}

fn test_c_string_literal_content_to_c_multiline() {
	out := c_string_literal_content_to_c('hello\nworld')
	assert out == '"hello\\n"\n"world"'
}

fn test_c_string_literal_content_to_c_trailing_newline() {
	out := c_string_literal_content_to_c('hello\n')
	assert out == '"hello\\n"\n""'
}

fn test_c_string_literal_content_to_c_escapes_quote() {
	out := c_string_literal_content_to_c('say "hello"')
	assert out == '"say \\"hello\\""'
}

fn test_c_string_literal_content_to_c_preserves_percent_placeholders() {
	out := c_string_literal_content_to_c('"%s"')
	assert out == '"\\"%s\\""'
}

fn test_c_string_literal_content_to_c_splits_hex_escape_before_hex_digit() {
	out := c_string_literal_content_to_c(r'\x0c8')
	assert out == '"\\x0c""8"'
}

fn test_const_shadowing_declared_fn_uses_renamed_storage() {
	decl := ast.ConstDecl{
		fields: [
			ast.FieldInit{
				name:  'test_strings'
				value: ast.Expr(ast.BasicLiteral{
					kind:  .number
					value: '1'
				})
			},
		]
	}

	mut g := Gen.new([])
	g.declared_fn_names['test_strings'] = true
	g.gen_const_decl(decl)
	csrc := g.sb.str()
	assert csrc.contains('static const int __v_const_test_strings = 1;')
	assert !csrc.contains('static const int test_strings = 1;')

	mut expr_g := Gen.new([])
	expr_g.const_c_names['test_strings'] = '__v_const_test_strings'
	expr_g.expr(ast.Expr(ast.Ident{
		name: 'test_strings'
	}))
	assert expr_g.sb.str() == '__v_const_test_strings'

	mut extern_g := Gen.new([])
	extern_g.declared_fn_names['test_strings'] = true
	extern_g.gen_const_decl_extern(decl)
	extern_src := extern_g.sb.str()
	assert extern_src.contains('static const int __v_const_test_strings = 1;')
	assert !extern_src.contains('static const int test_strings = 1;')
}

fn test_local_variable_uses_escaped_c_keyword_name() {
	mut g := Gen.new([])
	g.gen_assign_stmt(ast.AssignStmt{
		op:  .decl_assign
		lhs: [ast.Expr(ast.Ident{
			name: 'signed'
		})]
		rhs: [ast.Expr(ast.BasicLiteral{
			kind:  .key_true
			value: 'true'
		})]
	})
	assert g.sb.str().contains('bool _signed = true;')

	mut expr_g := Gen.new([])
	expr_g.runtime_local_types['signed'] = 'bool'
	expr_g.expr(ast.Expr(ast.Ident{
		name: 'signed'
	}))
	assert expr_g.sb.str() == '_signed'
	assert c_fn_param_name('unix', 0) == '_unix'
}

fn test_local_variable_shadowing_enum_member_is_addressable() {
	mut g := Gen.new([])
	g.runtime_local_types['name'] = 'string'
	g.enum_value_to_enum['name'] = 'token__Token'
	g.fn_param_is_ptr['map__delete'] = [true, true]
	g.fn_param_types['map__delete'] = ['map*', 'void*']
	g.gen_call_arg('map__delete', 1, ast.Expr(ast.Ident{
		name: 'name'
	}))
	assert g.sb.str() == '&name'
}

fn test_generic_struct_field_dependency_uses_concrete_bindings() {
	csrc := cleanc_csrc_for_test_source('generic_struct_field_dependency', '
module json2

struct Node[T] {
mut:
	value T
	next  &Node[T] = unsafe { nil }
}

struct ValueInfo {
	position int
}

struct Decoder {
	values_info  LinkedList[ValueInfo]
	current_node &Node[ValueInfo] = unsafe { nil }
}

struct LinkedList[T] {
mut:
	head &Node[T] = unsafe { nil }
	tail &Node[T] = unsafe { nil }
	len  int
}
')
	linked_list_pos := csrc.index('struct json2__LinkedList {') or {
		panic('missing LinkedList body')
	}
	decoder_pos := csrc.index('struct json2__Decoder {') or { panic('missing Decoder body') }
	assert linked_list_pos < decoder_pos
	assert csrc.contains('json2__LinkedList values_info;')
}

fn test_generic_pointer_empty_init_uses_null_pointer() {
	mut g := Gen.new([])
	g.active_generic_types['V'] = types.Type(types.Pointer{
		base_type: types.Type(types.NamedType('string'))
	})
	g.gen_init_expr(ast.InitExpr{
		typ: ast.Ident{
			name: 'V'
		}
	})
	assert g.sb.str() == '0'
}

fn test_decl_assign_unwraps_nested_static_modifier_lhs() {
	mut g := Gen.new([])
	static_lhs := ast.Expr(ast.ModifierExpr{
		kind: .key_mut
		expr: ast.ModifierExpr{
			kind: .key_static
			expr: ast.Ident{
				name: 'ptimers'
			}
		}
	})
	g.gen_assign_stmt(ast.AssignStmt{
		op:  .decl_assign
		lhs: [static_lhs]
		rhs: [
			ast.Expr(ast.CastExpr{
				typ:  ast.Ident{
					name: 'Timers*'
				}
				expr: ast.BasicLiteral{
					kind:  .number
					value: '0'
				}
			}),
		]
	})
	g.gen_assign_stmt(ast.AssignStmt{
		op:  .assign
		lhs: [
			ast.Expr(ast.Ident{
				name: 'ptimers'
			}),
		]
		rhs: [
			ast.Expr(ast.Ident{
				name: 'p'
			}),
		]
	})
	csrc := g.sb.str()
	assert csrc.contains('static Timers* ptimers = ')
	assert csrc.contains('ptimers = p;')
	assert !csrc.contains('Timers* ptimers = p;')
}

fn test_receiver_panic_method_call_keeps_method_name() {
	csrc := cleanc_csrc_for_test_source('receiver_panic_method_call', '
struct Table {}

fn (t &Table) panic(message string) {
	_ = message
}

fn (t &Table) register() {
	t.panic("duplicate")
}
')
	assert csrc.contains('Table__panic(t, (string){.str = "duplicate"')
	assert !csrc.contains('v_panic((*t),')
	assert !csrc.contains('v_panic(t,')
}

fn test_transformed_map_temps_keep_position_types_when_names_repeat() {
	csrc := cleanc_csrc_for_test_source('map_temp_position_types', '
struct StructField {
	name string
}

fn (f StructField) equals(other StructField) bool {
	return f.name == other.name
}

fn f(fields []StructField) {
	mut field_map := map[string]StructField{}
	mut field_usages := map[string]int{}
	for field in fields {
		if field.name !in field_map {
			field_map[field.name] = field
			field_usages[field.name]++
		} else if field.equals(field_map[field.name]) {
			field_usages[field.name]++
		}
	}
}
')
	for segment in csrc.split(';') {
		if string_idx := segment.index('string _or_t') {
			tail := segment[string_idx..]
			assert !tail.contains('= ((int)'), segment
			assert !tail.contains('= ((bool)'), segment
		}
		if field_idx := segment.index('StructField _or_t') {
			tail := segment[field_idx..]
			assert !tail.contains('= ((int)'), segment
		}
	}
	assert csrc.contains('map__set(&field_usages')
}

fn test_module_storage_selector_uses_declaring_module_prefix() {
	mut g := Gen.new([])
	g.cur_module = 'checker'
	g.cur_import_modules['ast'] = 'ast'
	g.expr(ast.Expr(ast.SelectorExpr{
		lhs: ast.Expr(ast.Ident{
			name: 'ast'
		})
		rhs: ast.Ident{
			name: 'global_table'
		}
	}))
	assert g.sb.str() == 'ast__global_table'

	mut local_g := Gen.new([])
	local_g.cur_module = 'checker'
	local_g.cur_import_modules['ast'] = 'ast'
	local_g.module_storage_vars['ast__global_table'] = 'ast'
	local_g.runtime_local_types['global_table'] = 'int'
	local_g.expr(ast.Expr(ast.Ident{
		name: 'global_table'
	}))
	assert local_g.sb.str() == 'global_table'
}

fn test_module_storage_selector_type_resolves_receiver_methods() {
	mut g := Gen.new([])
	g.cur_module = 'transformer'
	g.cur_import_modules['ast'] = 'ast'
	g.global_var_types['ast__global_table'] = 'ast__Table*'
	g.fn_return_types['ast__Table__type_to_str'] = 'string'
	g.fn_param_is_ptr['ast__Table__type_to_str'] = [true, false]
	g.fn_param_types['ast__Table__type_to_str'] = ['ast__Table*', 'ast__Type']
	g.runtime_local_types['typ'] = 'ast__Type'
	g.call_expr(ast.Expr(ast.SelectorExpr{
		lhs: ast.Expr(ast.SelectorExpr{
			lhs: ast.Expr(ast.Ident{
				name: 'ast'
			})
			rhs: ast.Ident{
				name: 'global_table'
			}
		})
		rhs: ast.Ident{
			name: 'type_to_str'
		}
	}), [
		ast.Expr(ast.Ident{
			name: 'typ'
		}),
	])
	out := g.sb.str()
	assert out.contains('ast__Table__type_to_str(ast__global_table, typ)'), out
	assert !out.contains('int__type_to_str'), out
}

fn test_generic_arg_or_index_field_selector_prefers_indexable_field_type() {
	u8_type := types.builtin_type('u8') or { panic('missing u8 type') }
	embedded_file_type := types.Type(types.Struct{
		name:   'EmbeddedFile'
		fields: [
			types.Field{
				name: 'bytes'
				typ:  types.Type(types.Array{
					elem_type: u8_type
				})
			},
		]
	})
	mut env := types.Environment.new()
	mut scope := types.new_scope(unsafe { nil })
	scope.insert('emfile', embedded_file_type)
	env.set_fn_scope('main', 'f', scope)
	mut g := Gen.new_with_env([], env)
	g.cur_module = 'main'
	g.cur_fn_name = 'f'
	g.runtime_local_types['emfile'] = 'EmbeddedFile'
	g.struct_field_types['EmbeddedFile.bytes'] = 'Array_u8'
	g.fn_return_types['string__bytes'] = 'Array_u8'

	ambiguous := ast.GenericArgOrIndexExpr{
		lhs:  ast.SelectorExpr{
			lhs: ast.Ident{
				name: 'emfile'
			}
			rhs: ast.Ident{
				name: 'bytes'
			}
		}
		expr: ast.BasicLiteral{
			kind:  .number
			value: '0'
		}
	}
	selector := ambiguous.lhs as ast.SelectorExpr
	assert g.generic_arg_or_index_expr_is_index(ambiguous), g.selector_field_type(selector)
	g.expr(ast.Expr(ambiguous))

	csrc := g.sb.str()
	assert csrc.contains('emfile.bytes'), csrc
	assert csrc.contains('.data'), csrc
	assert !csrc.contains('string__bytes'), csrc
}

fn test_return_mut_param_value_derefs_pointer_param() {
	mut g := Gen.new([])
	g.cur_fn_ret_type = 'Array_ast__Stmt'
	g.runtime_local_types['nodes'] = 'Array_ast__Stmt*'
	g.cur_fn_mut_params['nodes'] = true
	g.gen_stmt(ast.ReturnStmt{
		exprs: [ast.Expr(ast.Ident{
			name: 'nodes'
		})]
	})
	assert g.sb.str().contains('return (*nodes);')
}

fn test_return_mut_sumtype_param_derefs_before_sumtype_cast() {
	mut g := Gen.new([])
	g.cur_fn_ret_type = 'ast__Stmt'
	g.sum_type_variants['ast__Stmt'] = ['ast__ExprStmt']
	g.runtime_local_types['node'] = 'ast__Stmt*'
	g.cur_fn_mut_params['node'] = true
	g.gen_stmt(ast.ReturnStmt{
		exprs: [ast.Expr(ast.Ident{
			name: 'node'
		})]
	})
	assert g.sb.str().contains('return (*node);')
	assert !g.sb.str().contains('((ast__Stmt)(node))')
}

fn test_declared_function_pointer_call_return_type_from_runtime_alias() {
	tmp_file := '/tmp/v2_cleanc_fn_ptr_alias_${os.getpid()}.v'
	os.write_file(tmp_file, 'module pool\npub type ThreadCB = fn () voidptr\n') or {
		panic('failed to write temp file')
	}
	defer {
		os.rm(tmp_file) or {}
	}
	prefs := &vpref.Preferences{}
	mut file_set := token.FileSet.new()
	mut par := parser.Parser.new(prefs)
	files := par.parse_files([tmp_file], mut file_set)
	mut env := types.Environment.new()
	mut checker := types.Checker.new(prefs, file_set, env)
	checker.check_files(files)
	mut g := Gen.new_with_env([], env)
	g.cur_module = 'pool'
	g.runtime_decl_types['cb'] = 'pool__ThreadCB'
	g.gen_assign_stmt(ast.AssignStmt{
		op:  .decl_assign
		lhs: [ast.Expr(ast.Ident{
			name: 'res'
		})]
		rhs: [
			ast.Expr(ast.CallExpr{
				lhs: ast.Expr(ast.Ident{
					name: 'cb'
				})
			}),
		]
	})
	csrc := g.sb.str()
	assert !csrc.contains('int res = cb(')
	assert csrc.contains('voidptr res = cb(') || csrc.contains('void* res = cb(')
}

fn test_function_pointer_struct_field_call_uses_field_selector() {
	tmp_file := '/tmp/v2_cleanc_fn_ptr_field_alias_${os.getpid()}.v'
	os.write_file(tmp_file, 'module main\ntype LabelFn = fn (string, voidptr) string\n') or {
		panic('failed to write temp file')
	}
	defer {
		os.rm(tmp_file) or {}
	}
	prefs := &vpref.Preferences{}
	mut file_set := token.FileSet.new()
	mut par := parser.Parser.new(prefs)
	files := par.parse_files([tmp_file], mut file_set)
	mut env := types.Environment.new()
	mut checker := types.Checker.new(prefs, file_set, env)
	checker.check_files(files)
	env.set_expr_type(97, types.Type(types.FnType{}))
	mut g := Gen.new_with_env([], env)
	g.cur_module = 'main'
	g.struct_field_types['Config.cb'] = 'LabelFn'
	g.struct_field_types['Config.ctx'] = 'voidptr'
	g.remember_runtime_local_type('cfg', 'Config')
	g.remember_runtime_local_type('label', 'string')
	g.call_expr(ast.Expr(ast.SelectorExpr{
		lhs: ast.Expr(ast.Ident{
			name: 'cfg'
		})
		rhs: ast.Ident{
			name: 'cb'
		}
		pos: token.Pos{
			id: 97
		}
	}), [
		ast.Expr(ast.Ident{
			name: 'label'
		}),
		ast.Expr(ast.SelectorExpr{
			lhs: ast.Expr(ast.Ident{
				name: 'cfg'
			})
			rhs: ast.Ident{
				name: 'ctx'
			}
		}),
	])
	csrc := g.sb.str()
	assert csrc == 'cfg.cb(label, cfg.ctx)'
	assert !csrc.contains('Config__cb')
}

fn test_static_constructor_call_uses_syntactic_type_receiver_without_emitted_type() {
	mut g := Gen.new([])
	g.cur_module = 'searcher'
	g.fn_return_types['searcher__ByteSliceReader__new'] = 'searcher__ByteSliceReader'
	g.fn_param_types['searcher__ByteSliceReader__new'] = ['string']
	g.fn_param_is_ptr['searcher__ByteSliceReader__new'] = [false]
	g.fn_return_types['searcher__LineBufferReader__new'] = 'searcher__LineBufferReader'
	g.fn_param_types['searcher__LineBufferReader__new'] = [
		'searcher__ByteSliceReader*',
		'searcher__LineBuffer*',
	]
	g.fn_param_is_ptr['searcher__LineBufferReader__new'] = [true, true]
	g.remember_runtime_local_type('source', 'searcher__ByteSliceReader')
	g.remember_runtime_local_type('linebuf', 'searcher__LineBuffer')
	g.gen_assign_stmt(ast.AssignStmt{
		op:  .decl_assign
		lhs: [ast.Expr(ast.Ident{
			name: 'rdr'
		})]
		rhs: [
			ast.Expr(ast.CallExpr{
				lhs:  ast.Expr(ast.SelectorExpr{
					lhs: ast.Expr(ast.Ident{
						name: 'LineBufferReader'
					})
					rhs: ast.Ident{
						name: 'new'
					}
				})
				args: [
					ast.Expr(ast.PrefixExpr{
						op:   .amp
						expr: ast.Expr(ast.Ident{
							name: 'source'
						})
					}),
					ast.Expr(ast.PrefixExpr{
						op:   .amp
						expr: ast.Expr(ast.Ident{
							name: 'linebuf'
						})
					}),
				]
			}),
		]
	})
	csrc := g.sb.str()
	assert csrc.contains('searcher__LineBufferReader rdr = searcher__LineBufferReader__new('), csrc

	assert !csrc.contains('searcher__ByteSliceReader rdr = searcher__ByteSliceReader__new('), csrc
}

fn test_mangled_static_constructor_call_is_not_rewritten_from_first_arg_receiver() {
	mut g := Gen.new([])
	g.cur_module = 'searcher'
	g.fn_return_types['searcher__ByteSliceReader__new'] = 'searcher__ByteSliceReader'
	g.fn_param_types['searcher__ByteSliceReader__new'] = ['string']
	g.fn_param_is_ptr['searcher__ByteSliceReader__new'] = [false]
	g.fn_return_types['searcher__LineBufferReader__new'] = 'searcher__LineBufferReader'
	g.fn_param_types['searcher__LineBufferReader__new'] = [
		'io__Reader*',
		'searcher__LineBuffer*',
	]
	g.fn_param_is_ptr['searcher__LineBufferReader__new'] = [true, true]
	g.remember_runtime_local_type('source', 'searcher__ByteSliceReader')
	g.remember_runtime_local_type('linebuf', 'searcher__LineBuffer')
	g.gen_assign_stmt(ast.AssignStmt{
		op:  .decl_assign
		lhs: [ast.Expr(ast.Ident{
			name: 'rdr'
		})]
		rhs: [
			ast.Expr(ast.CallExpr{
				lhs:  ast.Expr(ast.Ident{
					name: 'searcher__LineBufferReader__new'
				})
				args: [
					ast.Expr(ast.PrefixExpr{
						op:   .amp
						expr: ast.Expr(ast.Ident{
							name: 'source'
						})
					}),
					ast.Expr(ast.PrefixExpr{
						op:   .amp
						expr: ast.Expr(ast.Ident{
							name: 'linebuf'
						})
					}),
				]
			}),
		]
	})
	csrc := g.sb.str()
	assert csrc.contains('searcher__LineBufferReader rdr = searcher__LineBufferReader__new('), csrc
	assert !csrc.contains('searcher__ByteSliceReader rdr = searcher__ByteSliceReader__new('), csrc
}

fn test_statement_else_unsafe_expr_emits_nested_if_guard_stmts() {
	mut g := Gen.new([])
	outer_if := ast.IfExpr{
		cond:      ast.Expr(ast.Ident{
			name: 'outer_ok'
		})
		stmts:     [
			ast.Stmt(ast.ExprStmt{
				expr: ast.Expr(ast.CallExpr{
					lhs: ast.Expr(ast.Ident{
						name: 'outer_call'
					})
				})
			}),
		]
		else_expr: ast.Expr(ast.UnsafeExpr{
			stmts: [
				ast.Stmt(ast.AssignStmt{
					op:  .decl_assign
					lhs: [ast.Expr(ast.Ident{
						name: '_opt_t'
					})]
					rhs: [ast.Expr(ast.BasicLiteral{
						kind:  .number
						value: '1'
					})]
				}),
				ast.Stmt(ast.ExprStmt{
					expr: ast.Expr(ast.IfExpr{
						cond:  ast.Expr(ast.Ident{
							name: 'inner_ok'
						})
						stmts: [
							ast.Stmt(ast.ExprStmt{
								expr: ast.Expr(ast.CallExpr{
									lhs: ast.Expr(ast.Ident{
										name: 'inner_call'
									})
								})
							}),
						]
					})
				}),
			]
		})
	}
	g.gen_if_expr_stmt(&outer_if)
	csrc := g.sb.str()
	assert csrc.contains('else {'), csrc
	assert csrc.contains('int _opt_t = 1;'), csrc
	assert csrc.contains('if (inner_ok)'), csrc
	assert csrc.contains('inner_call();'), csrc
}

fn test_interface_pointer_data_field_selector_uses_pointer_separator() {
	mut gen := Gen.new([])
	gen.interface_data_fields['IResolverType'] = [
		InterfaceDataFieldInfo{
			name:   'file'
			c_type: 'ast__File*'
		},
	]
	gen.struct_field_types['TypeResolver.resolver'] = 'IResolverType'
	gen.struct_field_types['ast__File.path'] = 'string'
	gen.remember_runtime_local_type('t', 'TypeResolver*')
	file_expr := ast.Expr(ast.SelectorExpr{
		lhs: ast.Expr(ast.SelectorExpr{
			lhs: ast.Expr(ast.Ident{
				name: 't'
			})
			rhs: ast.Ident{
				name: 'resolver'
			}
		})
		rhs: ast.Ident{
			name: 'file'
		}
	})
	assert gen.selector_field_type(file_expr as ast.SelectorExpr) == 'ast__File*'
	assert gen.expr_is_pointer(file_expr)

	gen.expr(ast.Expr(ast.SelectorExpr{
		lhs: file_expr
		rhs: ast.Ident{
			name: 'path'
		}
	}))
	out := gen.sb.str()
	assert out.contains('))->path')
	assert !out.contains(')).path')
}

fn test_option_return_auto_derefs_pointer_value_expr() {
	mut gen := Gen.new([])
	gen.cur_fn_ret_type = '_option_ast__Fn'
	gen.remember_runtime_local_type('method', 'ast__Fn*')
	gen.gen_stmt(ast.Stmt(ast.ReturnStmt{
		exprs: [
			ast.Expr(ast.Ident{
				name: 'method'
			}),
		]
	}))
	out := gen.sb.str()
	assert out.contains('ast__Fn _val = (*method);')
	assert !out.contains('ast__Fn _val = method;')
}

fn test_decl_temp_auto_derefs_pointer_rhs_for_value_scope_type() {
	mut gen := Gen.new([])
	mut scope := types.new_scope(unsafe { nil })
	scope.insert_or_update('_defer_t1', types.Type(types.SumType{
		name: 'ast__Stmt'
	}))
	gen.cur_fn_scope = scope
	gen.remember_runtime_local_type('node', 'ast__Stmt*')
	gen.gen_assign_stmt(ast.AssignStmt{
		op:  .decl_assign
		lhs: [ast.Expr(ast.Ident{
			name: '_defer_t1'
		})]
		rhs: [ast.Expr(ast.Ident{
			name: 'node'
		})]
	})
	out := gen.sb.str()
	assert out.contains('ast__Stmt _defer_t1 = (*node);')
	assert !out.contains('ast__Stmt _defer_t1 = node;')
}

fn test_value_return_auto_derefs_pointer_expr() {
	mut gen := Gen.new([])
	gen.cur_fn_ret_type = 'ast__Stmt'
	gen.remember_runtime_local_type('_defer_t1', 'ast__Stmt*')
	gen.gen_stmt(ast.Stmt(ast.ReturnStmt{
		exprs: [
			ast.Expr(ast.Ident{
				name: '_defer_t1'
			}),
		]
	}))
	out := gen.sb.str()
	assert out.contains('return (*_defer_t1);')
	assert !out.contains('return ((ast__Stmt)(_defer_t1));')
}

fn test_known_c_typedef_selectors_do_not_emit_struct_prefix() {
	mut g := Gen.new([])
	for name in ['atomic_uintptr_t', 'pthread_condattr_t', 'pthread_rwlockattr_t'] {
		c_type := g.expr_type_to_c(ast.Expr(ast.SelectorExpr{
			lhs: ast.Expr(ast.Ident{
				name: 'C'
			})
			rhs: ast.Ident{
				name: name
			}
		}))
		assert c_type == name
	}
}

fn test_signature_generic_struct_forward_typedefs_use_metadata() {
	mut g := Gen.new([])
	g.fn_return_types['veb__encode_gzip_T_GitHubContributor'] = 'veb__MiddlewareOptions_T_GitHubContributor'
	g.fn_return_types['math__div_T_ast_Fileptr'] = 'math__DivResult_T_ast__Fileptr'
	g.fn_param_types['use_middleware'] = ['veb__MiddlewareOptions_T_GitHubUser*']
	g.emit_forward_typedefs_for_signature_types()
	csrc := g.sb.str()
	assert csrc.contains('typedef struct veb__MiddlewareOptions_T_GitHubContributor veb__MiddlewareOptions_T_GitHubContributor;')
	assert csrc.contains('typedef struct veb__MiddlewareOptions_T_GitHubUser veb__MiddlewareOptions_T_GitHubUser;')
	assert csrc.contains('typedef struct math__DivResult_T_ast__Fileptr math__DivResult_T_ast__Fileptr;')
	assert !csrc.contains('math__DivResult_T_ast__File*')
	assert csrc.count('typedef struct veb__MiddlewareOptions_T_GitHubUser veb__MiddlewareOptions_T_GitHubUser;') == 1
}

fn test_fn_head_emits_forward_typedef_for_late_generic_receiver() {
	mut g := Gen.new([])
	g.cur_module = 'printer'
	g.active_generic_types['W'] = types.Type(types.string_)
	count_method := ast.FnDecl{
		name:      'count'
		is_method: true
		receiver:  ast.Parameter{
			name: 'w'
			typ:  ast.Expr(ast.Type(ast.GenericType{
				name:   ast.Expr(ast.SelectorExpr{
					lhs: ast.Expr(ast.Ident{
						name: 'printer'
					})
					rhs: ast.Ident{
						name: 'CounterWriter'
					}
				})
				params: [
					ast.Expr(ast.Ident{
						name: 'W'
					}),
				]
			}))
		}
		typ:       ast.FnType{
			return_type: ast.Expr(ast.Ident{
				name: 'u64'
			})
		}
		language:  .v
	}
	fn_name := 'printer__CounterWriter_T_string__count'
	g.fn_param_types[fn_name] = ['printer__CounterWriter_T_string']
	g.gen_fn_head_with_name(count_method, fn_name)
	g.sb.writeln(';')
	csrc := g.sb.str()
	assert csrc.contains('typedef struct printer__CounterWriter_T_string printer__CounterWriter_T_string;'), csrc
	assert csrc.contains('u64 printer__CounterWriter_T_string__count(printer__CounterWriter_T_string w);'), csrc
}

fn test_register_fn_signature_preserves_pointer_receiver_on_generic_struct_instance() {
	mut g := Gen.new([])
	g.cur_module = 'printer'
	g.active_generic_types['W'] = types.Type(types.string_)
	g.generic_struct_instances['printer__Sink'] = [
		GenericStructInstance{
			params_key: 'string'
			bindings:   {
				'W': types.Type(types.string_)
			}
			c_name:     'printer__Sink_T_string'
		},
	]
	stats_method := ast.FnDecl{
		name:      'stats'
		is_method: true
		receiver:  ast.Parameter{
			name: 'sink'
			typ:  ast.Expr(ast.Type(ast.PointerType{
				base_type: ast.Expr(ast.Type(ast.GenericType{
					name:   ast.Expr(ast.SelectorExpr{
						lhs: ast.Expr(ast.Ident{
							name: 'printer'
						})
						rhs: ast.Ident{
							name: 'Sink'
						}
					})
					params: [
						ast.Expr(ast.Ident{
							name: 'W'
						}),
					]
				}))
			}))
		}
		typ:       ast.FnType{
			return_type: ast.Expr(ast.Ident{
				name: 'int'
			})
		}
		language:  .v
	}
	fn_name := 'printer__Sink_T_string__stats'
	g.register_fn_signature(stats_method, fn_name)
	assert g.fn_param_types[fn_name][0] == 'printer__Sink_T_string*'
	assert g.fn_param_is_ptr[fn_name][0]
	g.gen_fn_head_with_name(stats_method, fn_name)
	csrc := g.sb.str()
	assert csrc.contains('int printer__Sink_T_string__stats(printer__Sink_T_string* sink)'), csrc
}

fn test_interface_fn_type_result_alias_is_forward_declared_before_interface_body() {
	csrc := cleanc_csrc_for_test_source('interface_fn_type_result_alias', '
module main

struct NoCaptures {}

interface Matcher {
	new_captures() !NoCaptures
}
')
	alias_pos := csrc.index('typedef struct _result_NoCaptures _result_NoCaptures;') or {
		panic('missing result alias forward declaration')
	}
	iface_pos := csrc.index('struct Matcher {') or { panic('missing interface body') }
	assert alias_pos < iface_pos
	assert csrc.contains('_result_NoCaptures (*new_captures)(void*)')
}

fn test_record_generic_struct_bindings_preserves_pointer_params() {
	mut env := types.Environment.new()
	mut printer_scope := types.new_scope(unsafe { nil })
	printer_scope.insert('CounterWriter', types.Type(types.Struct{
		name:           'printer__CounterWriter'
		generic_params: ['W']
	}))
	lock env.scopes {
		env.scopes['printer'] = printer_scope
	}
	mut g := Gen.new_with_env([], env)
	g.cur_module = 'printer'
	g.record_generic_struct_bindings('CounterWriter', 'printer__CounterWriter', [
		ast.Expr(ast.Ident{
			name: 'stringptr'
		}),
	])
	instances := g.generic_struct_instances['printer__CounterWriter']
	assert instances.len == 1
	assert instances[0].params_key == 'stringptr'
	binding := instances[0].bindings['W'] or { panic('missing W binding') }
	assert binding is types.Pointer
	assert (binding as types.Pointer).base_type.name() == 'string'
	g.active_generic_types = instances[0].bindings.clone()
	assert g.expr_type_to_c(ast.Expr(ast.Ident{
		name: 'W'
	})) == 'string*'
}

fn test_init_expr_uses_specialized_return_type_for_unqualified_generic_literal() {
	mut gen := Gen.new([])
	gen.cur_fn_ret_type = 'core__SearchWorker_T_stringptr'
	gen.emitted_types['body_core__SearchWorker_T_stringptr'] = true
	gen.emitted_types['body_core__SearchWorker_T_int'] = true
	gen.gen_init_expr(ast.InitExpr{
		typ: ast.Ident{
			name: 'SearchWorker'
		}
	})
	assert gen.sb.str() == '((core__SearchWorker_T_stringptr){0})'
}

fn test_init_expr_uses_qualified_return_type_for_primary_generic_literal() {
	mut gen := Gen.new([])
	gen.cur_fn_ret_type = 'core__SearchWorker'
	gen.gen_init_expr(ast.InitExpr{
		typ: ast.Ident{
			name: 'SearchWorker'
		}
	})
	assert gen.sb.str() == '((core__SearchWorker){0})'
}

fn test_init_expr_does_not_use_array_return_type_for_element_literal() {
	mut gen := Gen.new([])
	gen.cur_fn_ret_type = 'Array_printer__SubMatch'
	gen.cur_fn_c_name = 'printer__json_submatches'
	gen.gen_init_expr(ast.InitExpr{
		typ: ast.Ident{
			name: 'SubMatch'
		}
	})
	assert gen.sb.str() == '((printer__SubMatch){0})'
}

fn test_init_expr_qualifies_unqualified_literal_from_pending_late_struct() {
	mut gen := Gen.new([])
	gen.pending_late_body_keys['body_core__SearchWorker'] = true
	gen.gen_init_expr(ast.InitExpr{
		typ: ast.Ident{
			name: 'SearchWorker'
		}
	})
	assert gen.sb.str() == '((core__SearchWorker){0})'
}

fn test_init_expr_qualifies_unqualified_literal_from_module_scope_type() {
	mut env := types.Environment.new()
	mut core_scope := types.new_scope(unsafe { nil })
	core_scope.insert('SearchWorker', types.Type(types.Struct{
		name: 'core__SearchWorker'
	}))
	lock env.scopes {
		env.scopes['core'] = core_scope
	}
	mut gen := Gen.new_with_env([], env)
	gen.cur_fn_c_name = 'core__SearchWorkerBuilder__build_T_core_BufferWriter'
	gen.gen_init_expr(ast.InitExpr{
		typ: ast.Ident{
			name: 'SearchWorker'
		}
	})
	assert gen.sb.str() == '((core__SearchWorker){0})'
}

fn test_init_expr_keeps_builtin_option_type_unqualified_in_module_function() {
	mut gen := Gen.new([])
	gen.cur_fn_c_name = 'searcher__Searcher__new'
	gen.gen_init_expr(ast.InitExpr{
		typ:    ast.Ident{
			name: '_option_u64'
		}
		fields: [
			ast.FieldInit{
				name:  'state'
				value: ast.Expr(ast.BasicLiteral{
					kind:  .number
					value: '2'
				})
			},
		]
	})
	assert gen.sb.str() == '((_option_u64){ .state = 2 })'
}

fn test_init_expr_keeps_external_c_type_unqualified_in_module_function() {
	mut gen := Gen.new([])
	gen.cur_fn_c_name = 'time__init_time_base'
	gen.gen_init_expr(ast.InitExpr{
		typ: ast.Ident{
			name: 'mach_timebase_info_data_t'
		}
	})
	assert gen.sb.str() == '((mach_timebase_info_data_t){0})'
}

fn test_stdatomic_compat_directive_is_guarded_during_emit() {
	mut g := Gen.new([])
	mut seen := map[string]bool{}
	g.emit_directive(ast.Directive{
		name:  'include'
		value: '"/tmp/vroot/thirdparty/stdatomic/nix/atomic.h"'
	}, '/tmp/x.v', true, mut seen)
	csrc := g.sb.str()
	assert csrc.contains('#define extern static')
	assert csrc.contains('#include "/tmp/vroot/thirdparty/stdatomic/nix/atomic.h"')
	assert csrc.contains('#undef extern')
}

fn test_sum_type_call_arg_wraps_pointer_variant_arg() {
	mut g := Gen.new([])
	g.sum_type_variants['ast__Expr'] = ['SelectorExpr']
	g.runtime_local_types['node'] = 'ast__SelectorExpr*'
	assert g.gen_sum_type_call_arg('ast__Expr', ast.Expr(ast.Ident{
		name: 'node'
	}))
	out := g.sb.str()
	assert out.contains('(ast__Expr){._tag = 0, ._data._SelectorExpr =')
	assert out.contains('*node')
}

fn test_sum_type_call_arg_keeps_declared_sum_storage_for_smartcasted_ident() {
	mut g := Gen.new([])
	g.sum_type_variants['ast__ScopeObject'] = ['ast__Var']
	g.runtime_local_types['obj'] = 'ast__Var'
	g.runtime_decl_types['obj'] = 'ast__ScopeObject'
	assert g.gen_sum_type_call_arg('ast__ScopeObject', ast.Expr(ast.Ident{
		name: 'obj'
	}))
	out := g.sb.str()
	assert out == 'obj'
	assert !out.contains('._data._Var')
	assert !out.contains('memdup')
}

fn test_sum_type_wrap_uses_module_qualified_payload_type() {
	mut g := Gen.new([])
	g.sum_type_variants['ast__Expr'] = ['EmptyExpr', 'StructInit']
	g.remember_runtime_local_type('empty_expr', 'EmptyExpr')
	g.gen_type_cast_expr('ast__Expr', ast.Expr(ast.Ident{
		name: 'empty_expr'
	}))
	out := g.sb.str()
	assert out.contains('ast__EmptyExpr _st')
	assert out.contains('sizeof(ast__EmptyExpr)')
	assert !out.contains(' EmptyExpr _st')
	assert !out.contains('sizeof(EmptyExpr)')
}

fn test_sum_type_wrap_uses_module_qualified_alias_literal_type() {
	mut g := Gen.new([])
	g.emitted_types['alias_ast__EmptyExpr'] = true
	g.sum_type_variants['ast__Expr'] = ['EmptyExpr', 'StructInit']
	g.gen_type_cast_expr('ast__Expr', ast.Expr(ast.InitExpr{
		typ: ast.Expr(ast.Ident{
			name: 'EmptyExpr'
		})
	}))
	out := g.sb.str()
	assert out.contains('ast__EmptyExpr _st')
	assert out.contains('((ast__EmptyExpr){0})')
	assert out.contains('sizeof(ast__EmptyExpr)')
	assert !out.contains(' EmptyExpr _st')
	assert !out.contains('((EmptyExpr){0})')
	assert !out.contains('sizeof(EmptyExpr)')
}

fn test_pointer_arg_wraps_enum_member_ident() {
	mut g := Gen.new([])
	g.emitted_types['enum_token__Precedence'] = true
	g.enum_type_fields['token__Precedence'] = {
		'lowest': true
	}
	g.fn_param_is_ptr['__new_array_with_default_noscan'] = [false, false, false, true]
	g.gen_call_arg('__new_array_with_default_noscan', 3, ast.Expr(ast.Ident{
		name: 'token__Precedence__lowest'
	}))
	out := g.sb.str()
	assert out == '&((token__Precedence[1]){token__Precedence__lowest}[0])'
	assert !out.contains('&token__Precedence__lowest')
}

fn test_enum_member_c_name_uses_declared_enum_owner() {
	mut g := Gen.new([])
	g.emitted_types['enum_StrIntpType'] = true
	g.emitted_types['enum_c__StrIntpType'] = true
	g.enum_type_fields['StrIntpType'] = {
		'si_no_str': true
	}
	assert g.enum_member_c_name('c__StrIntpType', 'si_no_str') == 'StrIntpType__si_no_str'
}

fn test_enum_member_c_name_keeps_real_qualified_enum_owner() {
	mut g := Gen.new([])
	g.emitted_types['enum_Kind'] = true
	g.emitted_types['enum_foo__Kind'] = true
	g.enum_type_fields['Kind'] = {
		'a': true
	}
	g.enum_type_fields['foo__Kind'] = {
		'b': true
	}
	assert g.enum_member_c_name('foo__Kind', 'b') == 'foo__Kind__b'
}

fn test_enum_from_string_helper_emits_option_enum_result() {
	mut g := Gen.new([])
	g.option_aliases['_option_Mode'] = true
	g.emitted_types['enum_Mode'] = true
	g.emitted_interface_bodies['IError'] = true
	g.emitted_types['body_None__'] = true
	g.emitted_types['body_string'] = true
	g.emit_option_result_structs()
	g.gen_enum_from_string_helper(ast.EnumDecl{
		name:   'Mode'
		fields: [
			ast.FieldDecl{
				name: 'fast'
			},
			ast.FieldDecl{
				name: 'slow'
			},
		]
	})
	csrc := g.sb.str()
	assert csrc.contains('struct _option_Mode')
	assert csrc.contains('_option_Mode Mode__from_string(string s);')
	assert csrc.contains('memcmp(s.str, "fast", 4) == 0')
	assert csrc.contains('Mode _val = Mode__fast;')
	assert csrc.contains('_option_ok(&_val, (_option*)&_opt, sizeof(_val));')
	assert csrc.contains('return (_option_Mode){ .state = 2 };')
}

fn test_cached_builtin_init_calls_plain_builtin_const_init() {
	mut g := Gen.new([])
	g.export_const_symbols = true
	g.cache_bundle_name = 'builtin'
	g.emit_modules['builtin'] = true
	g.fn_return_types['__v_init_consts_builtin'] = 'void'
	g.emit_cached_module_init_function()
	csrc := g.sb.str()
	assert csrc.contains('void __v2_cached_init_builtin(void) {')
	assert csrc.contains('\t__v_init_consts_builtin();')
	assert !csrc.contains('builtin____v_init_consts_builtin();')
}

fn test_generated_test_main_runs_main_runtime_consts_after_module_init() {
	mut g := Gen.new([])
	g.test_fn_names << 'test_smoke'
	g.fn_return_types['__v_init_consts_main'] = 'void'
	g.fn_return_types['rand__init'] = 'void'
	csrc := g.gen_finalize()
	rand_init_idx := csrc.index('\trand__init();') or { panic('missing rand__init call') }
	main_consts_idx := csrc.index('\t__v_init_consts_main();') or {
		panic('missing main runtime const init call')
	}
	assert rand_init_idx < main_consts_idx
	assert csrc.count('__v_init_consts_main();') == 1
}

fn test_sum_common_field_selector_uses_payload_pointer() {
	mut gen := Gen.new([])
	gen.sum_type_variants['ast__Stmt'] = ['FnDecl', 'TypeDecl']
	gen.sum_type_variants['ast__TypeDecl'] = ['AliasTypeDecl', 'FnTypeDecl']
	gen.struct_field_types['ast__FnDecl.pos'] = 'token__Pos'
	gen.struct_field_types['FnDecl.pos'] = 'token__Pos'
	gen.struct_field_types['ast__AliasTypeDecl.pos'] = 'token__Pos'
	gen.struct_field_types['AliasTypeDecl.pos'] = 'token__Pos'
	gen.struct_field_types['ast__FnTypeDecl.pos'] = 'token__Pos'
	gen.struct_field_types['FnTypeDecl.pos'] = 'token__Pos'
	gen.remember_runtime_local_type('stmt', 'ast__Stmt')
	gen.expr(ast.Expr(ast.SelectorExpr{
		lhs: ast.Ident{
			name: 'stmt'
		}
		rhs: ast.Ident{
			name: 'pos'
		}
	}))
	out := gen.sb.str()
	assert out.contains('switch (_sum_cf')
	assert out.contains('._data._FnDecl')
	assert out.contains('._data._TypeDecl')
	assert out.contains('ast__TypeDecl*')
	assert !out.contains('stmt.pos')
}

fn test_sum_common_field_selector_respects_concrete_local_type() {
	mut gen := Gen.new([])
	gen.sum_type_variants['ast__ScopeObject'] = ['EmptyScopeObject', 'Var']
	gen.struct_field_types['ast__EmptyScopeObject.typ'] = 'ast__Type'
	gen.struct_field_types['EmptyScopeObject.typ'] = 'ast__Type'
	gen.struct_field_types['ast__Var.typ'] = 'ast__Type'
	gen.struct_field_types['Var.typ'] = 'ast__Type'
	gen.remember_runtime_local_type('obj', 'ast__Var')
	gen.expr(ast.Expr(ast.SelectorExpr{
		lhs: ast.Ident{
			name: 'obj'
		}
		rhs: ast.Ident{
			name: 'typ'
		}
	}))
	out := gen.sb.str()
	assert out == 'obj.typ'
	assert !out.contains('switch')
}

fn test_sum_common_field_assignment_uses_variant_switch() {
	mut gen := Gen.new([])
	gen.sum_type_variants['ast__ScopeObject'] = ['EmptyScopeObject', 'Var']
	gen.struct_field_types['ast__EmptyScopeObject.typ'] = 'ast__Type'
	gen.struct_field_types['EmptyScopeObject.typ'] = 'ast__Type'
	gen.struct_field_types['ast__Var.typ'] = 'ast__Type'
	gen.struct_field_types['Var.typ'] = 'ast__Type'
	gen.remember_runtime_local_type('obj', 'ast__ScopeObject')
	gen.remember_runtime_local_type('ptype', 'ast__Type')
	gen.gen_stmt(ast.Stmt(ast.AssignStmt{
		op:  .assign
		lhs: [
			ast.Expr(ast.SelectorExpr{
				lhs: ast.Expr(ast.Ident{
					name: 'obj'
				})
				rhs: ast.Ident{
					name: 'typ'
				}
			}),
		]
		rhs: [
			ast.Expr(ast.Ident{
				name: 'ptype'
			}),
		]
	}))
	out := gen.sb.str()
	assert out.contains('ast__ScopeObject _sum_cf')
	assert out.contains('ast__Type _field_cf')
	assert out.contains('switch (_sum_cf')
	assert out.contains('->typ = _field_cf')
	assert !out.contains('}) = ptype')
}

fn test_mut_arg_sum_common_field_selector_uses_variant_field_address() {
	fn_name := 'checker__Checker__stmts_ending_with_expression'
	for arg in [
		ast.Expr(ast.ModifierExpr{
			kind: token.Token.key_mut
			expr: sum_common_field_stmts_selector_for_test()
		}),
		ast.Expr(ast.PrefixExpr{
			op:   token.Token.amp
			expr: sum_common_field_stmts_selector_for_test()
		}),
		ast.Expr(ast.PrefixExpr{
			op:   token.Token.amp
			expr: ast.Expr(ast.SelectorExpr{
				lhs: ast.Expr(ast.ParenExpr{
					expr: ast.Expr(ast.SelectorExpr{
						lhs: ast.Expr(ast.Ident{
							name: 'expr'
						})
						rhs: ast.Ident{
							name: 'or_expr'
						}
					})
				})
				rhs: ast.Ident{
					name: 'stmts'
				}
			})
		}),
		ast.Expr(ast.PrefixExpr{
			op:   token.Token.amp
			expr: ast.Expr(ast.SelectorExpr{
				lhs: ast.Expr(ast.CastExpr{
					typ:  ast.Expr(ast.Ident{
						name: 'ast__OrExpr'
					})
					expr: ast.Expr(ast.SelectorExpr{
						lhs: ast.Expr(ast.Ident{
							name: 'expr'
						})
						rhs: ast.Ident{
							name: 'or_expr'
						}
					})
				})
				rhs: ast.Ident{
					name: 'stmts'
				}
			})
		}),
		ast.Expr(ast.PrefixExpr{
			op:   token.Token.amp
			expr: ast.Expr(ast.ModifierExpr{
				kind: token.Token.key_mut
				expr: sum_common_field_stmts_selector_for_test()
			})
		}),
	] {
		mut gen := Gen.new([])
		gen.fn_param_is_ptr[fn_name] = [true, true, false]
		gen.fn_param_types[fn_name] = ['checker__Checker*', 'Array_ast__Stmt*', 'ast__Type']
		gen.sum_type_variants['checker__ORMExpr'] = ['ast__SqlExpr', 'ast__SqlStmt']
		gen.sum_type_variants['ast__Stmt'] = ['ast__ExprStmt']
		gen.struct_field_types['ast__SqlExpr.or_expr'] = 'ast__OrExpr'
		gen.struct_field_types['ast__SqlStmt.or_expr'] = 'ast__OrExpr'
		gen.struct_field_types['ast__OrExpr.stmts'] = 'Array_ast__Stmt'
		gen.remember_runtime_local_type('expr', 'checker__ORMExpr*')
		gen.gen_call_arg(fn_name, 1, arg)
		out := gen.sb.str()
		assert out.contains('Array_ast__Stmt* _field_cf'), out
		assert out.contains('&((((ast__SqlExpr*)'), out
		assert out.contains(')->or_expr.stmts)'), out
		assert out.contains('&((((ast__SqlStmt*)'), out
		assert !out.contains('&({ checker__ORMExpr*'), out
	}
}

fn test_address_of_sum_common_field_selector_uses_variant_field_address() {
	mut gen := Gen.new([])
	gen.sum_type_variants['checker__ORMExpr'] = ['ast__SqlExpr', 'ast__SqlStmt']
	gen.struct_field_types['ast__SqlExpr.or_expr'] = 'ast__OrExpr'
	gen.struct_field_types['ast__SqlStmt.or_expr'] = 'ast__OrExpr'
	gen.struct_field_types['ast__OrExpr.stmts'] = 'Array_ast__Stmt'
	gen.remember_runtime_local_type('expr', 'checker__ORMExpr*')
	gen.expr(ast.Expr(ast.PrefixExpr{
		op:   token.Token.amp
		expr: sum_common_field_stmts_selector_for_test()
	}))
	out := gen.sb.str()
	assert out.contains('Array_ast__Stmt* _field_cf'), out
	assert out.contains(')->or_expr.stmts)'), out
	assert !out.contains('&({ checker__ORMExpr*'), out
}

fn sum_common_field_stmts_selector_for_test() ast.Expr {
	return ast.Expr(ast.SelectorExpr{
		lhs: ast.Expr(ast.SelectorExpr{
			lhs: ast.Expr(ast.Ident{
				name: 'expr'
			})
			rhs: ast.Ident{
				name: 'or_expr'
			}
		})
		rhs: ast.Ident{
			name: 'stmts'
		}
	})
}

fn test_sum_variant_check_supports_nested_sum_variants() {
	mut gen := Gen.new([])
	gen.sum_type_variants['ast__Node'] = ['CallArg', 'Expr', 'IfBranch']
	gen.sum_type_variants['ast__Expr'] = ['Ident', 'InfixExpr']
	gen.remember_runtime_local_type('node', 'ast__Node')
	gen.expr(ast.Expr(ast.InfixExpr{
		op:  token.Token.eq
		lhs: ast.Expr(ast.Ident{
			name: 'node'
		})
		rhs: ast.Expr(ast.SelectorExpr{
			lhs: ast.Expr(ast.Ident{
				name: 'ast'
			})
			rhs: ast.Ident{
				name: 'InfixExpr'
			}
		})
	}))
	out := gen.sb.str()
	assert out.contains('node._tag == 1')
	assert out.contains('((ast__Expr*)(node._data._Expr))')
	assert out.contains('->_tag == 1')
	assert !out.contains('node == ast__InfixExpr')
}

fn test_assign_wraps_concrete_value_for_sum_type_field() {
	mut gen := Gen.new([])
	gen.sum_type_variants['ast__TypeInfo'] = ['Interface', 'SumType']
	gen.struct_field_types['ast__TypeSymbol.info'] = 'ast__TypeInfo'
	gen.remember_runtime_local_type('expr_type_sym', 'ast__TypeSymbol*')
	gen.remember_runtime_local_type('info', 'ast__Interface')
	gen.gen_stmt(ast.Stmt(ast.AssignStmt{
		op:  .assign
		lhs: [
			ast.Expr(ast.SelectorExpr{
				lhs: ast.Expr(ast.Ident{
					name: 'expr_type_sym'
				})
				rhs: ast.Ident{
					name: 'info'
				}
			}),
		]
		rhs: [
			ast.Expr(ast.Ident{
				name: 'info'
			}),
		]
	}))
	out := gen.sb.str()
	assert out.contains('expr_type_sym->info = ((ast__TypeInfo){._tag = 0, ._data._Interface =')
	assert out.contains('memdup(&_st')
	assert !out.contains('expr_type_sym->info = info;')
}

fn test_init_expr_wraps_concrete_value_for_sum_type_field() {
	mut gen := Gen.new([])
	gen.sum_type_variants['ast__TypeInfo'] = ['Interface', 'SumType']
	gen.struct_field_types['ast__TypeSymbol.info'] = 'ast__TypeInfo'
	gen.remember_runtime_local_type('info', 'ast__Interface')
	gen.gen_init_expr(ast.InitExpr{
		typ:    ast.Ident{
			name: 'ast__TypeSymbol'
		}
		fields: [
			ast.FieldInit{
				name:  'info'
				value: ast.Expr(ast.Ident{
					name: 'info'
				})
			},
		]
	})
	out := gen.sb.str()
	assert out.contains('.info = ((ast__TypeInfo){._tag = 0, ._data._Interface ='), out
	assert out.contains('memdup(&_st'), out
	assert !out.contains('.info = info'), out
}

fn test_sum_variant_check_accepts_type_rhs_for_selector_field() {
	mut gen := Gen.new([])
	gen.sum_type_variants['ast__TypeInfo'] = ['UnknownTypeInfo', 'Array']
	gen.struct_field_types['ast__TypeSymbol.info'] = 'ast__TypeInfo'
	gen.remember_runtime_local_type('elem_sym', 'ast__TypeSymbol*')
	gen.expr(ast.Expr(ast.InfixExpr{
		op:  token.Token.eq
		lhs: ast.Expr(ast.ModifierExpr{
			kind: token.Token.key_mut
			expr: ast.Expr(ast.SelectorExpr{
				lhs: ast.Expr(ast.Ident{
					name: 'elem_sym'
				})
				rhs: ast.Ident{
					name: 'info'
				}
			})
		})
		rhs: ast.Expr(ast.Ident{
			name: 'Array'
		})
	}))
	out := gen.sb.str()
	assert out.contains('elem_sym->info._tag == 1'), out
	assert !out.contains('== Array'), out
}

fn test_selector_field_on_narrowed_sum_selector_extracts_payload() {
	mut env := types.Environment.new()
	env.set_expr_type(92, types.Type(types.Struct{
		name: 'ast__Array'
	}))
	mut gen := Gen.new_with_env([], env)
	gen.sum_type_variants['ast__TypeInfo'] = ['UnknownTypeInfo', 'Array']
	gen.struct_field_types['ast__TypeSymbol.info'] = 'ast__TypeInfo'
	gen.struct_field_types['ast__Array.elem_type'] = 'ast__Type'
	gen.remember_runtime_local_type('elem_sym', 'ast__TypeSymbol*')
	info_sel := ast.Expr(ast.SelectorExpr{
		lhs: ast.Expr(ast.Ident{
			name: 'elem_sym'
		})
		rhs: ast.Ident{
			name: 'info'
		}
		pos: token.Pos{
			id: 92
		}
	})
	gen.expr(ast.Expr(ast.SelectorExpr{
		lhs: ast.Expr(ast.ModifierExpr{
			kind: token.Token.key_mut
			expr: info_sel
		})
		rhs: ast.Ident{
			name: 'elem_type'
		}
	}))
	out := gen.sb.str()
	assert out.contains('._data._Array'), out
	assert out.contains('->elem_type'), out
	assert !out.contains('info.elem_type'), out
}

fn test_decl_assign_uses_position_type_before_function_scope_name_collision() {
	mut env := types.Environment.new()
	env.set_expr_type(77, types.int_)
	mut fn_scope := types.new_scope(unsafe { nil })
	fn_scope.insert('value', types.Type(types.string_))

	mut gen := Gen.new_with_env([], env)
	gen.cur_fn_scope = fn_scope
	gen.gen_stmt(ast.Stmt(ast.AssignStmt{
		op:  .decl_assign
		lhs: [
			ast.Expr(ast.Ident{
				pos:  token.Pos{
					id: 77
				}
				name: 'value'
			}),
		]
		rhs: [
			ast.Expr(ast.BasicLiteral{
				kind:  .number
				value: '0'
			}),
		]
	}))
	out := gen.sb.str().trim_space()
	assert out == 'int value = 0;'
	assert !out.contains('string value')
}

fn test_decl_assign_does_not_use_position_type_for_or_temp() {
	mut env := types.Environment.new()
	env.set_expr_type(88, types.Type(types.Array{
		elem_type: types.string_
	}))
	mut gen := Gen.new_with_env([], env)
	gen.fn_return_types['map__get_check'] = 'void*'
	gen.gen_stmt(ast.Stmt(ast.AssignStmt{
		op:  .decl_assign
		lhs: [
			ast.Expr(ast.Ident{
				pos:  token.Pos{
					id: 88
				}
				name: '_or_t1'
			}),
		]
		rhs: [
			ast.Expr(ast.CallExpr{
				lhs:  ast.Expr(ast.Ident{
					name: 'map__get_check'
				})
				args: [
					ast.Expr(ast.Ident{
						name: 'm'
					}),
					ast.Expr(ast.Ident{
						name: 'key'
					}),
				]
			}),
		]
	}))
	out := gen.sb.str()
	assert out.contains('_or_t1 = map__get_check')
	assert !out.contains('Array_string _or_t1')
}

fn test_addr_of_temp_compound_literal_uses_rhs_type_before_stale_scope_type() {
	mut gen := Gen.new([])
	gen.fn_return_types['string__plus'] = 'string'
	gen.runtime_local_types['_or_t_key'] = 'string*'
	gen.expr(ast.Expr(ast.UnsafeExpr{
		stmts: [
			ast.Stmt(ast.AssignStmt{
				op:  .decl_assign
				lhs: [ast.Expr(ast.Ident{
					name: '_or_t_key'
				})]
				rhs: [
					ast.Expr(ast.CallExpr{
						lhs: ast.Expr(ast.Ident{
							name: 'string__plus'
						})
					}),
				]
			}),
			ast.Stmt(ast.ExprStmt{
				expr: ast.Expr(ast.PrefixExpr{
					op:   .amp
					expr: ast.Expr(ast.Ident{
						name: '_or_t_key'
					})
				})
			}),
		]
	}))
	out := gen.sb.str()
	assert out.contains('&((string[1]){')
	assert !out.contains('&((string*[1]){')
}

fn test_at_vexeroot_ident_emits_string_literal() {
	mut gen := Gen.new([])
	prefs := &vpref.Preferences{
		vroot: '/tmp/vroot'
	}
	gen.pref = prefs
	gen.expr(ast.Expr(ast.Ident{
		name: '@VEXEROOT'
	}))
	out := gen.sb.str()
	assert out.contains('"/tmp/vroot"')
	assert !out.contains('@VEXEROOT')
}

fn test_as_cast_selector_uses_declared_sum_field_type() {
	mut gen := Gen.new([])
	gen.sum_type_variants['ast__Expr'] = ['CallExpr', 'AsCast']
	gen.struct_field_types['ast__ParExpr.expr'] = 'ast__Expr'
	gen.remember_runtime_local_type('par', 'ast__ParExpr')
	gen.expr(ast.Expr(ast.SelectorExpr{
		lhs: ast.Expr(ast.AsCastExpr{
			expr: ast.Expr(ast.SelectorExpr{
				lhs: ast.Expr(ast.Ident{
					name: 'par'
				})
				rhs: ast.Ident{
					name: 'expr'
				}
			})
			typ:  ast.Expr(ast.SelectorExpr{
				lhs: ast.Expr(ast.Ident{
					name: 'ast'
				})
				rhs: ast.Ident{
					name: 'AsCast'
				}
			})
		})
		rhs: ast.Ident{
			name: 'typ'
		}
	}))
	out := gen.sb.str()
	assert out.contains('par.expr')
	assert out.contains('._data._AsCast')
	assert !out.contains('._data._CallExpr)._data')
}

fn test_decl_assign_as_cast_uses_cast_target_before_env_type() {
	mut env := types.Environment.new()
	env.set_expr_type(91, types.Type(types.Struct{
		name: 'ast__Expr'
	}))
	mut gen := Gen.new_with_env([], env)
	gen.sum_type_variants['ast__Expr'] = ['ast__PrefixExpr']
	gen.remember_runtime_local_type('node', 'ast__Expr')
	gen.gen_assign_stmt(ast.AssignStmt{
		op:  .decl_assign
		lhs: [
			ast.Expr(ast.Ident{
				name: 'inner'
			}),
		]
		rhs: [
			ast.Expr(ast.AsCastExpr{
				expr: ast.Expr(ast.Ident{
					name: 'node'
				})
				typ:  ast.Expr(ast.SelectorExpr{
					lhs: ast.Expr(ast.Ident{
						name: 'ast'
					})
					rhs: ast.Ident{
						name: 'PrefixExpr'
					}
				})
				pos:  token.Pos{
					id: 91
				}
			}),
		]
	})
	out := gen.sb.str().trim_space()
	assert out.starts_with('ast__PrefixExpr inner =')
	assert !out.contains('ast__Expr inner =')
}

fn test_cast_expr_sum_variant_extract_uses_declared_selector_field_type() {
	mut env := types.Environment.new()
	env.set_expr_type(92, types.Type(types.Struct{
		name: 'ast__ArrayDecompose'
	}))
	mut gen := Gen.new_with_env([], env)
	gen.sum_type_variants['ast__Expr'] = ['ast__ArrayDecompose']
	gen.struct_field_types['ast__CallArg.expr'] = 'ast__Expr'
	gen.remember_runtime_local_type('arg', 'ast__CallArg')
	gen.expr(ast.Expr(ast.CastExpr{
		typ:  ast.Expr(ast.SelectorExpr{
			lhs: ast.Expr(ast.Ident{
				name: 'ast'
			})
			rhs: ast.Ident{
				name: 'ArrayDecompose'
			}
		})
		expr: ast.Expr(ast.SelectorExpr{
			lhs: ast.Expr(ast.Ident{
				name: 'arg'
			})
			rhs: ast.Ident{
				name: 'expr'
			}
			pos: token.Pos{
				id: 92
			}
		})
	}))
	out := gen.sb.str()
	assert out.contains('arg.expr')
	assert out.contains('._data._ast__ArrayDecompose')
	assert !out.contains('((ast__ArrayDecompose)(arg.expr))')

	mut ptr_gen := Gen.new([])
	ptr_gen.sum_type_variants['ast__Expr'] = ['ast__ArrayDecompose']
	ptr_gen.struct_field_types['ast__CallArg.expr'] = 'ast__Expr'
	ptr_sel := ast.SelectorExpr{
		lhs: ast.Expr(ast.PrefixExpr{
			op:   .mul
			expr: ast.Expr(ast.CastExpr{
				typ:  ast.Expr(ast.Ident{
					name: 'ast__CallArg*'
				})
				expr: ast.Expr(ast.CallExpr{
					lhs:  ast.Expr(ast.Ident{
						name: 'array__last'
					})
					args: [
						ast.Expr(ast.Ident{
							name: 'args'
						}),
					]
				})
			})
		})
		rhs: ast.Ident{
			name: 'expr'
		}
	}
	assert ptr_gen.selector_declared_field_type(ptr_sel) == 'ast__Expr'
	ptr_cast := ast.CastExpr{
		typ:  ast.Expr(ast.SelectorExpr{
			lhs: ast.Expr(ast.Ident{
				name: 'ast'
			})
			rhs: ast.Ident{
				name: 'ArrayDecompose'
			}
		})
		expr: ast.Expr(ptr_sel)
	}
	assert ptr_gen.get_sum_type_variants_for('ast__Expr').len == 1
	assert ptr_gen.expr_type_to_c(ptr_cast.typ) == 'ast__ArrayDecompose'
	assert ptr_gen.cast_expr_is_sum_variant_extract(ptr_cast, 'ast__ArrayDecompose')
	mut as_gen := Gen.new([])
	as_gen.sum_type_variants['ast__Expr'] = ['ast__ArrayDecompose']
	as_gen.struct_field_types['ast__CallArg.expr'] = 'ast__Expr'
	as_gen.gen_as_cast_expr(ast.AsCastExpr{
		expr: ast.Expr(ptr_sel)
		typ:  ptr_cast.typ
	})
	as_out := as_gen.sb.str()
	assert as_out.contains('._data._'), as_out
	ptr_gen.expr(ast.Expr(ptr_cast))
	out2 := ptr_gen.sb.str()
	assert out2.contains('._data._'), out2
	assert !out2.contains('((ast__ArrayDecompose)((*(ast__CallArg*)array__last(args)).expr))')
}

fn test_cast_expr_same_aggregate_value_omits_c_struct_cast() {
	mut env := types.Environment.new()
	env.set_expr_type(93, types.Type(types.Struct{
		name: 'ast__ArrayInit'
	}))
	mut gen := Gen.new_with_env([], env)
	gen.expr(ast.Expr(ast.CastExpr{
		typ:  ast.Expr(ast.Ident{
			name: 'ast__ArrayInit'
		})
		expr: ast.Expr(ast.ParenExpr{
			expr: ast.Expr(ast.PrefixExpr{
				op:   .mul
				expr: ast.Expr(ast.CastExpr{
					typ:  ast.Expr(ast.Ident{
						name: 'ast__ArrayInit*'
					})
					expr: ast.Expr(ast.Ident{
						name: 'payload'
					})
				})
			})
			pos:  token.Pos{
				id: 93
			}
		})
	}))
	out := gen.sb.str()
	assert out.contains('(*((ast__ArrayInit*)'), out
	assert !out.contains('((ast__ArrayInit)('), out
}

fn test_address_of_sum_type_cast_materializes_value() {
	mut gen := Gen.new([])
	gen.sum_type_variants['ast__Expr'] = ['IfExpr']
	gen.sum_type_variants['ast__HashStmtNode'] = ['IfExpr', 'HashStmt']
	gen.remember_runtime_local_type('node', 'ast__Expr')
	gen.expr(ast.Expr(ast.PrefixExpr{
		op:   .amp
		expr: ast.Expr(ast.CastExpr{
			typ:  ast.Expr(ast.SelectorExpr{
				lhs: ast.Expr(ast.Ident{
					name: 'ast'
				})
				rhs: ast.Ident{
					name: 'HashStmtNode'
				}
			})
			expr: ast.Expr(ast.AsCastExpr{
				expr: ast.Expr(ast.Ident{
					name: 'node'
				})
				typ:  ast.Expr(ast.SelectorExpr{
					lhs: ast.Expr(ast.Ident{
						name: 'ast'
					})
					rhs: ast.Ident{
						name: 'IfExpr'
					}
				})
			})
		})
	}))
	out := gen.sb.str()
	assert out.contains('malloc(sizeof(ast__HashStmtNode))')
	assert out.contains('._tag = 0')
	assert !out.contains('((ast__HashStmtNode*)(')
}

fn test_address_of_sum_type_cast_keeps_pointer_cast() {
	mut gen := Gen.new([])
	gen.sum_type_variants['ast__HashStmtNode'] = ['IfExpr', 'HashStmt']
	gen.expr(ast.Expr(ast.PrefixExpr{
		op:   .amp
		expr: ast.Expr(ast.CastExpr{
			typ:  ast.Expr(ast.SelectorExpr{
				lhs: ast.Expr(ast.Ident{
					name: 'ast'
				})
				rhs: ast.Ident{
					name: 'HashStmtNode'
				}
			})
			expr: ast.Expr(ast.PrefixExpr{
				op:   .amp
				expr: ast.Expr(ast.Ident{
					name: 'node'
				})
			})
		})
	}))
	out := gen.sb.str()
	assert out == '((ast__HashStmtNode*)(&node))'
	assert !out.contains('malloc(sizeof(ast__HashStmtNode))')
}

fn test_cast_expr_from_sum_type_to_variant_extracts_payload() {
	mut gen := Gen.new([])
	gen.sum_type_variants['ast__Stmt'] = ['ExprStmt', 'ReturnStmt']
	gen.remember_runtime_local_type('stmt', 'ast__Stmt')
	cast_expr := ast.CastExpr{
		typ:  ast.Expr(ast.SelectorExpr{
			lhs: ast.Expr(ast.Ident{
				name: 'ast'
			})
			rhs: ast.Ident{
				name: 'ExprStmt'
			}
		})
		expr: ast.Expr(ast.Ident{
			name: 'stmt'
		})
	}
	gen.expr(ast.Expr(cast_expr))
	out := gen.sb.str()
	assert out.contains('(stmt)._data._ExprStmt')
	assert !out.contains('((ast__ExprStmt)(stmt))')
}

fn test_cast_expr_module_struct_from_pointer_source_emits_pointer_cast() {
	mut gen := Gen.new([])
	gen.remember_runtime_local_type('source_fn', 'void*')
	gen.expr(ast.Expr(ast.CastExpr{
		typ:  ast.Expr(ast.SelectorExpr{
			lhs: ast.Expr(ast.Ident{
				name: 'ast'
			})
			rhs: ast.Ident{
				name: 'FnDecl'
			}
		})
		expr: ast.Expr(ast.Ident{
			name: 'source_fn'
		})
	}))
	out := gen.sb.str()
	assert out == '((ast__FnDecl*)(source_fn))'
	assert !out.contains('((ast__FnDecl)(source_fn))')
}

fn test_cast_expr_from_sum_type_pointer_deref_to_variant_extracts_payload() {
	mut gen := Gen.new([])
	gen.cur_import_modules['ast'] = 'ast'
	gen.sum_type_variants['ast__Stmt'] = ['ExprStmt', 'ReturnStmt']
	stmt_ptr_typ := ast.Expr(ast.PrefixExpr{
		op:   .amp
		expr: ast.Expr(ast.SelectorExpr{
			lhs: ast.Expr(ast.Ident{
				name: 'ast'
			})
			rhs: ast.Ident{
				name: 'Stmt'
			}
		})
	})
	assert gen.expr_type_to_c(stmt_ptr_typ).trim_space().trim_right('*') == 'ast__Stmt', gen.expr_type_to_c(stmt_ptr_typ)

	cast_expr := ast.CastExpr{
		typ:  ast.Expr(ast.SelectorExpr{
			lhs: ast.Expr(ast.Ident{
				name: 'ast'
			})
			rhs: ast.Ident{
				name: 'ExprStmt'
			}
		})
		expr: ast.Expr(ast.PrefixExpr{
			op:   .mul
			expr: ast.Expr(ast.CastExpr{
				typ:  stmt_ptr_typ
				expr: ast.Expr(ast.CallExpr{
					lhs:  ast.Expr(ast.Ident{
						name: 'array__last'
					})
					args: [ast.Expr(ast.Ident{
						name: 'stmts'
					})]
				})
			})
		})
	}
	assert gen.cast_expr_is_sum_variant_extract(cast_expr, 'ast__ExprStmt'), gen.get_expr_type(cast_expr.expr)
	gen.expr(ast.Expr(cast_expr))
	out := gen.sb.str()
	assert out.contains('._data._ExprStmt')
	assert !out.contains('((ast__ExprStmt)((*')
}

fn test_selector_on_cast_from_array_last_extracts_sum_payload() {
	mut gen := Gen.new([])
	gen.cur_import_modules['ast'] = 'ast'
	gen.sum_type_variants['ast__Stmt'] = ['ExprStmt', 'ReturnStmt']
	gen.struct_field_types['ast__IfBranch.stmts'] = 'Array_ast__Stmt'
	gen.struct_field_types['ast__ExprStmt.typ'] = 'ast__Type'
	gen.emitted_types['body_ast__ExprStmt'] = true
	gen.remember_runtime_local_type('branch', 'ast__IfBranch')
	stmts_expr := ast.Expr(ast.SelectorExpr{
		lhs: ast.Expr(ast.Ident{
			name: 'branch'
		})
		rhs: ast.Ident{
			name: 'stmts'
		}
	})
	cast_expr := ast.Expr(ast.CastExpr{
		typ:  ast.Expr(ast.Ident{
			name: 'ast__ExprStmt'
		})
		expr: ast.Expr(ast.PrefixExpr{
			op:   .mul
			expr: ast.Expr(ast.CastExpr{
				typ:  ast.Expr(ast.Ident{
					name: 'ast__Stmtptr'
				})
				expr: ast.Expr(ast.CallExpr{
					lhs:  ast.Expr(ast.Ident{
						name: 'array__last'
					})
					args: [stmts_expr]
				})
			})
		})
	})
	gen.expr(ast.Expr(ast.SelectorExpr{
		lhs: cast_expr
		rhs: ast.Ident{
			name: 'typ'
		}
	}))
	out := gen.sb.str()
	assert out.contains('._data._ExprStmt'), out
	assert out.contains('->typ') || out.contains('.typ'), out
	assert !out.contains('((ast__ExprStmt)((*'), out
}

fn test_selector_on_call_or_cast_from_array_last_extracts_sum_payload() {
	mut gen := Gen.new([])
	gen.cur_import_modules['ast'] = 'ast'
	gen.sum_type_variants['ast__Stmt'] = ['ExprStmt', 'ReturnStmt']
	gen.struct_field_types['ast__IfBranch.stmts'] = 'Array_ast__Stmt'
	gen.struct_field_types['ast__ExprStmt.typ'] = 'ast__Type'
	gen.emitted_types['body_ast__ExprStmt'] = true
	gen.remember_runtime_local_type('branch', 'ast__IfBranch')
	stmts_expr := ast.Expr(ast.SelectorExpr{
		lhs: ast.Expr(ast.Ident{
			name: 'branch'
		})
		rhs: ast.Ident{
			name: 'stmts'
		}
	})
	sum_value := ast.Expr(ast.PrefixExpr{
		op:   .mul
		expr: ast.Expr(ast.CastExpr{
			typ:  ast.Expr(ast.Ident{
				name: 'ast__Stmtptr'
			})
			expr: ast.Expr(ast.CallExpr{
				lhs:  ast.Expr(ast.Ident{
					name: 'array__last'
				})
				args: [stmts_expr]
			})
		})
	})
	gen.expr(ast.Expr(ast.SelectorExpr{
		lhs: ast.Expr(ast.CallOrCastExpr{
			lhs:  ast.Expr(ast.Ident{
				name: 'ast__ExprStmt'
			})
			expr: sum_value
		})
		rhs: ast.Ident{
			name: 'typ'
		}
	}))
	out := gen.sb.str()
	assert out.contains('._data._ExprStmt'), out
	assert !out.contains('((ast__ExprStmt)((*'), out
}

fn test_selector_on_as_cast_from_array_last_extracts_sum_payload() {
	mut gen := Gen.new([])
	gen.cur_import_modules['ast'] = 'ast'
	gen.sum_type_variants['ast__Stmt'] = ['ExprStmt', 'ReturnStmt']
	gen.struct_field_types['ast__IfBranch.stmts'] = 'Array_ast__Stmt'
	gen.struct_field_types['ast__ExprStmt.typ'] = 'ast__Type'
	gen.remember_runtime_local_type('branch', 'ast__IfBranch')
	stmts_expr := ast.Expr(ast.SelectorExpr{
		lhs: ast.Expr(ast.Ident{
			name: 'branch'
		})
		rhs: ast.Ident{
			name: 'stmts'
		}
	})
	sum_value := ast.Expr(ast.PrefixExpr{
		op:   .mul
		expr: ast.Expr(ast.CastExpr{
			typ:  ast.Expr(ast.Ident{
				name: 'ast__Stmtptr'
			})
			expr: ast.Expr(ast.CallExpr{
				lhs:  ast.Expr(ast.Ident{
					name: 'array__last'
				})
				args: [stmts_expr]
			})
		})
	})
	gen.expr(ast.Expr(ast.SelectorExpr{
		lhs: ast.Expr(ast.AsCastExpr{
			expr: sum_value
			typ:  ast.Expr(ast.Ident{
				name: 'ast__ExprStmt'
			})
		})
		rhs: ast.Ident{
			name: 'typ'
		}
	}))
	out := gen.sb.str()
	assert out.contains('._data._ExprStmt'), out
	assert out.contains('->typ') || out.contains('.typ'), out
	assert !out.contains('((ast__ExprStmt)((*'), out
}

fn test_as_cast_selector_from_array_last_resolves_source_sum_type() {
	mut gen := Gen.new([])
	gen.cur_import_modules['ast'] = 'ast'
	gen.sum_type_variants['ast__Expr'] = ['ArrayDecompose', 'Ident']
	gen.struct_field_types['ast__CallArg.expr'] = 'ast__Expr'
	gen.remember_runtime_local_type('args', 'Array_ast__CallArg')
	arg_expr := ast.Expr(ast.SelectorExpr{
		lhs: ast.Expr(ast.CallExpr{
			lhs:  ast.Expr(ast.Ident{
				name: 'array__last'
			})
			args: [ast.Expr(ast.Ident{
				name: 'args'
			})]
		})
		rhs: ast.Ident{
			name: 'expr'
		}
	})
	gen.expr(ast.Expr(ast.AsCastExpr{
		expr: arg_expr
		typ:  ast.Expr(ast.Ident{
			name: 'ast__ArrayDecompose'
		})
	}))
	out := gen.sb.str()
	assert out.contains('._data._ArrayDecompose'), out
	assert !out.contains('((ast__ArrayDecompose)((*'), out
}

fn test_sum_pointer_arg_uses_declared_selector_storage_despite_narrowed_env_type() {
	mut gen := Gen.new([])
	gen.sum_type_variants['ast__Expr'] = ['Ident', 'SelectorExpr']
	gen.struct_field_types['ast__MatchExpr.cond'] = 'ast__Expr'
	gen.remember_runtime_local_type('node', 'ast__MatchExpr*')
	gen.fn_param_types['checker__Checker__fail_if_immutable'] = ['ast__Expr*']
	gen.fn_param_is_ptr['checker__Checker__fail_if_immutable'] = [true]
	cond := ast.SelectorExpr{
		lhs: ast.Expr(ast.Ident{
			name: 'node'
		})
		rhs: ast.Ident{
			name: 'cond'
		}
		pos: token.Pos{
			id: 81
		}
	}
	gen.selector_field_type_cache[selector_field_type_cache_key(cond)] = 'ast__Ident'
	gen.gen_call_arg('checker__Checker__fail_if_immutable', 0, ast.Expr(cond))
	out := gen.sb.str()
	assert out == '&node->cond', out
	assert !out.contains('._tag ='), out
	assert !out.contains('._data._Ident'), out
}

fn test_as_cast_from_sum_type_pointer_deref_keeps_deref_for_payload_access() {
	mut gen := Gen.new([])
	gen.cur_import_modules['ast'] = 'ast'
	gen.sum_type_variants['ast__Stmt'] = ['GlobalDecl', 'FnDecl']
	gen.remember_runtime_local_type('stmt_ptr', 'ast__Stmt*')
	gen.expr(ast.Expr(ast.AsCastExpr{
		expr: ast.Expr(ast.PrefixExpr{
			op:   .mul
			expr: ast.Expr(ast.Ident{
				name: 'stmt_ptr'
			})
		})
		typ:  ast.Expr(ast.SelectorExpr{
			lhs: ast.Expr(ast.Ident{
				name: 'ast'
			})
			rhs: ast.Ident{
				name: 'GlobalDecl'
			}
		})
	}))
	out := gen.sb.str()
	assert out.contains('(*stmt_ptr))._data._GlobalDecl'), out
	assert !out.contains('stmt_ptr._data._GlobalDecl'), out
}

fn test_pipeline_as_cast_from_for_in_addressed_array_element_uses_pointer_payload_access() {
	csrc := cleanc_csrc_for_test_source('for_in_as_cast', 'module main

struct File {
	stmts []Stmt
}

type Stmt = GlobalDecl | FnDecl

struct GlobalDecl {}
struct FnDecl {}

fn use_global(_ GlobalDecl) {}

fn gen_file(file File) {
	mut global_indices := []int{}
	for gi in global_indices {
		stmt_ptr := &file.stmts[gi]
		use_global((*stmt_ptr) as GlobalDecl)
	}
}
')
	assert !csrc.contains('stmt_ptr._data._GlobalDecl'), csrc
	assert csrc.contains('((*stmt_ptr))._data._GlobalDecl')
		|| csrc.contains('stmt_ptr->_data._GlobalDecl'), csrc
}

fn test_for_in_over_cloned_u8_array_preserves_element_type() {
	mut g := Gen.new([])
	g.fn_return_types['array__clone'] = 'array'
	g.remember_runtime_local_type('src', 'Array_u8')
	g.remember_runtime_local_type('i', 'int')
	g.remember_runtime_local_type('sep', 'u8')
	g.gen_assign_stmt(ast.AssignStmt{
		op:  .decl_assign
		lhs: [ast.Expr(ast.Ident{
			name: 'bytes'
		})]
		rhs: [
			ast.Expr(ast.CallExpr{
				lhs:  ast.Expr(ast.Ident{
					name: 'array__clone'
				})
				args: [
					ast.Expr(ast.PrefixExpr{
						op:   .amp
						expr: ast.Expr(ast.Ident{
							name: 'src'
						})
					}),
				]
			}),
		]
	})
	g.gen_assign_stmt(ast.AssignStmt{
		op:  .decl_assign
		lhs: [ast.Expr(ast.Ident{
			name: 'byte'
		})]
		rhs: [
			ast.Expr(ast.IndexExpr{
				lhs:  ast.Expr(ast.Ident{
					name: 'bytes'
				})
				expr: ast.Expr(ast.Ident{
					name: 'i'
				})
			}),
		]
	})
	g.gen_assign_stmt(ast.AssignStmt{
		op:  .assign
		lhs: [
			ast.Expr(ast.IndexExpr{
				lhs:  ast.Expr(ast.Ident{
					name: 'bytes'
				})
				expr: ast.Expr(ast.Ident{
					name: 'i'
				})
			}),
		]
		rhs: [ast.Expr(ast.Ident{
			name: 'sep'
		})]
	})
	csrc := g.sb.str()
	assert csrc.contains('Array_u8 bytes = array__clone_to_depth('), csrc
	assert csrc.contains('u8 byte = ((u8*)'), csrc
	assert csrc.contains('((u8*)bytes.data)[((int)(i))] = sep;'), csrc
	assert !csrc.contains('int byte = ((int*)'), csrc
	assert !csrc.contains('((int*)bytes.data)[((int)(i))] = sep;'), csrc
}

fn test_pipeline_sum_variant_cast_from_array_last_call_extracts_payload() {
	csrc := cleanc_csrc_for_test_source('array_last_sum_cast', 'module main

struct ExprStmt {
	typ int
}

struct ReturnStmt {}

type Stmt = ExprStmt | ReturnStmt

struct Branch {
	stmts []Stmt
}

fn (a []Stmt) last() Stmt {
	return a[0]
}

fn f(branch Branch) int {
	if branch.stmts.len > 0 {
		if branch.stmts.last() is ExprStmt {
			return (branch.stmts.last() as ExprStmt).typ
		}
	}
	return 0
}
')
	assert csrc.contains('._data._ExprStmt'), csrc
	assert !csrc.contains('((ExprStmt)((*(Stmt*)array__last(branch.stmts))))'), csrc
	assert !csrc.contains('((main__ExprStmt)((*(main__Stmt*)array__last(branch.stmts))))'), csrc
}

fn test_pipeline_sum_variant_cast_from_selector_field_extracts_payload() {
	csrc := cleanc_csrc_for_test_source('selector_field_sum_cast', 'module main

struct ArrayDecompose {
	value int
}

struct Other {}

type Expr = ArrayDecompose | Other

struct CallArg {
	expr Expr
}

fn (a []CallArg) last() CallArg {
	return a[0]
}

fn f(args []CallArg) int {
	if args.len > 0 && args.last().expr is ArrayDecompose {
		array_decompose := args.last().expr as ArrayDecompose
		return array_decompose.value
	}
	return 0
}
')
	assert csrc.contains('._data._ArrayDecompose'), csrc
	assert !csrc.contains('((ArrayDecompose)((*(CallArg*)array__last(args)).expr))'), csrc
	assert !csrc.contains('((main__ArrayDecompose)((*(main__CallArg*)array__last(args)).expr))'), csrc
}

fn test_deref_expr_type_ignores_stale_pointer_env_type() {
	mut env := types.Environment.new()
	env.set_expr_type(61, types.Type(types.Pointer{
		base_type: types.Struct{
			name: 'ast__Stmt'
		}
	}))
	mut gen := Gen.new_with_env([], env)
	stmt_ptr_typ := ast.Expr(ast.PrefixExpr{
		op:   .amp
		expr: ast.Expr(ast.SelectorExpr{
			lhs: ast.Expr(ast.Ident{
				name: 'ast'
			})
			rhs: ast.Ident{
				name: 'Stmt'
			}
		})
	})
	deref_expr := ast.Expr(ast.PrefixExpr{
		op:   .mul
		expr: ast.Expr(ast.CastExpr{
			typ:  stmt_ptr_typ
			expr: ast.Expr(ast.Ident{
				name: 'ptr'
			})
		})
		pos:  token.Pos{
			id: 61
		}
	})
	assert gen.get_expr_type(deref_expr) == 'ast__Stmt'
	assert !gen.expr_is_pointer(deref_expr)

	gen.expr(ast.Expr(ast.SelectorExpr{
		lhs: deref_expr
		rhs: ast.Ident{
			name: '_tag'
		}
	}))
	out := gen.sb.str()
	assert out.contains(')._tag')
	assert !out.contains(')->_tag')
}

fn test_sum_cast_from_array_last_uses_value_separator() {
	mut gen := Gen.new([])
	gen.cur_import_modules['ast'] = 'ast'
	gen.sum_type_variants['ast__Stmt'] = ['ExprStmt', 'ReturnStmt']
	gen.struct_field_types['ast__IfBranch.stmts'] = 'Array_ast__Stmt'
	gen.remember_runtime_local_type('branch', 'ast__IfBranch')
	stmts_expr := ast.Expr(ast.SelectorExpr{
		lhs: ast.Expr(ast.Ident{
			name: 'branch'
		})
		rhs: ast.Ident{
			name: 'stmts'
		}
	})
	last_expr := ast.Expr(ast.CallExpr{
		lhs:  ast.Expr(ast.Ident{
			name: 'array__last'
		})
		args: [stmts_expr]
	})
	assert gen.get_expr_type(last_expr) == 'ast__Stmt'
	assert !gen.expr_is_pointer(last_expr)

	gen.expr(ast.Expr(ast.AsCastExpr{
		expr: last_expr
		typ:  ast.Expr(ast.SelectorExpr{
			lhs: ast.Expr(ast.Ident{
				name: 'ast'
			})
			rhs: ast.Ident{
				name: 'ExprStmt'
			}
		})
	}))
	out := gen.sb.str()
	assert out.contains(')._data._ExprStmt')
	assert !out.contains(')->_data._ExprStmt')

	mut method_gen := Gen.new([])
	method_gen.cur_import_modules['ast'] = 'ast'
	method_gen.sum_type_variants['ast__Stmt'] = ['ExprStmt', 'ReturnStmt']
	method_gen.struct_field_types['ast__IfBranch.stmts'] = 'Array_ast__Stmt'
	method_gen.remember_runtime_local_type('branch', 'ast__IfBranch')
	method_last_expr := ast.Expr(ast.CallExpr{
		lhs: ast.Expr(ast.SelectorExpr{
			lhs: stmts_expr
			rhs: ast.Ident{
				name: 'last'
			}
		})
	})
	assert method_gen.get_expr_type(method_last_expr) == 'ast__Stmt'
	assert !method_gen.expr_is_pointer(method_last_expr)
	method_gen.expr(ast.Expr(ast.AsCastExpr{
		expr: method_last_expr
		typ:  ast.Expr(ast.SelectorExpr{
			lhs: ast.Expr(ast.Ident{
				name: 'ast'
			})
			rhs: ast.Ident{
				name: 'ExprStmt'
			}
		})
	}))
	method_out := method_gen.sb.str()
	assert method_out.contains(')._data._ExprStmt')
	assert !method_out.contains(')->_data._ExprStmt')

	mut env := types.Environment.new()
	env.set_expr_type(62, types.Type(types.Pointer{
		base_type: types.Struct{
			name: 'ast__Stmt'
		}
	}))
	mut env_gen := Gen.new_with_env([], env)
	env_gen.cur_import_modules['ast'] = 'ast'
	env_gen.sum_type_variants['ast__Stmt'] = ['ExprStmt', 'ReturnStmt']
	env_gen.struct_field_types['ast__IfBranch.stmts'] = 'Array_ast__Stmt'
	env_gen.remember_runtime_local_type('branch', 'ast__IfBranch')
	env_last_expr := ast.Expr(ast.CallExpr{
		lhs: ast.Expr(ast.SelectorExpr{
			lhs: stmts_expr
			rhs: ast.Ident{
				name: 'last'
			}
		})
		pos: token.Pos{
			id: 62
		}
	})
	assert env_gen.get_expr_type(env_last_expr) == 'ast__Stmt'
	assert !env_gen.expr_is_pointer(env_last_expr)
	env_gen.expr(ast.Expr(ast.SelectorExpr{
		lhs: ast.Expr(ast.AsCastExpr{
			expr: env_last_expr
			typ:  ast.Expr(ast.SelectorExpr{
				lhs: ast.Expr(ast.Ident{
					name: 'ast'
				})
				rhs: ast.Ident{
					name: 'ExprStmt'
				}
			})
		})
		rhs: ast.Ident{
			name: 'typ'
		}
	}))
	env_out := env_gen.sb.str()
	assert env_out.contains(')._data._ExprStmt')
	assert !env_out.contains(')->_data._ExprStmt')
}

fn test_selector_on_array_last_pointer_element_uses_arrow() {
	mut gen := Gen.new([])
	gen.struct_field_types['builder__Builder.parsed_files'] = 'Array_ast__Fileptr'
	gen.remember_runtime_local_type('v', 'builder__Builder*')
	parsed_files := ast.Expr(ast.SelectorExpr{
		lhs: ast.Expr(ast.Ident{
			name: 'v'
		})
		rhs: ast.Ident{
			name: 'parsed_files'
		}
	})
	last_expr := ast.Expr(ast.CallExpr{
		lhs:  ast.Expr(ast.Ident{
			name: 'array__last'
		})
		args: [parsed_files]
	})
	assert gen.infer_array_method_elem_type(last_expr) == 'ast__File*'
	assert gen.expr_is_pointer(last_expr)
	gen.expr(ast.Expr(ast.SelectorExpr{
		lhs: last_expr
		rhs: ast.Ident{
			name: 'path'
		}
	}))
	out := gen.sb.str()
	assert out.contains('array__last(v->parsed_files))->path'), out
	assert !out.contains('array__last(v->parsed_files)).path'), out
}

fn test_if_expr_type_prefers_matching_concrete_branch_types_over_voidptr_env_type() {
	mut env := types.Environment.new()
	env.set_expr_type(71, types.Type(types.Pointer{
		base_type: types.Type(types.void_)
	}))
	mut gen := Gen.new_with_env([], env)
	gen.fn_return_types['next_value_id'] = 'int'
	if_expr := ast.IfExpr{
		cond:      ast.Expr(ast.BasicLiteral{
			kind:  .key_true
			value: 'true'
		})
		stmts:     [
			ast.Stmt(ast.ExprStmt{
				expr: ast.Expr(ast.CallExpr{
					lhs: ast.Expr(ast.Ident{
						name: 'next_value_id'
					})
				})
			}),
		]
		else_expr: ast.Expr(ast.CallExpr{
			lhs: ast.Expr(ast.Ident{
				name: 'next_value_id'
			})
		})
		pos:       token.Pos{
			id: 71
		}
	}
	assert gen.get_if_expr_type(&if_expr) == 'int'
}

fn test_if_expr_value_wraps_concrete_branch_for_sum_type_temp() {
	mut env := types.Environment.new()
	env.set_expr_type(81, types.Type(types.SumType{
		name:     'ast__Expr'
		variants: [
			types.Type(types.Struct{
				name: 'ast__SqlExpr'
			}),
		]
	}))
	mut gen := Gen.new_with_env([], env)
	gen.sum_type_variants['ast__Expr'] = ['ast__SqlExpr']
	gen.remember_runtime_local_type('sql_expr', 'ast__SqlExpr')
	gen.remember_runtime_local_type('empty_expr', 'ast__Expr')
	if_expr := ast.IfExpr{
		cond:      ast.Expr(ast.BasicLiteral{
			kind:  .key_true
			value: 'true'
		})
		stmts:     [
			ast.Stmt(ast.ExprStmt{
				expr: ast.Expr(ast.Ident{
					name: 'sql_expr'
				})
			}),
		]
		else_expr: ast.Expr(ast.Ident{
			name: 'empty_expr'
		})
		pos:       token.Pos{
			id: 81
		}
	}
	gen.gen_if_expr_value(&if_expr)
	out := gen.sb.str()
	assert out.contains('ast__Expr _if_expr_t')
	assert out.contains('._tag = 0')
	assert !out.contains('_if_expr_t0 = sql_expr;')
}

fn test_selector_field_smartcast_uses_declared_sum_type_payload() {
	mut env := types.Environment.new()
	env.set_expr_type(63, types.Type(types.Struct{
		name: 'ast__Ident'
	}))
	mut gen := Gen.new_with_env([], env)
	gen.sum_type_variants['ast__Expr'] = ['Ident', 'IntegerLiteral']
	gen.struct_field_types['ast__EnumField.expr'] = 'ast__Expr'
	gen.struct_field_types['ast__Ident.language'] = 'ast__Language'
	gen.struct_field_types['ast__Ident.kind'] = 'ast__IdentKind'
	gen.remember_runtime_local_type('field', 'ast__EnumField*')
	field_expr := ast.Expr(ast.SelectorExpr{
		lhs: ast.Expr(ast.Ident{
			name: 'field'
		})
		rhs: ast.Ident{
			name: 'expr'
		}
		pos: token.Pos{
			id: 63
		}
	})
	assert gen.selector_declared_field_type(field_expr as ast.SelectorExpr) == 'ast__Expr'

	gen.expr(ast.Expr(ast.SelectorExpr{
		lhs: field_expr
		rhs: ast.Ident{
			name: 'language'
		}
	}))
	out := gen.sb.str()
	assert out.contains('(field->expr)._data._Ident')
	assert out.contains('->language')
	assert !out.contains('field->expr.language')

	mut qualified_gen := Gen.new_with_env([], env)
	qualified_gen.sum_type_variants['ast__Expr'] = ['ast__Ident', 'ast__IntegerLiteral']
	qualified_gen.struct_field_types['ast__EnumField.expr'] = 'ast__Expr'
	qualified_gen.struct_field_types['ast__Ident.language'] = 'ast__Language'
	qualified_gen.remember_runtime_local_type('field', 'ast__EnumField*')
	qualified_gen.expr(ast.Expr(ast.SelectorExpr{
		lhs: field_expr
		rhs: ast.Ident{
			name: 'language'
		}
	}))
	qualified_out := qualified_gen.sb.str()
	assert qualified_out.contains('->language')
	assert !qualified_out.contains('field->expr.language')

	mut no_env_gen := Gen.new([])
	no_env_gen.sum_type_variants['ast__Expr'] = ['Ident', 'IntegerLiteral']
	no_env_gen.struct_field_types['ast__EnumField.expr'] = 'ast__Expr'
	no_env_gen.struct_field_types['ast__Ident.language'] = 'ast__Language'
	no_env_gen.remember_runtime_local_type('field', 'ast__EnumField*')
	no_env_gen.expr(ast.Expr(ast.SelectorExpr{
		lhs: field_expr
		rhs: ast.Ident{
			name: 'language'
		}
	}))
	no_env_out := no_env_gen.sb.str()
	assert no_env_out.contains('->language')
	assert !no_env_out.contains('field->expr.language')

	mut const_field_gen := Gen.new([])
	const_field_gen.sum_type_variants['ast__Expr'] = ['Ident', 'IntegerLiteral']
	const_field_gen.struct_field_types['ast__ConstField.expr'] = 'ast__Expr'
	const_field_gen.struct_field_types['ast__Ident.language'] = 'ast__Language'
	const_field_gen.remember_runtime_local_type('field', 'ast__ConstField*')
	const_field_gen.expr(ast.Expr(ast.SelectorExpr{
		lhs: field_expr
		rhs: ast.Ident{
			name: 'language'
		}
	}))
	const_field_out := const_field_gen.sb.str()
	assert const_field_out.contains('->language')
	assert !const_field_out.contains('field->expr.language')
}

fn test_selector_method_receiver_prefers_declared_field_type_over_env_fn_type() {
	mut env := types.Environment.new()
	env.set_expr_type(91, types.Type(types.FnType{}))
	env.set_expr_type(92, types.Type(types.FnType{}))
	mut gen := Gen.new_with_env([], env)
	gen.struct_field_types['Holder.typ'] = 'ast__Type'
	gen.remember_runtime_local_type('holder', 'Holder')
	gen.fn_return_types['ast__Type__is_ptr'] = 'bool'
	gen.fn_param_types['ast__Type__is_ptr'] = ['ast__Type']
	gen.fn_param_is_ptr['ast__Type__is_ptr'] = [false]
	receiver := ast.SelectorExpr{
		lhs: ast.Expr(ast.Ident{
			name: 'holder'
		})
		rhs: ast.Ident{
			name: 'typ'
		}
		pos: token.Pos{
			id: 91
		}
	}
	assert gen.method_receiver_base_type(ast.Expr(receiver)) == 'ast__Type'
	gen.call_expr(ast.Expr(ast.SelectorExpr{
		lhs: ast.Expr(receiver)
		rhs: ast.Ident{
			name: 'is_ptr'
		}
		pos: token.Pos{
			id: 92
		}
	}), [])
	out := gen.sb.str()
	assert out == 'ast__Type__is_ptr(holder.typ)'
	assert !out.contains('((bool(*)())')
}

fn test_selector_method_receiver_unwraps_address_of_local_before_env_fallback() {
	mut env := types.Environment.new()
	env.set_expr_type(99, types.Type(types.Pointer{
		base_type: types.Type(types.Struct{
			name: 'printer__JSONSink'
		})
	}))
	mut gen := Gen.new_with_env([], env)
	gen.remember_runtime_local_type('sink', 'printer__SummarySink')
	gen.fn_return_types['printer__SummarySink__stats'] = '_option_printer__Statsptr'
	gen.fn_param_types['printer__SummarySink__stats'] = ['printer__SummarySink*']
	gen.fn_param_is_ptr['printer__SummarySink__stats'] = [true]
	gen.fn_return_types['printer__JSONSink__stats'] = 'printer__Stats*'
	gen.fn_param_types['printer__JSONSink__stats'] = ['printer__JSONSink*']
	gen.fn_param_is_ptr['printer__JSONSink__stats'] = [true]
	receiver := ast.Expr(ast.ParenExpr{
		expr: ast.Expr(ast.PrefixExpr{
			op:   token.Token.amp
			expr: ast.Expr(ast.Ident{
				name: 'sink'
			})
		})
		pos:  token.Pos{
			id: 99
		}
	})
	assert gen.method_receiver_base_type(receiver) == 'printer__SummarySink'
	gen.call_expr(ast.Expr(ast.SelectorExpr{
		lhs: receiver
		rhs: ast.Ident{
			name: 'stats'
		}
	}), [])
	out := gen.sb.str()
	assert out.starts_with('printer__SummarySink__stats(')
	assert !out.contains('printer__JSONSink__stats')
}

fn test_array_selector_clone_does_not_emit_fn_pointer_field_call() {
	mut env := types.Environment.new()
	env.set_expr_type(94, types.Type(types.FnType{}))
	mut gen := Gen.new_with_env([], env)
	gen.struct_field_types['Holder.types'] = 'Array_ast__Type'
	gen.remember_runtime_local_type('holder', 'Holder')
	receiver := ast.SelectorExpr{
		lhs: ast.Expr(ast.Ident{
			name: 'holder'
		})
		rhs: ast.Ident{
			name: 'types'
		}
	}
	gen.call_expr(ast.Expr(ast.SelectorExpr{
		lhs: ast.Expr(receiver)
		rhs: ast.Ident{
			name: 'clone'
		}
		pos: token.Pos{
			id: 94
		}
	}), [])
	out := gen.sb.str()
	assert out == 'array__clone_to_depth((array*)&(holder.types), 0)'
	assert !out.contains('.clone')
	assert !out.contains('((Array_ast__Type(*)())')
}

fn test_map_selector_clone_uses_builtin_map_clone() {
	mut env := types.Environment.new()
	env.set_expr_type(98, types.Type(types.FnType{}))
	mut gen := Gen.new_with_env([], env)
	gen.struct_field_types['Holder.cache'] = 'Map_string_ssa__TypeID'
	gen.remember_runtime_local_type('holder', 'Holder')
	gen.fn_return_types['map__clone'] = 'map'
	gen.fn_param_types['map__clone'] = ['map*']
	gen.fn_param_is_ptr['map__clone'] = [true]
	receiver := ast.SelectorExpr{
		lhs: ast.Expr(ast.Ident{
			name: 'holder'
		})
		rhs: ast.Ident{
			name: 'cache'
		}
	}
	gen.call_expr(ast.Expr(ast.SelectorExpr{
		lhs: ast.Expr(receiver)
		rhs: ast.Ident{
			name: 'clone'
		}
		pos: token.Pos{
			id: 98
		}
	}), [])
	out := gen.sb.str()
	assert out.starts_with('map__clone(')
	assert out.contains('holder.cache')
	assert !out.contains('Map_string_ssa__TypeID__clone')
}

fn test_lowered_array_push_single_array_arg_emits_push_many() {
	mut gen := Gen.new([])
	gen.remember_runtime_local_type('matches', 'Array_usize')
	gen.remember_runtime_local_type('hits', 'Array_usize')
	gen.fn_param_types['array__push'] = ['array*', 'voidptr']
	gen.fn_param_is_ptr['array__push'] = [true, true]
	gen.call_expr(ast.Expr(ast.Ident{
		name: 'array__push'
	}), [
		ast.Expr(ast.Ident{
			name: 'matches'
		}),
		ast.Expr(ast.ArrayInitExpr{
			typ:   ast.Expr(ast.Type(ast.ArrayType{
				elem_type: ast.Expr(ast.Ident{
					name: 'usize'
				})
			}))
			exprs: [
				ast.Expr(ast.Ident{
					name: 'hits'
				}),
			]
		}),
	])
	out := gen.sb.str()
	assert out.contains('array__push_many(')
	assert out.contains('_arr_push_many_tmp_')
	assert out.contains('.data')
	assert !out.contains('&(usize[1]){hits}')
}

fn test_array_selector_clone_on_rvalue_receiver_uses_temp() {
	mut env := types.Environment.new()
	env.set_expr_type(96, types.Type(types.FnType{}))
	mut gen := Gen.new_with_env([], env)
	gen.struct_field_types['Holder.types'] = 'Array_ast__Type'
	gen.sum_type_variants['Node'] = ['Holder']
	gen.remember_runtime_local_type('node', 'Node')
	receiver := ast.SelectorExpr{
		lhs: ast.Expr(ast.AsCastExpr{
			expr: ast.Expr(ast.Ident{
				name: 'node'
			})
			typ:  ast.Expr(ast.Ident{
				name: 'Holder'
			})
		})
		rhs: ast.Ident{
			name: 'types'
		}
	}
	gen.call_expr(ast.Expr(ast.SelectorExpr{
		lhs: ast.Expr(receiver)
		rhs: ast.Ident{
			name: 'clone'
		}
		pos: token.Pos{
			id: 96
		}
	}), [])
	out := gen.sb.str()
	assert out.starts_with('({ Array_ast__Type _arr_clone_tmp_')
	assert out.contains('; array__clone_to_depth((array*)&_arr_clone_tmp_')
	assert !out.contains('&((((node)._data._Holder)')
	assert !out.contains('&(((node)._data._Holder)')
}

fn test_array_selector_clone_on_map_index_receiver_uses_typed_map_get_temp() {
	mut env := types.Environment.new()
	env.set_expr_type(97, types.Type(types.FnType{}))
	mut gen := Gen.new_with_env([], env)
	gen.remember_runtime_local_type('m', 'Map_string_Array_Map_string_types__Type')
	gen.remember_runtime_local_type('key', 'string')
	receiver := ast.IndexExpr{
		lhs:  ast.Expr(ast.Ident{
			name: 'm'
		})
		expr: ast.Expr(ast.Ident{
			name: 'key'
		})
	}
	gen.call_expr(ast.Expr(ast.SelectorExpr{
		lhs: ast.Expr(receiver)
		rhs: ast.Ident{
			name: 'clone'
		}
		pos: token.Pos{
			id: 97
		}
	}), [])
	out := gen.sb.str()
	assert out.starts_with('({ Array_Map_string_types__Type _arr_clone_tmp_')
	assert out.contains('map__get(&(m), (void*)&_map_key')
	assert out.contains('; array__clone_to_depth((array*)&_arr_clone_tmp_')
	assert !out.contains('cannot resolve map type for index expr')
	assert !out.contains('array__clone_to_depth((array*)&((')
}

fn test_array_selector_contains_does_not_emit_fn_pointer_field_call() {
	mut env := types.Environment.new()
	env.set_expr_type(95, types.Type(types.FnType{}))
	mut gen := Gen.new_with_env([], env)
	gen.struct_field_types['Holder.types'] = 'Array_ast__Type'
	gen.remember_runtime_local_type('holder', 'Holder')
	gen.remember_runtime_local_type('typ', 'ast__Type')
	receiver := ast.SelectorExpr{
		lhs: ast.Expr(ast.Ident{
			name: 'holder'
		})
		rhs: ast.Ident{
			name: 'types'
		}
	}
	gen.call_expr(ast.Expr(ast.SelectorExpr{
		lhs: ast.Expr(receiver)
		rhs: ast.Ident{
			name: 'contains'
		}
		pos: token.Pos{
			id: 95
		}
	}), [
		ast.Expr(ast.Ident{
			name: 'typ'
		}),
	])
	out := gen.sb.str()
	assert out == 'array__contains(holder.types, &typ)'
	assert !out.contains('.contains')
	assert !out.contains('((bool(*)())')
}

fn test_fixed_array_contains_call_is_inlined() {
	mut gen := Gen.new([])
	gen.remember_runtime_local_type('chars', 'Array_fixed_rune_4')
	gen.remember_runtime_local_type('ch', 'rune')
	gen.call_expr(ast.Expr(ast.Ident{
		name: 'Array_fixed_rune_4_contains'
	}), [
		ast.Expr(ast.Ident{
			name: 'chars'
		}),
		ast.Expr(ast.Ident{
			name: 'ch'
		}),
	])
	out := gen.sb.str()
	assert out.contains('for (int _i = 0; _i < 4; _i++)')
	assert out.contains('chars[_i] == ch')
	assert !out.contains('Array_fixed_rune_4_contains(')
}

fn test_new_array_from_c_array_elem_type_uses_sizeof_arg() {
	mut gen := Gen.new([])
	elem := gen.infer_array_elem_type_from_expr(ast.Expr(ast.CallExpr{
		lhs:  ast.Expr(ast.Ident{
			name: 'new_array_from_c_array'
		})
		args: [
			ast.Expr(ast.BasicLiteral{
				value: '4'
				kind:  .number
			}),
			ast.Expr(ast.BasicLiteral{
				value: '4'
				kind:  .number
			}),
			ast.Expr(ast.KeywordOperator{
				op:    .key_sizeof
				exprs: [
					ast.Expr(ast.Ident{
						name: 'u8'
					}),
				]
			}),
			ast.Expr(ast.PrefixExpr{
				op:   .amp
				expr: ast.Expr(ast.Ident{
					name: 'msg'
				})
			}),
		]
	}))
	assert elem == 'u8'
}

fn test_struct_fn_type_field_emits_callable_c_field() {
	mut env := types.Environment.new()
	mut http_scope := types.new_scope(unsafe { nil })
	http_scope.insert('Server', types.Type(types.Struct{
		name: 'http__Server'
	}))
	lock env.scopes {
		env.scopes['http'] = http_scope
	}
	mut gen := Gen.new_with_env([], env)
	gen.cur_module = 'http'
	gen.gen_struct_decl(ast.StructDecl{
		name:   'Server'
		fields: [
			ast.FieldDecl{
				name: 'on_closed'
				typ:  ast.Expr(ast.Type(ast.FnType{
					params: [
						ast.Parameter{
							name:   's'
							typ:    ast.Expr(ast.Ident{
								name: 'Server'
							})
							is_mut: true
						},
					]
				}))
			},
		]
	})
	out := gen.sb.str()
	assert out.contains('void (*on_closed)(http__Server*);')
	assert !out.contains('void* on_closed;')
}

fn test_struct_decl_emits_late_dynamic_array_alias() {
	mut gen := Gen.new([])
	gen.gen_struct_decl(ast.StructDecl{
		name:   'Holder'
		fields: [
			ast.FieldDecl{
				name: 'items'
				typ:  ast.Expr(ast.Type(ast.ArrayType{
					elem_type: ast.Expr(ast.Ident{
						name: 'Foo'
					})
				}))
			},
		]
	})
	out := gen.sb.str()
	alias_pos := out.index('typedef array Array_Foo;') or {
		assert false, out
		return
	}
	struct_pos := out.index('struct Holder {') or {
		assert false, out
		return
	}
	assert alias_pos < struct_pos
	assert out.contains('\tArray_Foo items;')
}

fn test_selector_method_receiver_uses_sum_common_field_type() {
	mut env := types.Environment.new()
	env.set_expr_type(93, types.Type(types.FnType{}))
	mut gen := Gen.new_with_env([], env)
	gen.sum_type_variants['ast__ScopeObject'] = ['EmptyScopeObject', 'Var']
	gen.struct_field_types['ast__EmptyScopeObject.typ'] = 'ast__Type'
	gen.struct_field_types['EmptyScopeObject.typ'] = 'ast__Type'
	gen.struct_field_types['ast__Var.typ'] = 'ast__Type'
	gen.struct_field_types['Var.typ'] = 'ast__Type'
	gen.remember_runtime_local_type('obj', 'ast__ScopeObject')
	gen.fn_return_types['ast__Type__is_ptr'] = 'bool'
	gen.fn_param_types['ast__Type__is_ptr'] = ['ast__Type']
	gen.fn_param_is_ptr['ast__Type__is_ptr'] = [false]
	receiver := ast.SelectorExpr{
		lhs: ast.Expr(ast.Ident{
			name: 'obj'
		})
		rhs: ast.Ident{
			name: 'typ'
		}
	}
	assert gen.method_receiver_base_type(ast.Expr(receiver)) == 'ast__Type'
	gen.call_expr(ast.Expr(ast.SelectorExpr{
		lhs: ast.Expr(receiver)
		rhs: ast.Ident{
			name: 'is_ptr'
		}
		pos: token.Pos{
			id: 93
		}
	}), [])
	out := gen.sb.str()
	assert out.contains('ast__Type__is_ptr(')
	assert out.contains('switch (_sum_cf')
	assert !out.contains('((bool(*)())')
}

fn test_call_arg_auto_derefs_runtime_local_pointer_for_value_param() {
	mut gen := Gen.new([])
	gen.fn_param_types['use_arg'] = ['ast__AsmArg']
	gen.remember_runtime_local_type('arg', 'ast__AsmArg*')
	gen.gen_call_arg('use_arg', 0, ast.Ident{
		name: 'arg'
	})
	assert gen.sb.str() == '(*arg)'
}

fn test_selector_array_field_is_array_value() {
	mut gen := Gen.new([])
	gen.struct_field_types['Holder.values'] = 'Array_int'
	gen.remember_runtime_local_type('holder', 'Holder*')
	sel := ast.SelectorExpr{
		lhs: ast.Expr(ast.Ident{
			name: 'holder'
		})
		rhs: ast.Ident{
			name: 'values'
		}
	}
	assert gen.expr_is_array_value(ast.Expr(sel))
	assert gen.expr_array_runtime_type(ast.Expr(sel)) == 'Array_int'
}

fn test_fixed_array_elem_type_ready_accepts_primitive_alias() {
	mut g := Gen.new([])
	g.primitive_type_aliases['sha3__Lane'] = true
	assert g.fixed_array_elem_type_ready('sha3__Lane')
}

fn test_fixed_array_elem_type_ready_waits_for_alias_base() {
	mut g := Gen.new([])
	g.alias_base_types['foo__Alias'] = 'foo__Thing'
	assert !g.fixed_array_elem_type_ready('foo__Alias')
	g.emitted_types['body_foo__Thing'] = true
	assert g.fixed_array_elem_type_ready('foo__Alias')
}

fn test_types_type_to_c_prefixes_declared_c_structs() {
	mut g := Gen.new([])
	g.c_struct_types['kevent'] = true
	assert g.types_type_to_c(types.Type(types.Struct{
		name: 'kevent'
	})) == 'struct kevent'

	g.typedef_c_types['kevent'] = true
	assert g.types_type_to_c(types.Type(types.Struct{
		name: 'kevent'
	})) == 'kevent'
}

fn test_specialized_generic_named_pointer_token_resolves_to_c_pointer_type() {
	mut g := Gen.new([
		ast.File{
			stmts: [
				ast.Stmt(ast.ModuleStmt{
					name: 'ast'
				}),
				ast.Stmt(ast.StructDecl{
					name: 'File'
				}),
			]
		},
	])
	assert g.types_type_to_c(types.Type(types.NamedType('ast_Fileptr'))) == 'ast__File*'
	assert g.types_type_to_c(types.Type(types.Struct{
		name: 'ast_Fileptr'
	})) == 'ast__File*'

	get_item := ast.FnDecl{
		name:      'get_item'
		is_method: true
		receiver:  ast.Parameter{
			name: 'pool'
			typ:  ast.Expr(ast.Ident{
				name: 'PoolProcessor'
			})
		}
		typ:       ast.FnType{
			generic_params: [
				ast.Expr(ast.Ident{
					name: 'T'
				}),
			]
			params:         [
				ast.Parameter{
					name: 'idx'
					typ:  ast.Expr(ast.Ident{
						name: 'int'
					})
				},
			]
			return_type:    ast.Expr(ast.Ident{
				name: 'T'
			})
		}
		language:  .v
	}
	g.active_generic_types['T'] = types.Type(types.NamedType('ast_Fileptr'))
	g.register_fn_signature(get_item, 'pool__PoolProcessor__get_item_T_ast_Fileptr')
	assert g.fn_return_types['pool__PoolProcessor__get_item_T_ast_Fileptr'] == 'ast__File*'
}

fn test_generic_struct_instance_lookup_mangles_c_struct_type_keys() {
	mut g := Gen.new([])
	g.generic_struct_instances['Box'] = [
		GenericStructInstance{
			params_key: 'int'
			c_name:     'Box'
		},
		GenericStructInstance{
			params_key: 'struct_cJSON'
			c_name:     'Box_T_struct_cJSON'
		},
	]

	resolved := g.resolve_generic_struct_c_name('Box', [
		ast.Expr(ast.SelectorExpr{
			lhs: ast.Expr(ast.Ident{
				name: 'C'
			})
			rhs: ast.Ident{
				name: 'cJSON'
			}
		}),
	])
	assert resolved == 'Box_T_struct_cJSON'
}

fn test_cache_generic_concrete_origin_accepts_unqualified_declared_type() {
	mut g := Gen.new([
		ast.File{
			stmts: [
				ast.Stmt(ast.ModuleStmt{
					name: 'main'
				}),
				ast.Stmt(ast.StructDecl{
					name: 'FileInfo'
				}),
			]
		},
	])
	g.cache_bundle_name = 'virtuals'
	g.emit_modules['main'] = true
	g.type_modules['main'] = true

	assert g.generic_concrete_c_name_belongs_to_emit_modules('FileInfo')
	assert g.generic_concrete_c_name_belongs_to_emit_modules('Array_FileInfo')
	assert g.generic_specialization_belongs_to_emit_modules({
		'T': types.Type(types.Array{
			elem_type: types.Type(types.Struct{
				name: 'FileInfo'
			})
		})
	})
}

fn test_weak_generic_emit_file_filter_skips_uncalled_cached_module_types() {
	api_file := ast.File{
		name:  '/tmp/v2_app/api/api.v'
		stmts: [
			ast.Stmt(ast.ModuleStmt{
				name: 'api'
			}),
			ast.Stmt(ast.StructDecl{
				name: 'ApiSuccessResponse'
			}),
		]
	}
	http_file := ast.File{
		name:  '/tmp/v2_cache/http/request.v'
		stmts: [
			ast.Stmt(ast.ModuleStmt{
				name: 'http'
			}),
			ast.Stmt(ast.StructDecl{
				name: 'Request'
			}),
		]
	}
	mut g := Gen.new([api_file, http_file])
	g.set_emit_files([api_file.name])

	assert g.weak_generic_specialization_belongs_to_emit_files({
		'T': types.Type(types.Struct{
			name: 'api__ApiSuccessResponse'
		})
	})
	assert !g.weak_generic_specialization_belongs_to_emit_files({
		'T': types.Type(types.Struct{
			name: 'http__Request'
		})
	})
	assert !g.weak_generic_specialization_belongs_to_emit_files({
		'T': types.Type(types.Array{
			elem_type: types.Type(types.Struct{
				name: 'http__Request'
			})
		})
	})
}

fn test_weak_generic_filter_uses_root_called_names() {
	t_ident := ast.Expr(ast.Ident{
		name: 'T'
	})
	decl := ast.FnDecl{
		name: 'encode'
		typ:  ast.FnType{
			generic_params: [t_ident]
		}
	}
	json_file := ast.File{
		mod:   'json2'
		name:  '/tmp/v2_cache/json2.v'
		stmts: [ast.Stmt(decl)]
	}
	api_file := ast.File{
		mod:   'api'
		name:  '/tmp/v2_app/api/api.v'
		stmts: [
			ast.Stmt(ast.ModuleStmt{
				name: 'api'
			}),
			ast.Stmt(ast.StructDecl{
				name: 'ApiSuccessResponse'
			}),
		]
	}
	http_file := ast.File{
		mod:   'http'
		name:  '/tmp/v2_cache/http/request.v'
		stmts: [
			ast.Stmt(ast.ModuleStmt{
				name: 'http'
			}),
			ast.Stmt(ast.StructDecl{
				name: 'Request'
			}),
		]
	}
	api_type := types.Type(types.Struct{
		name: 'api__ApiSuccessResponse'
	})
	http_type := types.Type(types.Struct{
		name: 'http__Request'
	})
	mut g := Gen.new_with_env([api_file, http_file, json_file], types.Environment.new())
	g.set_emit_files([api_file.name])
	g.record_late_generic_call_spec('json2__encode', {
		'T': api_type
	})
	g.record_late_generic_call_spec('json2__encode', {
		'T': http_type
	})
	g.set_file_module(json_file)
	api_name := g.specialized_fn_name(decl, {
		'T': api_type
	})
	http_name := g.specialized_fn_name(decl, {
		'T': http_type
	})
	assert api_name != ''
	assert http_name != ''
	mut needed_names := map[string]bool{}
	needed_names[api_name] = true
	needed_names[http_name] = true
	g.called_fn_names[http_name] = true
	mut root_called_names := map[string]bool{}
	specs := g.weak_generic_fn_specializations_for_names(&decl, needed_names, root_called_names)
	mut names := map[string]bool{}
	for spec in specs {
		names[spec.name] = true
	}
	assert api_name in names
	assert http_name !in names
	root_called_names[http_name] = true
	specs2 := g.weak_generic_fn_specializations_for_names(&decl, needed_names, root_called_names)
	mut names2 := map[string]bool{}
	for spec in specs2 {
		names2[spec.name] = true
	}
	assert http_name in names2
}

fn test_cache_generic_concrete_origin_rejects_unqualified_name_from_qualified_module() {
	mut env := types.Environment.new()
	mut veb_scope := types.new_scope(unsafe { nil })
	veb_scope.insert('Context', types.Type(types.Struct{
		name: 'veb__Context'
	}))
	lock env.scopes {
		env.scopes['veb'] = veb_scope
	}
	mut g := Gen.new_with_env([
		ast.File{
			stmts: [
				ast.Stmt(ast.ModuleStmt{
					name: 'veb'
				}),
				ast.Stmt(ast.StructDecl{
					name: 'Context'
				}),
			]
		},
	], env)
	g.cache_bundle_name = 'veb'
	g.emit_modules['veb'] = true
	g.type_modules['veb'] = true

	assert !g.generic_concrete_c_name_belongs_to_emit_modules('Context')
	assert g.generic_concrete_c_name_belongs_to_emit_modules('veb__Context')
}

fn test_cache_generic_struct_bindings_follow_emit_module_filter() {
	prefs := &vpref.Preferences{
		backend: .cleanc
	}
	mut env := types.Environment.new()
	mut arrays_scope := types.new_scope(unsafe { nil })
	arrays_scope.insert('ReverseIterator', types.Type(types.Struct{
		name:           'ReverseIterator'
		generic_params: ['T']
	}))
	arrays_scope.insert('Part', types.Type(types.Struct{
		name: 'arrays__Part'
	}))
	lock env.scopes {
		env.scopes['arrays'] = arrays_scope
	}
	main_file := ast.File{
		mod:   'main'
		stmts: [
			ast.Stmt(ast.StructDecl{
				name: 'GitHubRepoInfo'
			}),
		]
	}
	mut g := Gen.new_with_env_and_pref([main_file], env, prefs)
	g.cache_bundle_name = 'veb'
	g.cur_module = 'arrays'
	g.emit_modules['arrays'] = true
	g.type_modules['arrays'] = true
	g.record_generic_struct_bindings('ReverseIterator', 'arrays__ReverseIterator', [
		ast.Expr(ast.Ident{
			name: 'GitHubRepoInfo'
		}),
	])
	assert 'arrays__ReverseIterator' !in g.generic_struct_instances
	g.record_generic_struct_bindings('ReverseIterator', 'arrays__ReverseIterator', [
		ast.Expr(ast.Ident{
			name: 'Part'
		}),
	])
	assert g.generic_struct_instances['arrays__ReverseIterator'].len == 1
	g.generic_struct_instances.clear()
	env.generic_types['reverse_iterator'] = [
		{
			'T': types.Type(types.Struct{
				name: 'GitHubRepoInfo'
			})
		},
		{
			'T': types.Type(types.Struct{
				name: 'arrays__Part'
			})
		},
	]
	fallback := g.fallback_generic_bindings_for_names(['T']) or { panic('expected fallback') }
	fallback_t := fallback['T'] or { panic('expected T fallback') }
	assert fallback_t.name() == 'arrays__Part'
}

fn test_generic_fallback_requires_selected_fields_on_generic_param() {
	mut g := Gen.new([])
	node := ast.FnDecl{
		name:  'needs_req'
		typ:   ast.FnType{
			generic_params: [
				ast.Expr(ast.Ident{
					name: 'X'
				}),
			]
			params:         [
				ast.Parameter{
					name: 'ctx'
					typ:  ast.Expr(ast.Ident{
						name: 'X'
					})
				},
			]
		}
		stmts: [
			ast.Stmt(ast.ExprStmt{
				expr: ast.Expr(ast.SelectorExpr{
					lhs: ast.Expr(ast.Ident{
						name: 'ctx'
					})
					rhs: ast.Ident{
						name: 'req'
					}
				})
			}),
		]
	}
	assert !g.accepts_broad_generic_fallback_type(node, types.Type(types.i64_))
	assert g.accepts_broad_generic_fallback_type(node, types.Type(types.Struct{
		name:   'Context'
		fields: [
			types.Field{
				name: 'req'
				typ:  types.Type(types.string_)
			},
		]
	}))
}

fn test_generic_fallback_ignores_comptime_field_selectors() {
	mut g := Gen.new([])
	node := ast.FnDecl{
		name:  'encode_struct_fields'
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
						name: 'T'
					})
				},
			]
		}
		stmts: [
			ast.Stmt(ast.ExprStmt{
				expr: ast.Expr(ast.SelectorExpr{
					lhs: ast.Expr(ast.Ident{
						name: 'val'
					})
					rhs: ast.Ident{
						name: '__comptime_selector__'
					}
				})
			}),
		]
	}
	assert g.accepts_broad_generic_fallback_type(node, types.Type(types.Struct{
		name: 'Config'
	}))
}

fn test_generic_fallback_ignores_members_inside_comptime_if() {
	mut g := Gen.new([])
	node := ast.FnDecl{
		name:  'encode_value'
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
						name: 'T'
					})
				},
			]
		}
		stmts: [
			ast.Stmt(ast.ExprStmt{
				expr: ast.Expr(ast.ComptimeExpr{
					expr: ast.Expr(ast.IfExpr{
						cond:  ast.Expr(ast.Ident{
							name: 'T_is_array'
						})
						stmts: [
							ast.Stmt(ast.ExprStmt{
								expr: ast.Expr(ast.SelectorExpr{
									lhs: ast.Expr(ast.Ident{
										name: 'val'
									})
									rhs: ast.Ident{
										name: 'len'
									}
								})
							}),
						]
					})
				})
			}),
		]
	}
	assert g.accepts_broad_generic_fallback_type(node, types.Type(types.Struct{
		name: 'Config'
	}))
}

fn test_generic_fallback_requires_struct_for_unconditional_type_fields_loop() {
	mut g := Gen.new([])
	node := ast.FnDecl{
		name:  'fields_of_t'
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
						name: 'T'
					})
				},
			]
		}
		stmts: [
			ast.Stmt(ast.ComptimeStmt{
				stmt: ast.Stmt(ast.ForStmt{
					init: ast.Stmt(ast.ForInStmt{
						value: ast.Expr(ast.Ident{
							name: 'field'
						})
						expr:  ast.Expr(ast.SelectorExpr{
							lhs: ast.Expr(ast.Ident{
								name: 'T'
							})
							rhs: ast.Ident{
								name: 'fields'
							}
						})
					})
				})
			}),
		]
	}
	assert !g.accepts_broad_generic_fallback_type(node, types.Type(types.Pointer{
		base_type: types.Type(types.void_)
	}))
	assert g.accepts_broad_generic_fallback_type(node, types.Type(types.Struct{
		name: 'Config'
	}))
}

fn test_generic_fallback_rejects_unsupported_generic_value_operators() {
	mut g := Gen.new([])
	array_of_t := ast.Expr(ast.Type(ast.ArrayType{
		elem_type: ast.Expr(ast.Ident{
			name: 'T'
		})
	}))
	min_node := ast.FnDecl{
		name:  'min'
		typ:   ast.FnType{
			generic_params: [
				ast.Expr(ast.Ident{
					name: 'T'
				}),
			]
			params:         [
				ast.Parameter{
					name: 'values'
					typ:  array_of_t
				},
			]
		}
		stmts: [
			ast.Stmt(ast.AssignStmt{
				op:  .decl_assign
				lhs: [ast.Expr(ast.Ident{
					name: 'val'
				})]
				rhs: [
					ast.Expr(ast.IndexExpr{
						lhs:  ast.Expr(ast.Ident{
							name: 'values'
						})
						expr: ast.Expr(ast.BasicLiteral{
							kind:  .number
							value: '0'
						})
					}),
				]
			}),
			ast.Stmt(ast.ForStmt{
				init:  ast.Stmt(ast.ForInStmt{
					value: ast.Expr(ast.Ident{
						name: 'e'
					})
					expr:  ast.Expr(ast.IndexExpr{
						lhs:  ast.Expr(ast.Ident{
							name: 'values'
						})
						expr: ast.Expr(ast.RangeExpr{
							start: ast.Expr(ast.BasicLiteral{
								kind:  .number
								value: '1'
							})
							end:   ast.Expr(ast.SelectorExpr{
								lhs: ast.Expr(ast.Ident{
									name: 'values'
								})
								rhs: ast.Ident{
									name: 'len'
								}
							})
						})
					})
				})
				stmts: [
					ast.Stmt(ast.ExprStmt{
						expr: ast.Expr(ast.IfExpr{
							cond: ast.Expr(ast.InfixExpr{
								op:  .lt
								lhs: ast.Expr(ast.Ident{
									name: 'e'
								})
								rhs: ast.Expr(ast.Ident{
									name: 'val'
								})
							})
						})
					}),
				]
			}),
		]
	}
	sum_node := ast.FnDecl{
		name:  'sum'
		typ:   min_node.typ
		stmts: [
			ast.Stmt(ast.AssignStmt{
				op:  .decl_assign
				lhs: [
					ast.Expr(ast.ModifierExpr{
						kind: .key_mut
						expr: ast.Expr(ast.Ident{
							name: 'head'
						})
					}),
				]
				rhs: [
					ast.Expr(ast.IndexExpr{
						lhs:  ast.Expr(ast.Ident{
							name: 'values'
						})
						expr: ast.Expr(ast.BasicLiteral{
							kind:  .number
							value: '0'
						})
					}),
				]
			}),
			ast.Stmt(ast.ForStmt{
				init:  ast.Stmt(ast.ForInStmt{
					value: ast.Expr(ast.Ident{
						name: 'e'
					})
					expr:  ast.Expr(ast.Ident{
						name: 'values'
					})
				})
				stmts: [
					ast.Stmt(ast.AssignStmt{
						op:  .plus_assign
						lhs: [ast.Expr(ast.Ident{
							name: 'head'
						})]
						rhs: [ast.Expr(ast.Ident{
							name: 'e'
						})]
					}),
				]
			}),
		]
	}
	linked_list := types.Type(types.Struct{
		name: 'json2__LinkedList'
	})

	assert !g.accepts_broad_generic_fallback_type(min_node, linked_list)
	assert !g.accepts_broad_generic_fallback_type(sum_node, linked_list)
	assert g.accepts_broad_generic_fallback_type(min_node, types.Type(types.i64_))
	assert g.accepts_broad_generic_fallback_type(sum_node, types.Type(types.i64_))
}

fn test_generic_scan_tracks_mut_declared_local_type_for_late_call_specs() {
	mut env := types.Environment.new()
	array_of_t := ast.Expr(ast.Type(ast.ArrayType{
		elem_type: ast.Expr(ast.Ident{
			name: 'T'
		})
	}))
	file := ast.File{
		mod:   'main'
		stmts: [
			ast.Stmt(ast.FnDecl{
				name: 'uniq'
				typ:  ast.FnType{
					generic_params: [
						ast.Expr(ast.Ident{
							name: 'T'
						}),
					]
					params:         [
						ast.Parameter{
							name: 'a'
							typ:  array_of_t
						},
					]
				}
			}),
			ast.Stmt(ast.FnDecl{
				name:  'keys'
				stmts: [
					ast.Stmt(ast.AssignStmt{
						op:  .decl_assign
						lhs: [
							ast.Expr(ast.ModifierExpr{
								kind: .key_mut
								expr: ast.Expr(ast.Ident{
									name: 'res'
								})
							}),
						]
						rhs: [
							ast.Expr(ast.ArrayInitExpr{
								pos: token.Pos{
									id: 1
								}
							}),
						]
					}),
					ast.Stmt(ast.ExprStmt{
						expr: ast.Expr(ast.InfixExpr{
							op:  .left_shift
							lhs: ast.Expr(ast.Ident{
								name: 'res'
							})
							rhs: ast.Expr(ast.StringLiteral{
								value: 'x'
							})
						})
					}),
					ast.Stmt(ast.ReturnStmt{
						exprs: [
							ast.Expr(ast.CallExpr{
								lhs:  ast.Expr(ast.SelectorExpr{
									lhs: ast.Expr(ast.Ident{
										name: 'arrays'
									})
									rhs: ast.Ident{
										name: 'uniq'
									}
								})
								args: [
									ast.Expr(ast.Ident{
										name: 'res'
									}),
								]
							}),
						]
					}),
				]
			}),
		]
	}
	mut g := Gen.new_with_env([file], env)
	g.build_generic_fn_decl_index()
	g.discover_direct_generic_call_specs()
	specs := g.late_generic_specs['uniq']
	assert specs.len == 1
	spec_t := specs[0]['T'] or { panic('missing T') }
	assert spec_t.name() == 'string'
}

fn test_generic_body_requirements_treat_array_param_as_container() {
	array_of_t := ast.Expr(ast.Type(ast.ArrayType{
		elem_type: ast.Expr(ast.Ident{
			name: 'T'
		})
	}))
	node := ast.FnDecl{
		name:  'uniq'
		typ:   ast.FnType{
			generic_params: [
				ast.Expr(ast.Ident{
					name: 'T'
				}),
			]
			params:         [
				ast.Parameter{
					name: 'a'
					typ:  array_of_t
				},
			]
		}
		stmts: [
			ast.Stmt(ast.ExprStmt{
				expr: ast.Expr(ast.SelectorExpr{
					lhs: ast.Expr(ast.Ident{
						name: 'a'
					})
					rhs: ast.Ident{
						name: 'len'
					}
				})
			}),
		]
	}
	mut g := Gen.new([])
	assert g.generic_fallback_type_satisfies_body_requirements(node, types.Type(types.string_))
}

fn test_generic_fallback_follows_nested_generic_call_requirements() {
	m_ident := ast.Expr(ast.Ident{
		name: 'M'
	})
	try_node := ast.FnDecl{
		name:  'try_find_iter_at'
		typ:   ast.FnType{
			generic_params: [
				m_ident,
			]
			params:         [
				ast.Parameter{
					name: 'matcher_'
					typ:  m_ident
				},
			]
		}
		stmts: [
			ast.Stmt(ast.ExprStmt{
				expr: ast.Expr(ast.CallExpr{
					lhs: ast.Expr(ast.SelectorExpr{
						lhs: ast.Expr(ast.Ident{
							name: 'matcher_'
						})
						rhs: ast.Ident{
							name: 'find_at'
						}
					})
				})
			}),
		]
	}
	wrapper_node := ast.FnDecl{
		name:  'find_iter_at'
		typ:   ast.FnType{
			generic_params: [
				m_ident,
			]
			params:         [
				ast.Parameter{
					name: 'matcher_'
					typ:  m_ident
				},
			]
		}
		stmts: [
			ast.Stmt(ast.ExprStmt{
				expr: ast.Expr(ast.CallExpr{
					lhs:  ast.Expr(ast.Ident{
						name: 'try_find_iter_at'
					})
					args: [
						ast.Expr(ast.Ident{
							name: 'matcher_'
						}),
					]
				})
			}),
		]
	}
	mut g := Gen.new([
		ast.File{
			mod:   'matcher'
			stmts: [
				ast.Stmt(try_node),
				ast.Stmt(wrapper_node),
			]
		},
	])
	g.cur_module = 'matcher'
	g.build_generic_fn_decl_index()
	g.generic_fn_decl_index['matcher__try_find_iter_at'] = GenericFnDeclInfo{
		file_idx: 0
		stmt_idx: 0
	}
	g.generic_fn_decl_index['try_find_iter_at'] = GenericFnDeclInfo{
		file_idx: 0
		stmt_idx: 0
	}
	assert !g.generic_fallback_type_satisfies_body_requirements(wrapper_node,
		types.Type(types.string_))
}

fn test_generic_fallback_rejects_generic_value_passed_to_interface_param() {
	t_ident := ast.Expr(ast.Ident{
		name: 'T'
	})
	node := ast.FnDecl{
		name:  'capture_loop'
		typ:   ast.FnType{
			generic_params: [
				t_ident,
			]
			params:         [
				ast.Parameter{
					name: 'caps'
					typ:  t_ident
				},
			]
		}
		stmts: [
			ast.Stmt(ast.ExprStmt{
				expr: ast.Expr(ast.CallExpr{
					lhs:  ast.Expr(ast.Ident{
						name: 'capture_match_or_panic'
					})
					args: [
						ast.Expr(ast.Ident{
							name: 'caps'
						}),
						ast.Expr(ast.BasicLiteral{
							kind:  .number
							value: '0'
						}),
					]
				})
			}),
		]
	}
	mut g := Gen.new([])
	g.fn_param_types['capture_match_or_panic'] = ['Captures', 'usize']
	g.interface_methods['Captures'] = [
		InterfaceMethodInfo{
			name: 'len'
		},
		InterfaceMethodInfo{
			name: 'get'
		},
	]
	assert !g.generic_fallback_type_satisfies_body_requirements(node, types.Type(types.f64_))
}

fn test_generic_receiver_method_resolution_prefers_active_binding() {
	mut g := Gen.new([])
	g.active_generic_types['M'] = types.Type(types.Struct{
		name: 'matcher__TestMatcherNoCaps'
	})
	g.cur_fn_generic_params['matcher_'] = 'M'
	g.fn_return_types['matcher__TestMatcher__captures_at'] = '_result_bool'
	g.fn_param_types['matcher__TestMatcher__captures_at'] = ['matcher__TestMatcher', 'Array_u8',
		'usize', 'matcher__TestCaptures*']
	g.fn_return_types['matcher__TestMatcherNoCaps__captures_at'] = '_result_bool'
	g.fn_param_types['matcher__TestMatcherNoCaps__captures_at'] = [
		'matcher__TestMatcherNoCaps',
		'Array_u8',
		'usize',
		'matcher__NoCaptures*',
	]
	resolved := g.resolve_call_name(ast.Expr(ast.SelectorExpr{
		lhs: ast.Expr(ast.Ident{
			name: 'matcher_'
		})
		rhs: ast.Ident{
			name: 'captures_at'
		}
	}), 3)
	assert resolved == 'matcher__TestMatcherNoCaps__captures_at'
	direct_resolved := g.resolve_ident_receiver_method_call_name('matcher__TestMatcher__captures_at', ast.Expr(ast.Ident{
		name: 'matcher__TestMatcher__captures_at'
	}), [
		ast.Expr(ast.Ident{
			name: 'matcher_'
		}),
	])
	assert direct_resolved == 'matcher__TestMatcherNoCaps__captures_at'
}

fn test_fn_pointer_call_uses_existing_mut_generic_pointer_for_reference_arg() {
	csrc := cleanc_csrc_for_test_source('fnptr_mut_generic_pointer_arg', '
struct Caps {}

fn use_caps(caps &Caps) bool {
	_ = caps
	return true
}

fn call_cb[T](mut caps T, cb fn (&T) bool) bool {
	return cb(&caps)
}

fn main() {
	mut caps := Caps{}
	_ = call_cb(mut caps, use_caps)
}
')
	assert csrc.contains('bool call_cb_T_Caps(Caps* caps, bool (*cb)(Caps*))'), csrc
	assert csrc.contains('cb)(caps);'), csrc
	assert !csrc.contains('return cb(&caps);'), csrc
}

fn test_generic_fallback_derives_container_param_element_type() {
	mut env := types.Environment.new()
	env.generic_types['seed'] = [
		{
			'T': types.Type(types.Array{
				elem_type: types.Type(types.string_)
			})
		},
	]
	mut g := Gen.new_with_env([], env)
	array_of_t := ast.Expr(ast.Type(ast.ArrayType{
		elem_type: ast.Expr(ast.Ident{
			name: 'T'
		})
	}))
	node := ast.FnDecl{
		name: 'max'
		typ:  ast.FnType{
			generic_params: [
				ast.Expr(ast.Ident{
					name: 'T'
				}),
			]
			params:         [
				ast.Parameter{
					name: 'array'
					typ:  array_of_t
				},
			]
			return_type:    ast.Expr(ast.Type(ast.ResultType{
				base_type: ast.Expr(ast.Ident{
					name: 'T'
				})
			}))
		}
	}
	specs := g.generic_fn_specializations_with_fallback(node, true)
	assert specs.len == 1
	assert specs[0].name.ends_with('_T_string')
	assert !specs[0].name.contains('Array_string')
}

fn test_generic_call_inference_resolves_address_of_active_generic_param() {
	mut g := Gen.new([])
	g.active_generic_types['T'] = types.Type(types.i64_)
	g.cur_fn_generic_params['val'] = 'T'
	arg := ast.Expr(ast.PrefixExpr{
		op:   .amp
		expr: ast.Expr(ast.Ident{
			name: 'val'
		})
	})
	concrete := g.active_generic_concrete_from_arg(arg) or {
		panic('missing active generic concrete')
	}
	assert concrete is types.Pointer
	assert (concrete as types.Pointer).base_type.name() == 'i64'
	assert g.generic_specialization_arg_type_name(arg) == 'i64*'
	mut bindings := map[string]types.Type{}
	g.infer_generic_type_bindings_from_param(ast.Expr(ast.PrefixExpr{
		op:   .amp
		expr: ast.Expr(ast.Ident{
			name: 'T'
		})
	}), concrete, ['T'], mut bindings)
	binding := bindings['T'] or { panic('missing T binding') }
	assert binding.name() == 'i64'
}

fn test_generic_call_inference_reads_generic_struct_param_bindings() {
	mut env := types.Environment.new()
	mut printer_scope := types.new_scope(unsafe { nil })
	printer_scope.insert('Standard', types.Type(types.Struct{
		name:           'printer__Standard'
		generic_params: ['W']
	}))
	lock env.scopes {
		env.scopes['printer'] = printer_scope
	}
	mut g := Gen.new_with_env([], env)
	g.cur_module = 'core'
	mut bindings := map[string]types.Type{}
	g.infer_generic_type_bindings_from_param(ast.Expr(ast.Type(ast.GenericType{
		name:   ast.Expr(ast.SelectorExpr{
			lhs: ast.Expr(ast.Ident{
				name: 'printer'
			})
			rhs: ast.Ident{
				name: 'Standard'
			}
		})
		params: [
			ast.Expr(ast.Ident{
				name: 'W'
			}),
		]
	})), types.Type(types.Struct{
		name: 'printer__Standard_T_stringptr'
	}), ['W'], mut bindings)
	binding := bindings['W'] or { panic('missing W binding') }
	assert binding is types.Pointer
	assert (binding as types.Pointer).base_type.name() == 'string'
	concrete_struct := g.concrete_type_from_call_arg_c_name('printer__Standard_T_stringptr') or {
		panic('missing concrete specialized struct')
	}
	assert concrete_struct.name() == 'printer__Standard_T_stringptr'
	g.remember_runtime_local_type('standard', 'printer__Standard_T_stringptr')
	standard_param_type := ast.Expr(ast.Type(ast.GenericType{
		name:   ast.Expr(ast.SelectorExpr{
			lhs: ast.Expr(ast.Ident{
				name: 'printer'
			})
			rhs: ast.Ident{
				name: 'Standard'
			}
		})
		params: [
			ast.Expr(ast.Ident{
				name: 'W'
			}),
		]
	}))
	assert g.generic_call_has_concrete_arg_bindings(ast.FnDecl{
		typ: ast.FnType{
			generic_params: [
				ast.Expr(ast.Ident{
					name: 'W'
				}),
			]
			params:         [
				ast.Parameter{
					name: 'standard'
					typ:  standard_param_type
				},
			]
		}
	}, [
		ast.Expr(ast.Ident{
			name: 'standard'
		}),
	], ['W'])
	g.remember_runtime_local_type('standard_ref', 'printer__Standard_T_stringptr*')
	assert g.generic_call_has_concrete_arg_bindings(ast.FnDecl{
		is_method: true
		receiver:  ast.Parameter{
			name: 'standard'
			typ:  standard_param_type
		}
		typ:       ast.FnType{
			generic_params: [
				ast.Expr(ast.Ident{
					name: 'W'
				}),
			]
		}
	}, [
		ast.Expr(ast.Ident{
			name: 'standard_ref'
		}),
	], ['W'])
}

fn test_same_receiver_specialized_method_uses_current_suffix() {
	mut g := Gen.new([])
	g.cur_fn_c_name = 'printer__Standard_T_stringptr__sink_with_path'
	g.specialized_fn_bases['printer__Standard'] = true
	g.fn_return_types['printer__Standard_T_stringptr__sink'] = 'printer__StandardSink_T_stringptr'
	g.fn_return_types['printer__Standard_T_stringptr__needs_match_granularity'] = 'bool'
	assert g.same_receiver_specialized_method_name('printer__Standard_T_string__sink') or {
		panic('missing sink candidate')
	} == 'printer__Standard_T_stringptr__sink'
	assert g.same_receiver_specialized_method_name('printer__Standard_T_string__needs_match_granularity') or {
		panic('missing needs candidate')
	} == 'printer__Standard_T_stringptr__needs_match_granularity'
}

fn test_generic_call_inference_uses_generic_formal_argument_index() {
	t_ident := ast.Expr(ast.Ident{
		name: 'T'
	})
	decoder_ident := ast.Expr(ast.Ident{
		name: 'Decoder'
	})
	mut env := types.Environment.new()
	mut g := Gen.new_with_env([
		ast.File{
			stmts: [
				ast.Stmt(ast.ModuleStmt{
					name: 'json2'
				}),
				ast.Stmt(ast.FnDecl{
					name: 'decode_struct_key'
					typ:  ast.FnType{
						generic_params: [
							t_ident,
						]
						params:         [
							ast.Parameter{
								name:   'decoder'
								is_mut: true
								typ:    decoder_ident
							},
							ast.Parameter{
								name: 'val'
								typ:  t_ident
							},
						]
					}
				}),
			]
		},
	], env)
	g.cur_module = 'json2'
	g.build_generic_fn_decl_index()
	g.active_generic_types['T'] = types.Type(types.Struct{
		name: 'json2__SumtypeTimeValue'
	})
	g.cur_fn_generic_params['val'] = 'T'
	g.fn_return_types['json2__decode_struct_key_T_json2_Decoder'] = '_result_decoder'
	g.fn_return_types['json2__decode_struct_key_T_json2_SumtypeTimeValue'] = '_result_sumtype'

	specialized := g.try_specialize_generic_call_name('json2__decode_struct_key', [
		ast.Expr(ast.Ident{
			name: 'decoder'
		}),
		ast.Expr(ast.Ident{
			name: 'val'
		}),
	]) or { panic('missing specialized call') }
	assert specialized == 'json2__decode_struct_key_T_json2_SumtypeTimeValue'
}

fn test_direct_generic_call_inference_preserves_pointer_argument_type() {
	t_ident := ast.Expr(ast.Ident{
		name: 'T'
	})
	assert generic_specialization_token_for_direct_param(t_ident, 'json2__Node*') == 'json2_Nodeptr'

	mut env := types.Environment.new()
	mut g := Gen.new_with_env([
		ast.File{
			stmts: [
				ast.Stmt(ast.ModuleStmt{
					name: 'json2'
				}),
				ast.Stmt(ast.FnDecl{
					name:      'decode_value'
					is_method: true
					receiver:  ast.Parameter{
						name:   'decoder'
						is_mut: true
						typ:    ast.Expr(ast.Ident{
							name: 'Decoder'
						})
					}
					typ:       ast.FnType{
						generic_params: [
							t_ident,
						]
						params:         [
							ast.Parameter{
								name:   'val'
								is_mut: true
								typ:    t_ident
							},
						]
					}
				}),
			]
		},
	], env)
	g.cur_module = 'json2'
	g.build_generic_fn_decl_index()
	g.generic_fn_decl_index['json2__Decoder__decode_value'] = GenericFnDeclInfo{
		file_idx: 0
		stmt_idx: 1
	}
	g.remember_runtime_local_type('node', 'json2__Node*')
	g.fn_return_types['json2__Decoder__decode_value_T_json2_Node'] = '_result_void'
	g.fn_return_types['json2__Decoder__decode_value_T_json2_Nodeptr'] = '_result_void'

	specialized := g.try_specialize_generic_call_name('json2__Decoder__decode_value', [
		ast.Expr(ast.Ident{
			name: 'decoder'
		}),
		ast.Expr(ast.Ident{
			name: 'node'
		}),
	]) or { panic('missing specialized pointer call') }
	assert specialized == 'json2__Decoder__decode_value_T_json2_Nodeptr'
}

fn test_comptime_struct_match_resolves_named_struct_type() {
	mut env := types.Environment.new()
	mut json2_scope := types.new_scope(unsafe { nil })
	json2_scope.insert('Decoder', types.Type(types.Struct{
		name: 'json2__Decoder'
	}))
	lock env.scopes {
		env.scopes['json2'] = json2_scope
	}
	mut g := Gen.new_with_env([], env)
	g.cur_module = 'json2'
	assert g.comptime_matches_keyword(types.Type(types.NamedType('json2__Decoder')), 'struct')
}

fn test_json2_cached_field_info_specs_follow_value_specializations() {
	mut env := types.Environment.new()
	mut json2_scope := types.new_scope(unsafe { nil })
	json2_scope.insert('Decoder', types.Type(types.Struct{
		name: 'json2__Decoder'
	}))
	json2_scope.insert('Encoder', types.Type(types.Struct{
		name: 'json2__Encoder'
	}))
	lock env.scopes {
		env.scopes['json2'] = json2_scope
	}
	mut g := Gen.new_with_env([], env)
	g.cur_module = 'json2'
	g.fn_return_types['json2__Decoder__decode_value_T_json2_Decoder'] = '_result_void'
	g.fn_return_types['json2__Encoder__encode_struct_fields_T_json2_Encoderptr'] = 'bool'
	g.late_generic_specs['json2__Decoder__decode_value'] = [
		{
			'T': types.Type(types.Struct{
				name: 'json2__LinkedList'
			})
		},
		{
			'T': types.Type(types.Struct{
				name: 'json2__Nodeptr'
			})
		},
	]
	decode_cached := ast.FnDecl{
		name:      'cached_struct_field_infos'
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
	}
	encode_cached := ast.FnDecl{
		name:      'cached_field_infos'
		is_method: true
		receiver:  ast.Parameter{
			name: 'encoder'
			typ:  ast.Expr(ast.Ident{
				name: 'Encoder'
			})
		}
		typ:       ast.FnType{
			generic_params: [
				ast.Expr(ast.Ident{
					name: 'T'
				}),
			]
		}
	}
	decode_specs := g.generic_fn_specializations_with_fallback(decode_cached, true)
	encode_specs := g.generic_fn_specializations_with_fallback(encode_cached, true)
	assert decode_specs.any(it.name == 'json2__Decoder__cached_struct_field_infos_T_json2_Decoder')
	assert decode_specs.any(it.name == 'json2__Decoder__cached_struct_field_infos_T_json2_LinkedList')
	assert decode_specs.any(it.name == 'json2__Decoder__cached_struct_field_infos_T_json2_Nodeptr')
	assert encode_specs.any(it.name == 'json2__Encoder__cached_field_infos_T_json2_Encoderptr')
}

fn test_generic_scan_records_implicit_address_of_method_call() {
	t_ident := ast.Expr(ast.Ident{
		name: 'T'
	})
	decode_number_call := ast.Stmt(ast.ExprStmt{
		expr: ast.Expr(ast.CallExpr{
			lhs:  ast.Expr(ast.SelectorExpr{
				lhs: ast.Expr(ast.Ident{
					name: 'decoder'
				})
				rhs: ast.Ident{
					name: 'decode_number'
				}
			})
			args: [
				ast.Expr(ast.PrefixExpr{
					op:   .amp
					expr: ast.Expr(ast.Ident{
						name: 'val'
					})
				}),
			]
		})
	})
	decode_value := ast.FnDecl{
		name:      'decode_value'
		is_method: true
		receiver:  ast.Parameter{
			name: 'decoder'
			typ:  ast.Expr(ast.Ident{
				name: 'Decoder'
			})
		}
		typ:       ast.FnType{
			generic_params: [t_ident]
			params:         [
				ast.Parameter{
					name:   'val'
					is_mut: true
					typ:    t_ident
				},
			]
		}
		stmts:     [decode_number_call]
	}
	file := ast.File{
		mod:   'json2'
		stmts: [
			ast.Stmt(ast.FnDecl{
				name:      'decode_number'
				is_method: true
				receiver:  decode_value.receiver
				typ:       ast.FnType{
					generic_params: [t_ident]
					params:         [ast.Parameter{
						name: 'val'
						typ:  ast.Expr(ast.PrefixExpr{
							op:   .amp
							expr: t_ident
						})
					}]
				}
			}),
			ast.Stmt(decode_value),
		]
	}
	mut g := Gen.new([file])
	g.cur_module = 'json2'
	g.active_generic_types['T'] = types.Type(types.i64_)
	g.build_generic_fn_decl_index()
	g.seed_fn_scan_runtime_types(decode_value, 'json2__Decoder__decode_value_T_i64')
	g.scan_fn_body_for_generic_types(decode_value, 'i64')
	specs := g.late_generic_specs['decode_number']
	assert specs.len == 1
	spec_t := specs[0]['T'] or { panic('missing T') }
	assert spec_t.name() == 'i64'
	qualified := g.qualified_generic_fn_base_name('Decoder__decode_number') or {
		panic('missing qualified generic name')
	}
	assert qualified == 'Decoder__decode_number'
	g.record_late_generic_call_spec('json2__Decoder__decode_number', {
		'T': types.Type(types.int_)
	})
	assert 'json2__Decoder__decode_number' in g.generic_spec_index['decode_number']
	assert g.generic_key_matches_decl(ast.FnDecl{
		name: 'decode_number'
	}, 'json2__Decoder__decode_number')
}

fn test_generic_scan_prefers_active_binding_over_stale_embedded_call_suffix() {
	t_ident := ast.Expr(ast.Ident{
		name: 'T'
	})
	encode_value := ast.FnDecl{
		name:      'encode_value'
		is_method: true
		receiver:  ast.Parameter{
			name: 'encoder'
			typ:  ast.Expr(ast.Ident{
				name: 'Encoder'
			})
		}
		typ:       ast.FnType{
			generic_params: [t_ident]
			params:         [
				ast.Parameter{
					name: 'val'
					typ:  t_ident
				},
			]
		}
	}
	file := ast.File{
		mod:   'json2'
		stmts: [
			ast.Stmt(encode_value),
		]
	}
	mut g := Gen.new([file])
	g.cur_module = 'json2'
	g.build_generic_fn_decl_index()
	g.collect_generic_scan_calls = true
	g.active_generic_types['T'] = types.Type(types.string_)
	g.cur_fn_generic_params['val'] = 'T'
	g.remember_runtime_local_type('encoder', 'json2__Encoder')
	g.remember_runtime_local_type('val', 'string')
	g.scan_call_for_generic_fn_specs(ast.CallExpr{
		lhs:  ast.Expr(ast.SelectorExpr{
			lhs: ast.Expr(ast.Ident{
				name: 'encoder'
			})
			rhs: ast.Ident{
				name: 'json2__Encoder__encode_value_T_oauth_Request'
			}
		})
		args: [
			ast.Expr(ast.Ident{
				name: 'encoder'
			}),
			ast.Expr(ast.Ident{
				name: 'val'
			}),
		]
	})
	assert 'json2__Encoder__encode_value_T_string' in g.generic_scan_called_names
	assert 'json2__Encoder__encode_value_T_oauth_Request' !in g.generic_scan_called_names
}

fn test_generic_scan_prefers_embedded_suffix_over_unbound_generic_param_fallback_type() {
	a_ident := ast.Expr(ast.Ident{
		name: 'A'
	})
	x_ident := ast.Expr(ast.Ident{
		name: 'X'
	})
	run_new := ast.FnDecl{
		name: 'run_new'
		typ:  ast.FnType{
			generic_params: [a_ident, x_ident]
			params:         [
				ast.Parameter{
					name: 'global_app'
					typ:  a_ident
				},
				ast.Parameter{
					name: 'params'
					typ:  ast.Expr(ast.Ident{
						name: 'RunParams'
					})
				},
			]
		}
	}
	file := ast.File{
		mod:   'veb'
		stmts: [
			ast.Stmt(run_new),
		]
	}
	mut g := Gen.new([file])
	g.cur_module = 'veb'
	g.build_generic_fn_decl_index()
	g.collect_generic_scan_calls = true
	g.remember_runtime_local_type('global_app', 'f64*')
	g.remember_runtime_local_type('params', 'veb__RunParams')
	g.scan_call_for_generic_fn_specs(ast.CallExpr{
		lhs:  ast.Expr(ast.Ident{
			name: 'run_new_T_string_bool'
		})
		args: [
			ast.Expr(ast.Ident{
				name: 'global_app'
			}),
			ast.Expr(ast.Ident{
				name: 'params'
			}),
		]
	})
	assert 'veb__run_new_T_string_bool' in g.generic_scan_called_names
	assert 'veb__run_new_T_f64_bool' !in g.generic_scan_called_names
}

fn test_generic_scan_prefers_matching_active_embedded_suffix_over_local_argument_type() {
	a_ident := ast.Expr(ast.Ident{
		name: 'A'
	})
	x_ident := ast.Expr(ast.Ident{
		name: 'X'
	})
	handle_route := ast.FnDecl{
		name: 'handle_route'
		typ:  ast.FnType{
			generic_params: [a_ident, x_ident]
			params:         [
				ast.Parameter{
					name:   'app'
					is_mut: true
					typ:    a_ident
				},
				ast.Parameter{
					name:   'user_context'
					is_mut: true
					typ:    x_ident
				},
			]
		}
	}
	file := ast.File{
		stmts: [
			ast.Stmt(ast.ModuleStmt{
				name: 'veb'
			}),
			ast.Stmt(handle_route),
		]
	}
	mut g := Gen.new([file])
	g.cur_module = 'veb'
	g.build_generic_fn_decl_index()
	g.collect_generic_scan_calls = true
	g.active_generic_types['A'] = types.Type(types.Struct{
		name: 'App'
	})
	g.active_generic_types['X'] = types.Type(types.Struct{
		name: 'Context'
	})
	g.remember_runtime_local_type('app', 'App*')
	g.remember_runtime_local_type('user_context', 'veb__Context*')
	g.scan_call_for_generic_fn_specs(ast.CallExpr{
		lhs:  ast.Expr(ast.Ident{
			name: 'handle_route_T_A_X'
		})
		args: [
			ast.Expr(ast.ModifierExpr{
				kind: .key_mut
				expr: ast.Expr(ast.Ident{
					name: 'app'
				})
			}),
			ast.Expr(ast.ModifierExpr{
				kind: .key_mut
				expr: ast.Expr(ast.Ident{
					name: 'user_context'
				})
			}),
		]
	})
	assert 'veb__handle_route_T_App_Context' in g.generic_scan_called_names
	assert 'veb__handle_route_T_App_veb_Context' !in g.generic_scan_called_names
}

fn test_generic_emit_prefers_matching_active_embedded_suffix_over_local_argument_type() {
	a_ident := ast.Expr(ast.Ident{
		name: 'A'
	})
	x_ident := ast.Expr(ast.Ident{
		name: 'X'
	})
	handle_route := ast.FnDecl{
		name: 'handle_route'
		typ:  ast.FnType{
			generic_params: [a_ident, x_ident]
			params:         [
				ast.Parameter{
					name:   'app'
					is_mut: true
					typ:    a_ident
				},
				ast.Parameter{
					name:   'user_context'
					is_mut: true
					typ:    x_ident
				},
			]
		}
	}
	file := ast.File{
		stmts: [
			ast.Stmt(ast.ModuleStmt{
				name: 'veb'
			}),
			ast.Stmt(handle_route),
		]
	}
	mut env := types.Environment.new()
	mut g := Gen.new_with_env([file], env)
	g.cur_module = 'veb'
	g.build_generic_fn_decl_index()
	g.active_generic_types['A'] = types.Type(types.Struct{
		name: 'App'
	})
	g.active_generic_types['X'] = types.Type(types.Struct{
		name: 'Context'
	})
	g.remember_runtime_local_type('app', 'App*')
	g.remember_runtime_local_type('user_context', 'veb__Context*')
	g.fn_return_types['veb__handle_route_T_App_Context'] = 'void'
	g.fn_return_types['veb__handle_route_T_App_veb_Context'] = 'void'
	active_bindings := g.active_generic_bindings_matching_name_suffix('veb__handle_route_T_App_Context', [
		'A',
		'X',
	]) or { panic('missing active suffix bindings') }
	assert active_bindings['X'] or { panic('missing X binding') } == types.Type(types.Struct{
		name: 'Context'
	})
	specialized := g.try_specialize_generic_call_name('veb__handle_route_T_App_Context', [
		ast.Expr(ast.ModifierExpr{
			kind: .key_mut
			expr: ast.Expr(ast.Ident{
				name: 'app'
			})
		}),
		ast.Expr(ast.ModifierExpr{
			kind: .key_mut
			expr: ast.Expr(ast.Ident{
				name: 'user_context'
			})
		}),
	]) or { panic('missing specialized call') }
	assert specialized == 'veb__handle_route_T_App_Context'
}

fn test_generic_scan_records_embedded_generic_function_value_ident() {
	a_ident := ast.Expr(ast.Ident{
		name: 'A'
	})
	x_ident := ast.Expr(ast.Ident{
		name: 'X'
	})
	handler := ast.FnDecl{
		name: 'parallel_request_handler'
		typ:  ast.FnType{
			generic_params: [a_ident, x_ident]
		}
	}
	file := ast.File{
		mod:   'veb'
		stmts: [
			ast.Stmt(handler),
		]
	}
	mut g := Gen.new([file])
	g.cur_module = 'veb'
	g.build_generic_fn_decl_index()
	g.collect_generic_scan_calls = true
	g.scan_expr_for_generic_types(ast.Expr(ast.Ident{
		name: 'parallel_request_handler_T_string_bool'
	}))
	assert 'veb__parallel_request_handler_T_string_bool' in g.generic_scan_called_names
}

fn test_generic_scan_prunes_inactive_comptime_type_branch() {
	t_ident := ast.Expr(ast.Ident{
		name: 'T'
	})
	number_call := ast.Stmt(ast.ExprStmt{
		expr: ast.Expr(ast.CallExpr{
			lhs: ast.Expr(ast.GenericArgOrIndexExpr{
				lhs:  ast.Expr(ast.SelectorExpr{
					lhs: ast.Expr(ast.Ident{
						name: 'decoder'
					})
					rhs: ast.Ident{
						name: 'decode_number_from_string'
					}
				})
				expr: t_ident
			})
		})
	})
	string_call := ast.Stmt(ast.ExprStmt{
		expr: ast.Expr(ast.CallExpr{
			lhs: ast.Expr(ast.GenericArgOrIndexExpr{
				lhs:  ast.Expr(ast.SelectorExpr{
					lhs: ast.Expr(ast.Ident{
						name: 'decoder'
					})
					rhs: ast.Ident{
						name: 'decode_string'
					}
				})
				expr: t_ident
			})
		})
	})
	decode_value := ast.FnDecl{
		name:      'decode_value'
		is_method: true
		receiver:  ast.Parameter{
			name: 'decoder'
			typ:  ast.Expr(ast.Ident{
				name: 'Decoder'
			})
		}
		typ:       ast.FnType{
			generic_params: [t_ident]
			params:         [
				ast.Parameter{
					name: 'val'
					typ:  t_ident
				},
			]
		}
		stmts:     [
			ast.Stmt(ast.ComptimeStmt{
				stmt: ast.Stmt(ast.ExprStmt{
					expr: ast.Expr(ast.ComptimeExpr{
						expr: ast.Expr(ast.IfExpr{
							cond:      ast.Expr(ast.InfixExpr{
								op:  .key_is
								lhs: ast.Expr(ast.SelectorExpr{
									lhs: t_ident
									rhs: ast.Ident{
										name: 'unaliased_typ'
									}
								})
								rhs: ast.Expr(ast.ComptimeExpr{
									expr: ast.Expr(ast.Ident{
										name: 'int'
									})
								})
							})
							stmts:     [number_call]
							else_expr: ast.Expr(ast.IfExpr{
								cond:  ast.empty_expr
								stmts: [string_call]
							})
						})
					})
				})
			}),
		]
	}
	file := ast.File{
		mod:   'json2'
		stmts: [
			ast.Stmt(ast.FnDecl{
				name:      'decode_number_from_string'
				is_method: true
				receiver:  decode_value.receiver
				typ:       decode_value.typ
			}),
			ast.Stmt(ast.FnDecl{
				name:      'decode_string'
				is_method: true
				receiver:  decode_value.receiver
				typ:       decode_value.typ
			}),
			ast.Stmt(decode_value),
		]
	}
	mut g := Gen.new([file])
	g.cur_module = 'json2'
	g.active_generic_types['T'] = types.Type(types.string_)
	g.build_generic_fn_decl_index()
	g.scan_fn_body_for_generic_types(decode_value, 'string')
	assert g.late_generic_specs['decode_number_from_string'].len == 0
	string_specs := g.late_generic_specs['decode_string']
	assert string_specs.len == 1
	spec_t := string_specs[0]['T'] or { panic('missing T') }
	assert spec_t.name() == 'string'
}

fn test_nested_generic_scan_uses_fallback_specialization_bindings() {
	mut env := types.Environment.new()
	env.generic_types['seed'] = [
		{
			'T': types.Type(types.i64_)
		},
	]
	mut fn_scope := types.new_scope(unsafe { nil })
	fn_scope.insert('j', types.Type(types.NamedType('T')))
	env.set_fn_scope('veb', 'Context__json', fn_scope)
	t_ident := ast.Expr(ast.Ident{
		name: 'T'
	})
	file := ast.File{
		mod:   'veb'
		stmts: [
			ast.Stmt(ast.FnDecl{
				name: 'encode'
				typ:  ast.FnType{
					generic_params: [t_ident]
					params:         [ast.Parameter{
						name: 'val'
						typ:  t_ident
					}]
				}
			}),
			ast.Stmt(ast.FnDecl{
				name:      'json'
				is_method: true
				receiver:  ast.Parameter{
					name: 'ctx'
					typ:  ast.Expr(ast.Ident{
						name: 'Context'
					})
				}
				typ:       ast.FnType{
					generic_params: [t_ident]
					params:         [ast.Parameter{
						name: 'j'
						typ:  t_ident
					}]
				}
				stmts:     [
					ast.Stmt(ast.ExprStmt{
						expr: ast.Expr(ast.CallExpr{
							lhs:  ast.Expr(ast.Ident{
								name: 'encode'
							})
							args: [
								ast.Expr(ast.Ident{
									name: 'j'
								}),
							]
						})
					}),
				]
			}),
		]
	}
	mut g := Gen.new_with_env([file], env)
	g.build_generic_fn_decl_index()
	g.discover_nested_generic_specs()
	specs := g.late_generic_specs['encode']
	assert specs.len == 1
	spec_t := specs[0]['T'] or { panic('missing T') }
	assert spec_t.name() == 'i64'
}

fn test_option_result_payload_ready_accepts_external_c_typedefs() {
	mut g := Gen.new([])
	g.typedef_c_types['pthread_rwlock_t'] = true
	g.option_aliases['_option_pthread_rwlock_t'] = true
	g.result_aliases['_result_pthread_rwlock_t'] = true
	g.emitted_interface_bodies['IError'] = true
	g.emit_option_result_structs()
	csrc := g.sb.str()
	assert csrc.contains('struct _option_pthread_rwlock_t')
	assert csrc.contains('struct _result_pthread_rwlock_t')
	assert csrc.contains('sizeof(pthread_rwlock_t)')
}

fn test_emit_option_result_structs_accepts_nested_option_payloads() {
	mut g := Gen.new([])
	g.emitted_interface_bodies['IError'] = true
	g.register_alias_type('_result__option_u64')
	g.emit_option_result_structs()
	csrc := g.sb.str()
	assert csrc.contains('struct _option_u64')
	assert csrc.contains('struct _result__option_u64')
	assert csrc.contains('sizeof(_option_u64)')
}

fn test_json2_encode_fallbacks_include_voidptr_and_native_int_helpers() {
	mut env := types.Environment.new()
	usize_type := resolve_primitive_type_name('usize') or { panic('missing usize primitive') }
	env.generic_types['encode'] = [
		{
			'T': types.Type(types.Pointer{
				base_type: types.Type(types.void_)
			})
		},
		{
			'T': usize_type
		},
	]
	mut g := Gen.new_with_env([], env)
	g.cur_module = 'json2'
	g.build_generic_spec_index()
	encode_value_node := ast.FnDecl{
		name:      'encode_value'
		is_method: true
		receiver:  ast.Parameter{
			name: 'encoder'
			typ:  ast.Expr(ast.Ident{
				name: 'Encoder'
			})
		}
		typ:       ast.FnType{
			generic_params: [
				ast.Expr(ast.Ident{
					name: 'T'
				}),
			]
			params:         [
				ast.Parameter{
					name: 'val'
					typ:  ast.Expr(ast.Ident{
						name: 'T'
					})
				},
			]
		}
	}
	encode_value_specs := g.generic_fn_specializations(encode_value_node)
	mut encode_value_names := []string{}
	for spec in encode_value_specs {
		encode_value_names << spec.name
	}
	assert 'Encoder__encode_value_T_voidptr' in encode_value_names
	assert 'Encoder__encode_value_T_usize' in encode_value_names
	encode_number_node := ast.FnDecl{
		name:      'encode_number'
		is_method: true
		receiver:  encode_value_node.receiver
		typ:       encode_value_node.typ
	}
	encode_number_specs := g.generic_fn_specializations(encode_number_node)
	mut encode_number_names := []string{}
	for spec in encode_number_specs {
		encode_number_names << spec.name
	}
	assert 'Encoder__encode_number_T_usize' in encode_number_names
	assert 'Encoder__encode_number_T_isize' in encode_number_names
}

fn test_generic_specs_filter_unconditional_type_fields_loop() {
	mut env := types.Environment.new()
	env.generic_types['encode_struct_fields'] = [
		{
			'T': types.Type(types.Pointer{
				base_type: types.Type(types.void_)
			})
		},
		{
			'T': types.Type(types.Struct{
				name: 'Config'
			})
		},
	]
	mut g := Gen.new_with_env([], env)
	g.build_generic_spec_index()
	node := ast.FnDecl{
		name:      'encode_struct_fields'
		is_method: true
		receiver:  ast.Parameter{
			name: 'encoder'
			typ:  ast.Expr(ast.Ident{
				name: 'Encoder'
			})
		}
		typ:       ast.FnType{
			generic_params: [
				ast.Expr(ast.Ident{
					name: 'T'
				}),
			]
			params:         [
				ast.Parameter{
					name: 'val'
					typ:  ast.Expr(ast.Ident{
						name: 'T'
					})
				},
			]
		}
		stmts:     [
			ast.Stmt(ast.ComptimeStmt{
				stmt: ast.Stmt(ast.ForStmt{
					init: ast.Stmt(ast.ForInStmt{
						value: ast.Expr(ast.Ident{
							name: 'field'
						})
						expr:  ast.Expr(ast.SelectorExpr{
							lhs: ast.Expr(ast.Ident{
								name: 'T'
							})
							rhs: ast.Ident{
								name: 'fields'
							}
						})
					})
				})
			}),
		]
	}
	specs := g.generic_fn_specializations_with_fallback(node, false)
	assert specs.len == 1
	assert specs[0].name.ends_with('_T_Config')
}

fn test_generic_specs_skip_comptime_metadata_types() {
	mut env := types.Environment.new()
	env.generic_types['meta_sink'] = [
		{
			'T': types.Type(types.NamedType('Type'))
		},
		{
			'T': types.Type(types.Struct{
				name: 'Type'
			})
		},
		{
			'T': types.Type(types.Struct{
				name: '__type_info'
			})
		},
		{
			'T': types.Type(types.Array{
				elem_type: types.Type(types.Struct{
					name: '__field_info'
				})
			})
		},
		{
			'T': types.Type(types.int_)
		},
	]
	mut g := Gen.new_with_env([], env)
	g.build_generic_spec_index()
	node := ast.FnDecl{
		name: 'meta_sink'
		typ:  ast.FnType{
			generic_params: [
				ast.Expr(ast.Ident{
					name: 'T'
				}),
			]
		}
	}
	specs := g.generic_fn_specializations_with_fallback(node, false)
	assert specs.len == 1
	assert specs[0].name.ends_with('_T_int')
}

fn test_generic_placeholder_name_selector_emits_static_string() {
	mut g := Gen.new([])
	g.expr(ast.Expr(ast.SelectorExpr{
		lhs: ast.Expr(ast.Ident{
			name: 'T'
		})
		rhs: ast.Ident{
			name: 'name'
		}
	}))
	csrc := g.sb.str()

	assert csrc.contains('"T"')
	assert !csrc.contains('T.name')
}

fn test_scan_comptime_for_records_field_type_generic_specs() {
	mut g := Gen.new([
		ast.File{
			mod:   'main'
			stmts: [
				ast.Stmt(ast.FnDecl{
					name: 'struct_field_should_encode'
					typ:  ast.FnType{
						generic_params: [
							ast.Expr(ast.Ident{
								name: 'T'
							}),
						]
						params:         [
							ast.Parameter{
								name: 'val'
								typ:  ast.Expr(ast.Ident{
									name: 'T'
								})
							},
						]
					}
				}),
				ast.Stmt(ast.FnDecl{
					name: 'meta_type_sink'
					typ:  ast.FnType{
						generic_params: [
							ast.Expr(ast.Ident{
								name: 'T'
							}),
						]
						params:         [
							ast.Parameter{
								name: 'val'
								typ:  ast.Expr(ast.Ident{
									name: 'T'
								})
							},
						]
					}
				}),
			]
		},
	])
	g.active_generic_types['T'] = types.Type(types.Struct{
		name:   'Repo'
		fields: [
			types.Field{
				name: 'description'
				typ:  types.Type(types.string_)
			},
		]
	})
	g.scan_comptime_for_for_generic_types(ast.ForStmt{
		init:  ast.Stmt(ast.ForInStmt{
			value: ast.Expr(ast.Ident{
				name: 'field'
			})
			expr:  ast.Expr(ast.SelectorExpr{
				lhs: ast.Expr(ast.Ident{
					name: 'T'
				})
				rhs: ast.Ident{
					name: 'fields'
				}
			})
		})
		stmts: [
			ast.Stmt(ast.ExprStmt{
				expr: ast.Expr(ast.CallExpr{
					lhs:  ast.Expr(ast.Ident{
						name: 'struct_field_should_encode'
					})
					args: [
						ast.Expr(ast.SelectorExpr{
							lhs: ast.Expr(ast.Ident{
								name: 'val'
							})
							rhs: ast.Ident{
								name: '__comptime_selector__'
							}
						}),
					]
				})
			}),
			ast.Stmt(ast.ExprStmt{
				expr: ast.Expr(ast.CallExpr{
					lhs:  ast.Expr(ast.Ident{
						name: 'meta_type_sink'
					})
					args: [
						ast.Expr(ast.SelectorExpr{
							lhs: ast.Expr(ast.Ident{
								name: 'field'
							})
							rhs: ast.Ident{
								name: 'typ'
							}
						}),
					]
				})
			}),
		]
	})
	specs := g.late_generic_specs['struct_field_should_encode']
	assert specs.len == 1
	spec_t := specs[0]['T'] or { panic('missing T') }
	assert spec_t.name() == 'string'
	assert g.late_generic_specs['meta_type_sink'].len == 0
}

fn test_comptime_for_uses_full_struct_metadata_from_env() {
	mut env := types.Environment.new()
	mut config_scope := types.new_scope(unsafe { nil })
	config_scope.insert('Config', types.Type(types.Struct{
		name:   'config__Config'
		fields: [
			types.Field{
				name: 'port'
				typ:  types.Type(types.int_)
			},
		]
	}))
	lock env.scopes {
		env.scopes['config'] = config_scope
	}
	mut g := Gen.new_with_env([], env)
	g.cur_module = 'json2'
	fallback := types.Struct{
		name:   'config__Config'
		fields: [
			types.Field{
				name: 'port'
				typ:  types.Type(types.Pointer{
					base_type: types.Type(types.Struct{
						name: 'config__Config'
					})
				})
			},
		]
	}
	resolved := g.comptime_for_struct_type(types.Type(types.Struct{
		name: 'config__Config'
	}), fallback)
	assert resolved.fields.len == 1
	assert resolved.fields[0].name == 'port'
	assert resolved.fields[0].typ.name() == 'int'
	g.comptime_field_var = 'field'
	g.comptime_field_type = 'int'
	assert g.decl_selector_rhs_type(ast.SelectorExpr{
		lhs: ast.Expr(ast.Ident{
			name: 'val'
		})
		rhs: ast.Ident{
			name: '__comptime_selector__'
		}
	}) == 'int'
}

fn test_generic_index_equality_uses_active_concrete_type() {
	mut env := types.Environment.new()
	mut fn_scope := types.new_scope(unsafe { nil })
	fn_scope.insert('a', types.Type(types.Array{
		elem_type: types.Type(types.NamedType('T'))
	}))
	fn_scope.insert('e', types.Type(types.NamedType('T')))
	env.set_expr_type(1, types.string_)
	env.set_expr_type(2, types.int_)
	env.set_expr_type(3, types.string_)
	env.set_expr_type(4, types.string_)

	mut g := Gen.new_with_env([], env)
	g.cur_fn_scope = fn_scope
	g.active_generic_types['T'] = types.Type(types.Struct{
		name: 'Item'
	})
	node := ast.InfixExpr{
		op:  .eq
		lhs: ast.Expr(ast.IndexExpr{
			lhs:  ast.Expr(ast.Ident{
				pos:  token.Pos{
					id: 1
				}
				name: 'a'
			})
			expr: ast.Expr(ast.Ident{
				pos:  token.Pos{
					id: 2
				}
				name: 'idx'
			})
			pos:  token.Pos{
				id: 3
			}
		})
		rhs: ast.Expr(ast.Ident{
			pos:  token.Pos{
				id: 4
			}
			name: 'e'
		})
	}
	g.gen_infix_expr(&node)
	out := g.sb.str()
	assert out.contains('memcmp')
	assert out.contains('Item')
	assert !out.contains('string__eq')

	mut g2 := Gen.new_with_env([], env)
	g2.runtime_local_types['a'] = 'Array_Item'
	g2.runtime_local_types['e'] = 'Item'
	g2.gen_infix_expr(&node)
	out2 := g2.sb.str()
	assert out2.contains('memcmp')
	assert out2.contains('Item')
	assert !out2.contains('string__eq')
}

fn test_map_equality_uses_map_eq_function() {
	mut g := Gen.new([])
	g.runtime_local_types['a'] = 'Map_string_string'
	g.runtime_local_types['b'] = 'Map_string_string'
	node := ast.InfixExpr{
		op:  .eq
		lhs: ast.Expr(ast.Ident{
			name: 'a'
		})
		rhs: ast.Expr(ast.Ident{
			name: 'b'
		})
	}
	g.gen_infix_expr(&node)
	assert g.sb.str() == 'Map_string_string_map_eq(a, b)'
}

fn test_map_equality_helper_call_is_not_module_qualified() {
	mut g := Gen.new([])
	g.cur_module = 'builder'
	g.call_expr(ast.Expr(ast.Ident{
		name: 'Map_string_string_map_eq'
	}), [
		ast.Expr(ast.Ident{
			name: 'new_hashes'
		}),
		ast.Expr(ast.Ident{
			name: 'old_hashes'
		}),
	])
	assert g.sb.str() == 'Map_string_string_map_eq(new_hashes, old_hashes)'
}

fn test_omitted_end_array_slice_uses_non_indexing_slice_helper() {
	mut g := Gen.new([])
	g.remember_runtime_local_type('args', 'Array_string')
	g.expr(ast.Expr(ast.IndexExpr{
		lhs:  ast.Expr(ast.Ident{
			name: 'args'
		})
		expr: ast.Expr(ast.RangeExpr{
			start: ast.Expr(ast.BasicLiteral{
				value: '1'
				kind:  .number
			})
			end:   ast.empty_expr
		})
	}))
	assert g.sb.str() == 'array__slice_ni(args, 1, args.len)'
}

fn test_mut_receiver_operator_assignment_dereferences_receiver() {
	mut g := Gen.new([])
	g.runtime_local_types['result'] = 'Big*'
	g.runtime_local_types['ten'] = 'Big'
	g.cur_fn_mut_params['result'] = true
	g.fn_return_types['Big__mul'] = 'Big'
	node := ast.AssignStmt{
		op:  .assign
		lhs: [ast.Expr(ast.Ident{
			name: 'result'
		})]
		rhs: [
			ast.Expr(ast.InfixExpr{
				op:  .mul
				lhs: ast.Expr(ast.Ident{
					name: 'result'
				})
				rhs: ast.Expr(ast.Ident{
					name: 'ten'
				})
			}),
		]
	}
	g.gen_assign_stmt(node)
	out := g.sb.str().trim_space()
	assert out == '*result = Big__mul((*result), ten);'
}

fn test_decl_from_mut_receiver_operator_uses_value_type() {
	mut g := Gen.new([])
	g.runtime_local_types['result'] = 'Big*'
	g.runtime_local_types['ten'] = 'Big'
	g.cur_fn_mut_params['result'] = true
	g.fn_return_types['Big__mul'] = 'Big'
	node := ast.AssignStmt{
		op:  .decl_assign
		lhs: [ast.Expr(ast.Ident{
			name: 'next'
		})]
		rhs: [
			ast.Expr(ast.InfixExpr{
				op:  .mul
				lhs: ast.Expr(ast.Ident{
					name: 'result'
				})
				rhs: ast.Expr(ast.Ident{
					name: 'ten'
				})
			}),
		]
	}
	g.gen_assign_stmt(node)
	out := g.sb.str().trim_space()
	assert out == 'Big next = Big__mul((*result), ten);'
	assert !out.contains('Big* next')
}

fn test_nested_mut_receiver_operator_does_not_deref_value_result_with_pointer_env() {
	mut env := types.Environment.new()
	env.set_expr_type(25, types.Type(types.Pointer{
		base_type: types.Struct{
			name: 'Big'
		}
	}))
	mut g := Gen.new_with_env([], env)
	g.runtime_local_types['result'] = 'Big*'
	g.runtime_local_types['ten'] = 'Big'
	g.runtime_local_types['one'] = 'Big'
	g.cur_fn_mut_params['result'] = true
	g.fn_return_types['Big__mul'] = 'Big'
	g.fn_return_types['Big__plus'] = 'Big'
	node := ast.InfixExpr{
		op:  .plus
		lhs: ast.Expr(ast.CallExpr{
			lhs:  ast.Expr(ast.Ident{
				name: 'Big__mul'
			})
			args: [ast.Expr(ast.Ident{
				name: 'result'
			}),
				ast.Expr(ast.Ident{
					name: 'ten'
				})]
			pos:  token.Pos{
				id: 25
			}
		})
		rhs: ast.Expr(ast.Ident{
			name: 'one'
		})
	}
	g.gen_infix_expr(&node)
	out := g.sb.str()
	assert out == 'Big__plus(Big__mul(result, ten), one)'
	assert !out.contains('*(Big__mul')
}

fn test_preamble_overrides_cpu_relax_for_tinyc_arm() {
	mut g := Gen.new([])
	g.write_preamble()
	out := g.sb.str()
	assert out.contains('#undef cpu_relax')
	assert out.contains('#define cpu_relax() ((void)0)')
}

fn test_preamble_defines_v_commit_hash_fallback() {
	mut g := Gen.new([])
	g.write_preamble()
	out := g.sb.str()
	assert out.contains('#ifndef V_COMMIT_HASH')
	assert out.contains('#define V_COMMIT_HASH "@@@"')
}

fn test_struct_equality_resolves_alias_field_base_type() {
	mut g := Gen.new([])
	db_type := types.Struct{
		name:   'pg__DB'
		fields: [
			types.Field{
				name: 'conninfo'
				typ:  types.string_
			},
		]
	}
	app_type := types.Struct{
		name:   'App'
		fields: [
			types.Field{
				name: 'db'
				typ:  types.Alias{
					name:      'GitlyDb'
					base_type: types.Type(db_type)
				}
			},
		]
	}

	assert g.struct_has_ref_fields(app_type)
	out := g.gen_struct_field_eq_expr(app_type, 'left', 'right')
	assert out == 'string__eq(left.db.conninfo, right.db.conninfo)'
	assert !out.contains('left.db == right.db')
}

fn test_fixed_array_alias_index_uses_element_type_before_env() {
	mut env := types.Environment.new()
	mut fn_scope := types.new_scope(unsafe { nil })
	fn_scope.insert('b', types.Type(types.Alias{
		name:      'fixed_u8_2'
		base_type: types.Type(types.ArrayFixed{
			len:       2
			elem_type: types.Type(types.Primitive{
				props: .integer | .unsigned
				size:  8
			})
		})
	}))
	env.set_expr_type(5, types.Type(types.Alias{
		name: 'fixed_u8_2'
	}))

	mut g := Gen.new_with_env([], env)
	g.cur_fn_scope = fn_scope
	elem_type := g.get_expr_type(ast.Expr(ast.IndexExpr{
		lhs:  ast.Expr(ast.Ident{
			name: 'b'
		})
		expr: ast.Expr(ast.BasicLiteral{
			value: '1'
			kind:  .number
		})
		pos:  token.Pos{
			id: 5
		}
	}))
	assert elem_type == 'u8'
}

fn test_fixed_array_local_index_uses_element_type_not_fixed_alias_name() {
	assert array_alias_elem_type('Array_fixed_int_5') == 'int'

	mut env := types.Environment.new()
	mut fn_scope := types.new_scope(unsafe { nil })
	fn_scope.insert('fixed_int_5', types.Type(types.Alias{
		name:      'fixed_int_5'
		base_type: types.Type(types.int_)
	}))
	fn_scope.insert('fixed_literal', types.Type(types.ArrayFixed{
		len:       5
		elem_type: types.Type(types.int_)
	}))

	mut g := Gen.new_with_env([], env)
	g.cur_fn_scope = fn_scope
	g.remember_runtime_local_type('fixed_literal', 'Array_fixed_int_5')
	elem_type := g.get_expr_type(ast.Expr(ast.IndexExpr{
		lhs:  ast.Expr(ast.Ident{
			name: 'fixed_literal'
		})
		expr: ast.Expr(ast.BasicLiteral{
			value: '1'
			kind:  .number
		})
	}))
	assert elem_type == 'int'
}

fn test_string_selector_index_method_uses_u8_before_env_int() {
	mut env := types.Environment.new()
	env.set_expr_type(41, types.int_)
	mut g := Gen.new_with_env([], env)
	g.struct_field_types['Parser.scanner'] = 'Scanner'
	g.struct_field_types['Scanner.src'] = 'string'
	g.remember_runtime_local_type('p', 'Parser')
	g.remember_runtime_local_type('idx', 'int')
	g.fn_return_types['u8__is_space'] = 'bool'
	g.fn_param_types['u8__is_space'] = ['u8']
	g.fn_param_is_ptr['u8__is_space'] = [false]
	g.fn_return_types['int__is_space'] = 'bool'
	g.fn_param_types['int__is_space'] = ['int']
	g.fn_param_is_ptr['int__is_space'] = [false]
	src_expr := ast.SelectorExpr{
		lhs: ast.Expr(ast.SelectorExpr{
			lhs: ast.Expr(ast.Ident{
				name: 'p'
			})
			rhs: ast.Ident{
				name: 'scanner'
			}
		})
		rhs: ast.Ident{
			name: 'src'
		}
	}
	index_expr := ast.IndexExpr{
		lhs:  ast.Expr(src_expr)
		expr: ast.Expr(ast.Ident{
			name: 'idx'
		})
		pos:  token.Pos{
			id: 41
		}
	}
	assert g.get_expr_type(ast.Expr(index_expr)) == 'u8'
	g.call_expr(ast.Expr(ast.SelectorExpr{
		lhs: ast.Expr(index_expr)
		rhs: ast.Ident{
			name: 'is_space'
		}
	}), [])
	out := g.sb.str()
	assert out.starts_with('u8__is_space(')
	assert !out.contains('int__is_space')
}

fn test_string_pointer_index_returns_string_not_u8() {
	mut env := types.Environment.new()
	mut fn_scope := types.new_scope(unsafe { nil })
	fn_scope.insert('input_base', types.Type(types.Pointer{
		base_type: types.string_
	}))
	mut g := Gen.new_with_env([], env)
	g.cur_fn_scope = fn_scope
	g.remember_runtime_local_type('input_base', 'string*')
	g.remember_runtime_local_type('i', 'int')
	elem_type := g.get_expr_type(ast.Expr(ast.IndexExpr{
		lhs:  ast.Expr(ast.Ident{
			name: 'input_base'
		})
		expr: ast.Expr(ast.Ident{
			name: 'i'
		})
	}))
	assert elem_type == 'string'
}

fn test_main_method_local_struct_init_keeps_main_type_name() {
	csrc := cleanc_csrc_for_test_source('main_method_local_struct_init', 'module main

struct App {}

struct Repo {
	name string
}

fn clone_repo(repo Repo) {
	_ = repo
}

fn (mut app App) create() {
	mut new_repo := &Repo{
		name: "x"
	}
	clone_job_repo := *new_repo
	clone_repo(clone_job_repo)
}
')
	assert csrc.contains('Repo* new_repo =')
	assert csrc.contains('Repo clone_job_repo = *new_repo;')
	assert !csrc.contains('App__Repo')
}

fn test_seed_implicit_veb_ctx_keeps_route_param_value_type() {
	mut g := Gen.new([])
	mut fn_scope := types.new_scope(unsafe { nil })
	fn_scope.insert('ctx', types.Type(types.Pointer{
		base_type: types.Struct{
			name: 'Context'
		}
	}))
	g.cur_fn_scope = fn_scope
	g.fn_return_types['App__user_repos'] = 'veb__Result'
	g.fn_param_types['App__user_repos'] = ['App*', 'Context*', 'string']
	g.fn_param_is_ptr['App__user_repos'] = [true, true, false]
	node := ast.FnDecl{
		is_method: true
		receiver:  ast.Parameter{
			name:   'app'
			typ:    ast.Expr(ast.Ident{
				name: 'App'
			})
			is_mut: true
		}
		name:      'user_repos'
		typ:       ast.FnType{
			params:      [
				ast.Parameter{
					name: 'username'
					typ:  ast.Expr(ast.Ident{
						name: 'string'
					})
				},
			]
			return_type: ast.Expr(ast.SelectorExpr{
				lhs: ast.Expr(ast.Ident{
					name: 'veb'
				})
				rhs: ast.Ident{
					name: 'Result'
				}
			})
		}
	}
	g.seed_fn_scan_runtime_types(node, 'App__user_repos')
	username_type := g.get_local_var_c_type('username') or { '' }
	assert username_type == 'string'
}

fn test_primary_generic_struct_binding_is_runtime_specializable() {
	mut env := types.Environment.new()
	mut api_scope := types.new_scope(unsafe { nil })
	api_scope.insert('ApiSuccessResponse', types.Type(types.Struct{
		name:           'api__ApiSuccessResponse'
		generic_params: ['T']
		fields:         [
			types.Field{
				name: 'result'
				typ:  types.Type(types.Struct{
					name: 'T'
				})
			},
		]
	}))
	lock env.scopes {
		env.scopes['api'] = api_scope
	}
	mut g := Gen.new_with_env([], env)
	g.generic_struct_bindings['api__ApiSuccessResponse'] = {
		'T': types.Type(types.string_)
	}
	assert g.generic_concrete_type_is_runtime_specializable(types.Type(types.Struct{
		name: 'api__ApiSuccessResponse'
	}))
}

fn test_called_weak_generic_worklist_only_adds_called_specs() {
	t_ident := ast.Expr(ast.Ident{
		name: 'T'
	})
	file := ast.File{
		mod:   'json2'
		name:  '/tmp/v2_cache/json2.v'
		stmts: [
			ast.Stmt(ast.FnDecl{
				name: 'encode'
				typ:  ast.FnType{
					generic_params: [t_ident]
				}
			}),
		]
	}
	mut g := Gen.new_with_env([file], types.Environment.new())
	g.set_emit_files(['/tmp/v2_app/main.v'])
	g.record_late_generic_call_spec('json2__encode', {
		'T': types.Type(types.string_)
	})
	g.record_late_generic_call_spec('json2__encode', {
		'T': types.Type(types.int_)
	})
	string_name := 'encode_T_string'
	int_name := 'encode_T_int'
	late_names := g.late_weak_generic_name_set()
	assert string_name in late_names, late_names.str()
	assert int_name in late_names, late_names.str()
	g.called_fn_names[string_name] = true
	mut needed_names := map[string]bool{}
	g.add_called_weak_generic_names(mut needed_names)
	assert string_name in needed_names
	assert int_name !in needed_names
}

fn test_gen_stmts_restores_source_module_between_statements() {
	mut env := types.Environment.new()
	mut veb_scope := types.new_scope(unsafe { nil })
	veb_scope.insert('Route', types.Type(types.Struct{
		name: 'veb__Route'
	}))
	mut buffer_scope := types.new_scope(unsafe { nil })
	buffer_scope.insert('Route', types.Type(types.Struct{
		name: 'buffer__Route'
	}))
	lock env.scopes {
		env.scopes['veb'] = veb_scope
		env.scopes['buffer'] = buffer_scope
	}
	mut g := Gen.new_with_env([], env)
	g.cur_file_name = '/tmp/veb.v'
	g.cur_module = 'veb'
	g.gen_stmts([
		ast.Stmt(ast.ModuleStmt{
			name: 'buffer'
		}),
		ast.Stmt(ast.AssignStmt{
			op:  .decl_assign
			lhs: [ast.Expr(ast.Ident{
				name: 'route'
			})]
			rhs: [
				ast.Expr(ast.InitExpr{
					typ:    ast.Ident{
						name: 'Route'
					}
					fields: [
						ast.FieldInit{
							name:  'path'
							value: ast.Expr(ast.StringLiteral{
								value: '/'
							})
						},
					]
				}),
			]
		}),
	])
	out := g.sb.str()
	assert out.contains('veb__Route route = ((veb__Route){'), out
	assert !out.contains('buffer__Route route'), out
}

fn test_gen_specialized_fn_body_uses_module_prefix_context() {
	mut env := types.Environment.new()
	mut veb_scope := types.new_scope(unsafe { nil })
	veb_scope.insert('Route', types.Type(types.Struct{
		name: 'veb__Route'
	}))
	mut buffer_scope := types.new_scope(unsafe { nil })
	buffer_scope.insert('Route', types.Type(types.Struct{
		name: 'buffer__Route'
	}))
	lock env.scopes {
		env.scopes['veb'] = veb_scope
		env.scopes['buffer'] = buffer_scope
	}
	mut g := Gen.new_with_env([], env)
	g.cur_module = 'buffer'
	node := ast.FnDecl{
		name:  'make_route'
		stmts: [
			ast.Stmt(ast.AssignStmt{
				op:  .decl_assign
				lhs: [ast.Expr(ast.Ident{
					name: 'route'
				})]
				rhs: [
					ast.Expr(ast.InitExpr{
						typ:    ast.Ident{
							name: 'Route'
						}
						fields: [
							ast.FieldInit{
								name:  'path'
								value: ast.Expr(ast.StringLiteral{
									value: '/'
								})
							},
						]
					}),
				]
			}),
		]
	}
	g.gen_fn_decl_with_name_ptr(&node, 'veb__make_route_T_App')
	out := g.sb.str()
	assert out.contains('veb__Route route = ((veb__Route){'), out
	assert !out.contains('buffer__Route route'), out
}

fn test_source_module_exists_falls_back_to_env_for_cached_chunks() {
	mut env := types.Environment.new()
	mut veb_scope := types.new_scope(unsafe { nil })
	veb_scope.insert('Route', types.Type(types.Struct{
		name: 'veb__Route'
	}))
	lock env.scopes {
		env.scopes['veb'] = veb_scope
	}
	mut g := Gen.new_with_env([
		ast.File{
			stmts: [
				ast.Stmt(ast.ModuleStmt{
					name: 'buffer'
				}),
			]
		},
	], env)
	g.collect_source_module_names()
	assert 'veb' !in g.source_module_names
	assert g.source_module_exists('veb')
	assert !g.source_module_exists('array')
}

fn test_init_expr_prefers_current_fn_module_when_nested_context_leaks() {
	mut env := types.Environment.new()
	mut veb_scope := types.new_scope(unsafe { nil })
	veb_scope.insert('Route', types.Type(types.Struct{
		name: 'veb__Route'
	}))
	mut buffer_scope := types.new_scope(unsafe { nil })
	buffer_scope.insert('Route', types.Type(types.Struct{
		name: 'buffer__Route'
	}))
	lock env.scopes {
		env.scopes['veb'] = veb_scope
		env.scopes['buffer'] = buffer_scope
	}
	mut g := Gen.new_with_env([], env)
	g.source_module_names['buffer'] = true
	g.cur_module = 'buffer'
	g.cur_fn_c_name = 'veb__generate_routes_T_App_Context'
	g.gen_init_expr(ast.InitExpr{
		typ:    ast.Ident{
			name: 'Route'
		}
		fields: [
			ast.FieldInit{
				name:  'path'
				value: ast.Expr(ast.StringLiteral{
					value: '/'
				})
			},
		]
	})
	out := g.sb.str()
	assert out.contains('((veb__Route){'), out
	assert !out.contains('buffer__Route'), out
}

fn test_decl_assign_prefers_current_fn_module_for_init_expr_type() {
	mut env := types.Environment.new()
	mut veb_scope := types.new_scope(unsafe { nil })
	veb_scope.insert('Route', types.Type(types.Struct{
		name: 'veb__Route'
	}))
	mut buffer_scope := types.new_scope(unsafe { nil })
	buffer_scope.insert('Route', types.Type(types.Struct{
		name: 'buffer__Route'
	}))
	lock env.scopes {
		env.scopes['veb'] = veb_scope
		env.scopes['buffer'] = buffer_scope
	}
	mut g := Gen.new_with_env([], env)
	g.source_module_names['buffer'] = true
	g.cur_module = 'buffer'
	g.cur_fn_c_name = 'veb__generate_routes_T_App_Context'
	g.gen_assign_stmt(ast.AssignStmt{
		op:  .decl_assign
		lhs: [ast.Expr(ast.Ident{
			name: 'route'
		})]
		rhs: [
			ast.Expr(ast.InitExpr{
				typ:    ast.Ident{
					name: 'Route'
				}
				fields: [
					ast.FieldInit{
						name:  'path'
						value: ast.Expr(ast.StringLiteral{
							value: '/'
						})
					},
				]
			}),
		]
	})
	out := g.sb.str()
	assert out.contains('veb__Route route = ((veb__Route){'), out
	assert !out.contains('buffer__Route'), out
}
