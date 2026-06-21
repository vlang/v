module c

import v3.flat
import v3.types

fn (mut g FlatGen) gen_fns() {
	mut cur_module := ''
	for i in 0 .. g.a.nodes.len {
		node := g.a.nodes[i]
		kind_id := node_kind_id(node)
		if kind_id == 77 {
			cur_module = ''
			g.tc.cur_module = cur_module
			continue
		}
		if kind_id == 73 {
			cur_module = node.value
			g.tc.cur_module = cur_module
			continue
		}

		if kind_id == 61 {
			if !g.should_emit_fn_node_in_module(node, i, cur_module) {
				continue
			}
			qfn := qualified_fn_name_in_module(cur_module, node.value)
			if g.emitted_fn_contains(qfn) {
				continue
			}
			g.emitted_fns[qfn] = true
			g.tc.cur_module = cur_module
			g.gen_fn_in_module(node, cur_module)
		}
	}
}

fn (mut g FlatGen) should_emit_fn_node(node flat.Node, node_index int) bool {
	return g.should_emit_fn_node_in_module(node, node_index, g.tc.cur_module)
}

fn (mut g FlatGen) should_emit_fn_node_in_module(node flat.Node, node_index int, module_name string) bool {
	_ = node_index
	dfn := dotted_fn_name_in_module(module_name, node.value)
	cfn := c_name(node.value)
	qfn := qualified_fn_name_in_module(module_name, node.value)
	if cfn in ['Array_string__join', 'array_string_join'] {
		return false
	}
	if module_name == 'main' {
		return !g.has_generic_params(node)
	}
	if g.has_used_fn_filter() && !g.used_fn_contains(node.value) && !g.used_fn_contains(dfn)
		&& !g.used_fn_contains(cfn) && !g.used_fn_contains(qfn) {
		return false
	}
	if g.has_generic_params(node) {
		return false
	}
	return true
}

fn (g &FlatGen) used_fn_contains(name string) bool {
	if name.len == 0 {
		return false
	}
	return g.used_fns[name]
}

fn (g &FlatGen) has_used_fn_filter() bool {
	return g.used_fns.len > 0 && g.used_fn_contains('main')
}

fn (g &FlatGen) emitted_fn_contains(name string) bool {
	return name.len > 0 && g.emitted_fns[name]
}

fn (g &FlatGen) qualified_fn_name(name string) string {
	return qualified_fn_name_in_module(g.tc.cur_module, name)
}

fn qualified_fn_name_in_module(module_name string, name string) string {
	if module_name == 'builtin' && name == 'free' {
		return 'v_free'
	}
	if module_name.len > 0 && module_name != 'main' && module_name != 'builtin' {
		return c_name('${module_name}.${name}')
	}
	if name == 'free' {
		return 'v_free'
	}
	return c_name(name)
}

fn (g &FlatGen) direct_call_name(name string) string {
	if name == 'free' {
		return 'v_free'
	}
	if name == 'int_str' {
		return 'int__str'
	}
	if name == 'bool_str' {
		return 'bool__str'
	}
	return c_name(name)
}

fn (g &FlatGen) dotted_fn_name(name string) string {
	return dotted_fn_name_in_module(g.tc.cur_module, name)
}

fn dotted_fn_name_in_module(module_name string, name string) string {
	if module_name.len > 0 && module_name != 'main' && module_name != 'builtin' {
		return '${module_name}.${name}'
	}
	return name
}

fn qualify_name_in_module(module_name string, name string) string {
	if module_name.len == 0 || module_name == 'main' || module_name == 'builtin' {
		return name
	}
	if name.contains('.') {
		return name
	}
	return '${module_name}.${name}'
}

fn (mut g FlatGen) gen_fn(node flat.Node) {
	g.gen_fn_in_module(node, g.tc.cur_module)
}

fn (mut g FlatGen) gen_fn_in_module(node flat.Node, module_name string) {
	g.tc.cur_module = module_name
	g.cur_fn_name = node.value
	g.tc.push_scope()
	g.defers = []flat.NodeId{}
	g.set_cur_fn_ret(types.Type(types.void_))
	old_param_names := g.cur_param_names.clone()
	old_param_type_values := g.cur_param_type_values.clone()
	old_param_types := g.cur_param_types.clone()
	g.cur_param_names = []string{}
	g.cur_param_type_values = []types.Type{}
	g.cur_param_types = map[string]types.Type{}
	for i in 0 .. node.children_count {
		param_id := g.a.child(&node, i)
		p := g.a.node(param_id)
		if node_kind_id(p) == 75 && p.value.len > 0 {
			param_type := g.tc.parse_type(p.typ)
			g.cur_param_names << p.value
			g.cur_param_type_values << param_type
			g.cur_param_types[p.value] = param_type
			g.tc.cur_scope.insert(p.value, param_type)
		}
	}
	is_entry_main := node.value == 'main' && (module_name.len == 0 || module_name == 'main')
	if is_entry_main {
		g.writeln('int main(int argc, char** argv) {')
		if g.has_builtins {
			g.writeln('\tg_main_argc = argc;')
			g.writeln('\tg_main_argv = argv;')
		}
		if g.runtime_inits.len > 0 {
			g.writeln('\t_vinit();')
		}
	} else {
		ret_type := g.tc.parse_type(node.typ)
		g.set_cur_fn_ret(ret_type)
		g.write(g.optional_type_name(ret_type))
		g.write(' ')
		g.write(qualified_fn_name_in_module(module_name, node.value))
		g.write('(')
		g.write_fn_node_params(node)
		g.writeln(') {')
	}
	g.indent++

	for i in 0 .. node.children_count {
		id := g.a.child(&node, i)
		child := g.a.node(id)
		if child.kind != .param {
			g.gen_node(id)
		}
	}
	g.gen_defers()
	if is_entry_main {
		g.writeln('return 0;')
	} else if g.cur_fn_ret_is_optional {
		ct := g.optional_type_name(g.cur_fn_ret)
		g.writeln('return (${ct}){.ok = true};')
	}
	g.indent--
	g.writeln('}')
	g.writeln('')
	g.cur_param_names = old_param_names.clone()
	g.cur_param_type_values = old_param_type_values.clone()
	g.cur_param_types = old_param_types.clone()
	g.tc.pop_scope()
}

fn (mut g FlatGen) set_cur_fn_ret(ret_type types.Type) {
	g.cur_fn_ret = ret_type
	g.cur_fn_ret_is_optional = false
	g.cur_fn_ret_base = types.Type(types.void_)
	if ret_type is types.OptionType {
		g.cur_fn_ret_is_optional = true
		g.cur_fn_ret_base = ret_type.base_type
	} else if ret_type is types.ResultType {
		g.cur_fn_ret_is_optional = true
		g.cur_fn_ret_base = ret_type.base_type
	}
}

fn (mut g FlatGen) gen_defers() {
	g.gen_defers_from(0)
}

fn (mut g FlatGen) gen_defers_from(start int) {
	if g.defers.len == 0 {
		return
	}
	mut i := g.defers.len
	for i > start {
		i--
		defer_body := g.a.nodes[int(g.defers[i])]
		g.writeln('{')
		g.indent++
		for j in 0 .. defer_body.children_count {
			g.gen_node(g.a.child(&defer_body, j))
		}
		g.indent--
		g.writeln('}')
	}
}

fn (mut g FlatGen) trim_defers(start int) {
	if start >= g.defers.len {
		return
	}
	g.defers = g.defers[..start].clone()
}

fn (mut g FlatGen) gen_call(id flat.NodeId, node flat.Node) {
	fn_node := g.a.child_node(&node, 0)
	fn_name := fn_node.value
	if fn_node.kind == .selector && fn_node.value == 'str' {
		base_type := g.tc.resolve_type(g.a.child(fn_node, 0))
		clean_type := types.unwrap_pointer(base_type)
		if clean_type is types.Enum {
			g.gen_enum_str_call(fn_node, clean_type)
			return
		}
	}
	if fn_node.kind == .selector && fn_node.value == 'close' {
		base_id := g.a.child(fn_node, 0)
		base_type := g.tc.resolve_type(base_id)
		if base_type is types.Channel {
			g.write('sync__Channel__close(')
			g.gen_expr(base_id)
			g.write(', array_new(sizeof(IError), 0, 0))')
			return
		}
	}
	match fn_name {
		'new_map' {
			if node.typ.starts_with('map[') {
				map_type := g.tc.parse_type(node.typ)
				if map_type is types.Map {
					g.write_new_map(map_type.key_type, map_type.value_type)
					return
				}
			}
			g.write('new_map(')
			g.gen_call_args(fn_name, node, 1)
			g.write(')')
			return
		}
		'panic' {
			g.write('v_panic(')
			if node.children_count > 1 {
				arg_id := g.a.child(&node, 1)
				arg_type := g.tc.resolve_type(arg_id)
				if arg_type is types.Struct && arg_type.name == 'IError' {
					g.gen_expr(arg_id)
					g.write('.message')
				} else {
					g.gen_expr(arg_id)
				}
			}
			g.write(')')
			return
		}
		'error' {
			if g.cur_fn_ret_is_optional {
				ct := g.optional_type_name(g.cur_fn_ret)
				g.write('(${ct}){.ok = false}')
			} else {
				g.write('(IError){._typ = 0, ._object = NULL, .message = ')
				if node.children_count > 1 {
					g.gen_expr(g.a.child(&node, 1))
				} else {
					g.write('_S("")')
				}
				g.write(', .code = 0}')
			}
			return
		}
		'error_with_code' {
			if g.cur_fn_ret_is_optional {
				ct := g.optional_type_name(g.cur_fn_ret)
				g.write('(${ct}){.ok = false}')
			} else {
				g.write('(IError){._typ = 0, ._object = NULL, .message = ')
				if node.children_count > 1 {
					g.gen_expr(g.a.child(&node, 1))
				} else {
					g.write('_S("")')
				}
				g.write(', .code = ')
				if node.children_count > 2 {
					g.gen_expr(g.a.child(&node, 2))
				} else {
					g.write('0')
				}
				g.write('}')
			}
			return
		}
		else {
			mut is_method := false
			mut is_c_call := false
			mut method_name := ''
			mut base_id := flat.NodeId(0)
			if fn_node.kind == .selector {
				base := g.a.child_node(fn_node, 0)
				base_is_local := if base.kind == .ident {
					(g.tc.cur_scope.lookup(base.value) or { types.Type(types.void_) }) !is types.Void
				} else {
					false
				}
				if base.kind == .ident && base.value == 'C' {
					g.write(fn_node.value)
					is_c_call = true
				} else if g.is_flag_enum_method(fn_node) {
					g.gen_flag_enum_call(node)
					return
				} else if base.kind == .ident && !base_is_local && base.value in g.modules {
					mod := g.modules[base.value]
					short_mod := if mod.contains('.') {
						mod.all_after_last('.')
					} else {
						mod
					}
					full_name := '${short_mod}.${fn_node.value}'
					if full_name in g.tc.type_aliases || full_name in g.tc.structs
						|| full_name in g.tc.enum_names || full_name in g.tc.sum_types {
						target_type := g.tc.parse_type(full_name)
						ct := g.tc.c_type(target_type)
						if target_type is types.SumType && node.children_count > 1 {
							inner_id := g.a.child(&node, 1)
							inner := g.a.nodes[int(inner_id)]
							variant_name0 := if inner.kind == .struct_init
								|| inner.kind == .cast_expr {
								inner.value
							} else {
								g.tc.resolve_type(inner_id).name()
							}
							variant_name := g.resolve_variant(target_type.name, variant_name0)
							idx := g.sum_type_index(target_type.name, variant_name)
							field := g.sum_field_name(variant_name)
							if g.variant_references_sum(variant_name, target_type.name) {
								inner_ct := g.tc.c_type(g.tc.parse_type(variant_name))
								g.write('(${ct}){.typ = ${idx}, .${field} = (${inner_ct}*)memdup(&(${inner_ct}){')
								if inner.kind == .struct_init {
									for si in 0 .. inner.children_count {
										sf := g.a.child_node(&inner, si)
										if si > 0 {
											g.write(', ')
										}
										g.write('.${c_name(sf.value)} = ')
										g.gen_expr(g.a.child(sf, 0))
									}
								} else {
									g.gen_expr(inner_id)
								}
								g.write('}, sizeof(${inner_ct}))}')
							} else {
								g.write('(${ct}){.typ = ${idx}, .${field} = ')
								g.gen_expr(inner_id)
								g.write('}')
							}
						} else {
							g.write('(${ct})(')
							for i in 1 .. node.children_count {
								if i > 1 {
									g.write(', ')
								}
								g.gen_expr(g.a.child(&node, i))
							}
							g.write(')')
						}
						return
					}
					g.write(c_name(full_name))
					g.write('(')
					g.gen_call_args(full_name, node, 1)
					g.write(')')
					return
				} else if base.kind == .selector {
					inner := g.a.child_node(base, 0)
					inner_is_local := if inner.kind == .ident {
						(g.tc.cur_scope.lookup(inner.value) or { types.Type(types.void_) }) !is types.Void
					} else {
						false
					}
					if inner.kind == .ident && !inner_is_local && inner.value in g.modules {
						mod := g.modules[inner.value]
						short_mod := if mod.contains('.') {
							mod.all_after_last('.')
						} else {
							mod
						}
						full_name := '${short_mod}.${base.value}.${fn_node.value}'
						g.write(c_name(full_name))
						g.write('(')
						g.gen_call_args(full_name, node, 1)
						g.write(')')
						return
					} else {
						base_type := g.tc.resolve_type(g.a.child(fn_node, 0))
						clean_type := types.unwrap_pointer(base_type)
						if g.gen_fn_field_call(node, fn_node, base_type) {
							return
						}
						if arr := array_like_type(clean_type) {
							g.gen_array_method_call(node, fn_node, arr)
							return
						}
						if clean_type is types.ArrayFixed && fn_node.value == 'bytestr' {
							g.write('u8__vstring_with_len((u8*)')
							g.gen_expr(g.a.child(fn_node, 0))
							len_expr := g.fixed_array_len_value(clean_type)
							g.write(', ${len_expr})')
							return
						}
						if clean_type is types.Map {
							if fn_node.value == 'delete' {
								g.gen_map_delete(node, fn_node, clean_type)
								return
							} else if fn_node.value == 'clone' {
								g.write('map__clone(&')
								g.gen_expr(g.a.child(fn_node, 0))
								g.write(')')
								return
							} else if fn_node.value == 'clear' {
								g.write('map__clear(&')
								g.gen_expr(g.a.child(fn_node, 0))
								g.write(')')
								return
							} else if fn_node.value == 'free' {
								g.write('map__free(')
								if base_type is types.Pointer {
									g.gen_expr(g.a.child(fn_node, 0))
								} else {
									g.write('&')
									g.gen_expr(g.a.child(fn_node, 0))
								}
								g.write(')')
								return
							}
						}
						if clean_type is types.String {
							method_name = 'string.${fn_node.value}'
							if method_name in g.tc.fn_param_types {
								is_method = true
								base_id = g.a.child(fn_node, 0)
								g.write(c_name(method_name))
							} else {
								g.write('string__${fn_node.value}(')
								g.gen_expr(g.a.child(fn_node, 0))
								for i in 1 .. node.children_count {
									g.write(', ')
									g.gen_expr(g.a.child(&node, i))
								}
								g.write(')')
								return
							}
						}
						if !is_method && (clean_type is types.Primitive
							|| clean_type is types.ISize || clean_type is types.USize
							|| clean_type is types.Rune) {
							tname := clean_type.name()
							prim_method := '${tname}.${fn_node.value}'
							if prim_method in g.tc.fn_param_types {
								is_method = true
								base_id = g.a.child(fn_node, 0)
								g.write(c_name(prim_method))
							} else {
								mut prim_found := false
								if alias_method := g.find_alias_method(tname, fn_node.value) {
									is_method = true
									prim_found = true
									base_id = g.a.child(fn_node, 0)
									g.write(c_name(alias_method))
								}
								if !prim_found {
									alt_name := g.find_prim_method(fn_node.value)
									if alt_name.len > 0 {
										is_method = true
										base_id = g.a.child(fn_node, 0)
										g.write(alt_name)
									}
								}
							}
						}
						if !is_method {
							mut struct_name := clean_type.name()
							if clean_type is types.Struct {
								struct_name = clean_type.name
							}
							method_name = '${struct_name}.${fn_node.value}'
							if method_name !in g.tc.fn_param_types {
								for alias, target in g.tc.type_aliases {
									if target == struct_name {
										alias_method := '${alias}.${fn_node.value}'
										if alias_method in g.tc.fn_param_types {
											method_name = alias_method
											break
										}
									}
								}
							}
							if method_name in g.tc.fn_param_types {
								is_method = true
								base_id = g.a.child(fn_node, 0)
								g.write(c_name(method_name))
							} else {
								str_method := 'string.${fn_node.value}'
								if str_method in g.tc.fn_param_types {
									is_method = true
									method_name = str_method
									base_id = g.a.child(fn_node, 0)
									g.write(c_name(str_method))
								} else if struct_name.len > 0 {
									is_method = true
									base_id = g.a.child(fn_node, 0)
									g.write(c_name(method_name))
								} else {
									g.gen_expr(g.a.child(&node, 0))
								}
							}
						}
					}
				} else if base.kind == .ident
					&& (base.value in g.tc.structs || base.value in g.tc.enum_names || g.tc.qualify_name(base.value) in g.tc.structs
					|| g.tc.qualify_name(base.value) in g.tc.enum_names) {
					qname := if base.value in g.tc.structs || base.value in g.tc.enum_names {
						base.value
					} else {
						g.tc.qualify_name(base.value)
					}
					static_name := '${qname}.${fn_node.value}'
					g.write(c_name(static_name))
					g.write('(')
					for i in 1 .. node.children_count {
						if i > 1 {
							g.write(', ')
						}
						g.gen_expr(g.a.child(&node, i))
					}
					g.write(')')
					return
				} else {
					base_type := g.tc.resolve_type(g.a.child(fn_node, 0))
					clean_type := types.unwrap_pointer(base_type)
					if g.gen_fn_field_call(node, fn_node, base_type) {
						return
					}
					if arr := array_like_type(clean_type) {
						g.gen_array_method_call(node, fn_node, arr)
						return
					}
					if clean_type is types.ArrayFixed && fn_node.value == 'bytestr' {
						g.write('u8__vstring_with_len((u8*)')
						g.gen_expr(g.a.child(fn_node, 0))
						len_expr := g.fixed_array_len_value(clean_type)
						g.write(', ${len_expr})')
						return
					}
					if clean_type is types.Map {
						if fn_node.value == 'delete' {
							g.gen_map_delete(node, fn_node, clean_type)
							return
						} else if fn_node.value == 'clone' {
							g.write('map__clone(&')
							g.gen_expr(g.a.child(fn_node, 0))
							g.write(')')
							return
						} else if fn_node.value == 'clear' {
							g.write('map__clear(&')
							g.gen_expr(g.a.child(fn_node, 0))
							g.write(')')
							return
						} else if fn_node.value == 'free' {
							g.write('map__free(')
							if base_type is types.Pointer {
								g.gen_expr(g.a.child(fn_node, 0))
							} else {
								g.write('&')
								g.gen_expr(g.a.child(fn_node, 0))
							}
							g.write(')')
							return
						}
					}
					if clean_type is types.String {
						method_name = 'string.${fn_node.value}'
						if method_name in g.tc.fn_param_types {
							is_method = true
							base_id = g.a.child(fn_node, 0)
							g.write(c_name(method_name))
						} else {
							g.write('string__${fn_node.value}(')
							g.gen_expr(g.a.child(fn_node, 0))
							for i in 1 .. node.children_count {
								g.write(', ')
								g.gen_expr(g.a.child(&node, i))
							}
							g.write(')')
							return
						}
					}
					if !is_method && (clean_type is types.Void || clean_type is types.Primitive)
						&& fn_node.value in ['vstring', 'vstring_with_len'] {
						g.write('u8__${fn_node.value}((u8*)')
						g.gen_expr(g.a.child(fn_node, 0))
						for i in 1 .. node.children_count {
							g.write(', ')
							g.gen_expr(g.a.child(&node, i))
						}
						g.write(')')
						return
					}
					if !is_method && clean_type is types.Struct && clean_type.name == 'IError' {
						if fn_node.value == 'msg' {
							g.gen_expr(g.a.child(fn_node, 0))
							g.write('.message')
							return
						} else if fn_node.value == 'code' {
							g.gen_expr(g.a.child(fn_node, 0))
							g.write('.code')
							return
						}
					}
					if !is_method && clean_type is types.Struct && clean_type.name == 'array'
						&& fn_node.value == 'free' {
						g.write('array__free(')
						if base_type is types.Pointer {
							g.gen_expr(g.a.child(fn_node, 0))
						} else {
							g.write('&')
							g.gen_expr(g.a.child(fn_node, 0))
						}
						g.write(')')
						return
					}
					if !is_method && (clean_type is types.Primitive
						|| clean_type is types.ISize || clean_type is types.USize
						|| clean_type is types.Rune) {
						tname := clean_type.name()
						prim_method := '${tname}.${fn_node.value}'
						if prim_method in g.tc.fn_param_types {
							is_method = true
							base_id = g.a.child(fn_node, 0)
							g.write(c_name(prim_method))
						} else {
							mut prim_found := false
							if alias_method := g.find_alias_method(tname, fn_node.value) {
								is_method = true
								prim_found = true
								base_id = g.a.child(fn_node, 0)
								g.write(c_name(alias_method))
							}
							if !prim_found {
								alt_name := g.find_prim_method(fn_node.value)
								if alt_name.len > 0 {
									is_method = true
									base_id = g.a.child(fn_node, 0)
									g.write(alt_name)
								}
							}
						}
					}
					if !is_method {
						mut struct_name := clean_type.name()
						if clean_type is types.Struct {
							struct_name = clean_type.name
						}
						method_name = '${struct_name}.${fn_node.value}'
						if method_name !in g.tc.fn_param_types {
							for alias, target in g.tc.type_aliases {
								if target == struct_name {
									alias_method := '${alias}.${fn_node.value}'
									if alias_method in g.tc.fn_param_types {
										method_name = alias_method
										break
									}
								}
							}
						}
						if method_name in g.tc.fn_param_types {
							is_method = true
							base_id = g.a.child(fn_node, 0)
							g.write(c_name(method_name))
						} else {
							str_method := 'string.${fn_node.value}'
							if str_method in g.tc.fn_param_types {
								is_method = true
								method_name = str_method
								base_id = g.a.child(fn_node, 0)
								g.write(c_name(str_method))
							} else if struct_name.len > 0 {
								is_method = true
								base_id = g.a.child(fn_node, 0)
								g.write(c_name(method_name))
							} else {
								g.gen_expr(g.a.child(&node, 0))
							}
						}
					}
					// !is_method
				}
			} else {
				fn_id := g.a.child(&node, 0)
				fn_ident := g.a.nodes[int(fn_id)]
				if fn_ident.kind == .ident {
					qname := g.tc.qualify_name(fn_ident.value)
					if fn_ident.value in g.tc.type_aliases || qname in g.tc.type_aliases
						|| fn_ident.value in g.tc.structs || qname in g.tc.structs
						|| fn_ident.value in g.tc.enum_names || qname in g.tc.enum_names
						|| fn_ident.value in g.tc.sum_types || qname in g.tc.sum_types {
						target_type := g.tc.parse_type(fn_ident.value)
						ct := g.tc.c_type(target_type)
						if target_type is types.SumType && node.children_count > 1 {
							inner_id := g.a.child(&node, 1)
							inner := g.a.nodes[int(inner_id)]
							variant_name0 := if inner.kind == .struct_init
								|| inner.kind == .cast_expr {
								inner.value
							} else {
								g.tc.resolve_type(inner_id).name()
							}
							variant_name := g.resolve_variant(target_type.name, variant_name0)
							idx := g.sum_type_index(target_type.name, variant_name)
							field := g.sum_field_name(variant_name)
							if g.variant_references_sum(variant_name, target_type.name) {
								inner_ct := g.tc.c_type(g.tc.parse_type(variant_name))
								g.write('(${ct}){.typ = ${idx}, .${field} = (${inner_ct}*)memdup(&(${inner_ct}){')
								if inner.kind == .struct_init {
									for si in 0 .. inner.children_count {
										sf := g.a.child_node(&inner, si)
										if si > 0 {
											g.write(', ')
										}
										g.write('.${c_name(sf.value)} = ')
										g.gen_expr(g.a.child(sf, 0))
									}
								} else {
									g.gen_expr(inner_id)
								}
								g.write('}, sizeof(${inner_ct}))}')
							} else {
								g.write('(${ct}){.typ = ${idx}, .${field} = ')
								g.gen_expr(inner_id)
								g.write('}')
							}
						} else {
							g.write('(${ct})(')
							for i in 1 .. node.children_count {
								if i > 1 {
									g.write(', ')
								}
								g.gen_expr(g.a.child(&node, i))
							}
							g.write(')')
						}
						return
					}
					call_key := g.call_key(id, fn_ident.value)
					if call_key in g.tc.fn_ret_types || call_key in g.tc.fn_param_types {
						g.write(g.direct_call_name(call_key))
					} else {
						g.write(g.direct_call_name(fn_ident.value))
					}
				} else {
					g.gen_expr(fn_id)
				}
			}
			g.write('(')
			actual_fn := if is_method {
				method_name
			} else {
				g.call_key(id, fn_name)
			}
			param_types := g.param_types_for(actual_fn, fn_name)
			mut arg_start := 1
			if is_method {
				base_type := g.receiver_base_type(base_id)
				is_ptr_base := base_type is types.Pointer
				wants_ptr := param_types.len > 0 && param_types[0] is types.Pointer
				if wants_ptr && !is_ptr_base {
					g.write('&')
				} else if !wants_ptr && is_ptr_base {
					g.write('*')
				}
				g.gen_expr(base_id)
				arg_start = 1
			}
			num_call_args := node.children_count - arg_start
			is_variadic_fn := !is_method && !is_c_call && (g.tc.fn_variadic[actual_fn] or { false })
			variadic_idx := if is_variadic_fn && param_types.len > 0
				&& param_types[param_types.len - 1] is types.Array {
				param_types.len - 1
			} else {
				-1
			}
			for i in arg_start .. node.children_count {
				if is_method || i > 1 {
					g.write(', ')
				}
				arg_idx := if is_method { i } else { i - 1 }
				arg_id := g.a.child(&node, i)
				arg_node := g.a.nodes[int(arg_id)]
				if arg_node.kind == .field_init {
					// `@[params]` struct argument: trailing `key: value` args form a struct literal
					ptyp := if arg_idx < param_types.len {
						param_types[arg_idx]
					} else {
						types.Type(types.void_)
					}
					g.gen_params_struct_arg(ptyp, node, i)
					break
				}
				if !is_method && actual_fn == 'array_push_many' && arg_idx == 1
					&& arg_node.kind == .array_literal {
					elem_type := if arg_node.children_count > 0 {
						g.tc.resolve_type(g.a.child(&arg_node, 0))
					} else {
						types.Type(types.int_)
					}
					g.gen_array_literal_value(arg_node, elem_type)
					continue
				}
				if variadic_idx >= 0 && arg_idx == variadic_idx {
					variadic_type := param_types[variadic_idx]
					if variadic_type is types.Array {
						if num_call_args > param_types.len {
							c_elem := g.tc.c_type(variadic_type.elem_type)
							count := num_call_args - variadic_idx
							g.write('new_array_from_c_array(${count}, ${count}, sizeof(${c_elem}), (${c_elem}[]){')
							for j in i .. node.children_count {
								if j > i {
									g.write(', ')
								}
								g.gen_expr_with_expected_type(g.a.child(&node, j),
									variadic_type.elem_type)
							}
							g.write('})')
							break
						}
						arg_type := g.tc.resolve_type(arg_id)
						if arg_type !is types.Array {
							c_elem := g.tc.c_type(variadic_type.elem_type)
							g.write('new_array_from_c_array(1, 1, sizeof(${c_elem}), (${c_elem}[]){')
							g.gen_expr_with_expected_type(arg_id, variadic_type.elem_type)
							g.write('})')
							continue
						}
					}
				}
				mut needs_addr := false
				if !is_c_call && arg_idx < param_types.len && param_types[arg_idx] is types.Pointer
					&& !(arg_node.kind == .prefix && arg_node.op == .amp) {
					arg_type := g.tc.resolve_type(arg_id)
					if arg_type !is types.Pointer {
						needs_addr = true
					}
				}
				if !is_c_call && arg_idx < param_types.len {
					pt := param_types[arg_idx]
					if pt is types.Enum {
						g.expected_enum = pt.name
					}
				}
				is_rvalue := arg_node.kind == .call
					|| (arg_node.kind == .index && arg_node.value == 'range')
				if needs_addr && is_rvalue {
					pt := param_types[arg_idx]
					ct := g.tc.c_type(types.unwrap_pointer(pt))
					g.write('({${ct} _t${g.tmp_count} = ')
					g.gen_expr_with_expected_type(arg_id, types.unwrap_pointer(pt))
					g.write('; &_t${g.tmp_count};})')
					g.tmp_count++
				} else {
					if needs_addr {
						g.write('&')
					}
					emitted_variant := !needs_addr && !is_c_call && arg_idx < param_types.len
						&& g.gen_sum_variant_arg(arg_id, param_types[arg_idx])
					if !emitted_variant {
						if !is_c_call && arg_idx < param_types.len
							&& g.gen_optional_arg(arg_id, param_types[arg_idx]) {
							// handled
						} else if !is_c_call && arg_idx < param_types.len {
							g.gen_expr_with_expected_type(arg_id, param_types[arg_idx])
						} else {
							g.gen_expr(arg_id)
						}
					}
				}
				g.expected_enum = ''
			}
			actual_args := node.children_count - arg_start
			expected_args := if is_method {
				param_types.len - 1
			} else {
				param_types.len
			}
			if !is_c_call && expected_args > 0 && actual_args < expected_args {
				for pi in actual_args .. expected_args {
					if is_method || pi > 0 {
						g.write(', ')
					}
					pidx := if is_method { pi + 1 } else { pi }
					pt := param_types[pidx]
					g.gen_default_value_for_type(pt)
				}
			}
			g.write(')')
		}
	}
}

fn (g &FlatGen) receiver_base_type(base_id flat.NodeId) types.Type {
	if int(base_id) < 0 {
		return types.Type(types.void_)
	}
	base := g.a.nodes[int(base_id)]
	if base.kind == .ident {
		if typ := g.current_param_type(base.value) {
			return typ
		}
		if typ := g.cur_param_types[base.value] {
			return typ
		}
		if typ := g.tc.cur_scope.lookup(base.value) {
			return typ
		}
	}
	return g.tc.resolve_type(base_id)
}

fn (g &FlatGen) current_param_type(name string) ?types.Type {
	for i in 0 .. g.cur_param_names.len {
		if g.cur_param_names[i] == name {
			return g.cur_param_type_values[i]
		}
	}
	return none
}

fn (mut g FlatGen) gen_enum_str_call(fn_node &flat.Node, enum_type types.Enum) {
	fields := g.tc.enum_fields[enum_type.name] or { []string{} }
	if fields.len == 0 {
		sid := g.intern_string('')
		g.write('_str_${sid}')
		return
	}
	g.write('({ int _e${g.tmp_count} = ')
	g.gen_expr(g.a.child(fn_node, 0))
	g.write('; ')
	for field in fields {
		ekey := '${enum_type.name}.${field}'
		if ekey in g.enum_vals {
			val := g.enum_vals[ekey]
			sid := g.intern_string(field)
			g.write('_e${g.tmp_count} == ${val} ? _str_${sid} : ')
		}
	}
	unknown := g.intern_string('')
	g.write('_str_${unknown}; })')
	g.tmp_count++
}

fn (mut g FlatGen) gen_fn_field_call(node flat.Node, fn_node &flat.Node, base_type types.Type) bool {
	fn_type := g.fn_field_type(base_type, fn_node.value) or { return false }
	base_id := g.a.child(fn_node, 0)
	base := g.a.nodes[int(base_id)]
	needs_paren := base.kind !in [.ident, .selector, .call]
	if needs_paren {
		g.write('(')
	}
	g.gen_expr(base_id)
	if needs_paren {
		g.write(')')
	}
	if base_type is types.Pointer {
		g.write('->')
	} else {
		g.write('.')
	}
	g.write(c_name(fn_node.value))
	g.write('(')
	for i in 1 .. node.children_count {
		if i > 1 {
			g.write(', ')
		}
		arg_id := g.a.child(&node, i)
		arg_idx := i - 1
		if arg_idx < fn_type.params.len {
			g.gen_arg_for_expected_type(arg_id, fn_type.params[arg_idx])
		} else {
			g.gen_expr(arg_id)
		}
	}
	g.write(')')
	return true
}

fn (g &FlatGen) call_key(id flat.NodeId, name string) string {
	if resolved := g.tc.resolved_call_name(id) {
		return g.normalize_call_key(resolved)
	}
	return g.normalize_call_key(name)
}

fn (g &FlatGen) normalize_call_key(name string) string {
	if name.starts_with('main.') {
		short_name := name.all_after_last('.')
		if short_name in g.tc.fn_param_types || short_name in g.tc.fn_ret_types {
			return short_name
		}
	}
	if !name.contains('.') && g.tc.cur_module.len > 0 && g.tc.cur_module != 'main'
		&& g.tc.cur_module != 'builtin' {
		local := '${g.tc.cur_module}.${name}'
		if local in g.tc.fn_param_types || local in g.tc.fn_ret_types {
			return local
		}
	}
	if name in g.tc.fn_param_types || name in g.tc.fn_ret_types {
		return name
	}
	qname := g.tc.qualify_fn_name(name)
	if qname in g.tc.fn_param_types || qname in g.tc.fn_ret_types {
		return qname
	}
	for _, mod_name in g.tc.imports {
		imported := '${mod_name}.${name}'
		if imported in g.tc.fn_param_types || imported in g.tc.fn_ret_types {
			return imported
		}
	}
	return qname
}

fn (mut g FlatGen) param_types_for(name string, fallback string) []types.Type {
	decl_types := g.param_types_from_decl(name, fallback)
	if decl_types.len > 0 {
		return decl_types
	}
	for candidate in [name, fallback] {
		if candidate in g.tc.fn_param_types {
			return g.tc.fn_param_types[candidate]
		}
		short_method := short_receiver_method_name(candidate)
		if short_method.len > 0 {
			if short_method in g.tc.fn_param_types {
				return g.tc.fn_param_types[short_method]
			}
		}
		if candidate.starts_with('main.') {
			short_name := candidate.all_after_last('.')
			if short_name in g.tc.fn_param_types {
				return g.tc.fn_param_types[short_name]
			}
		}
	}
	return []types.Type{}
}

fn (mut g FlatGen) param_types_from_decl(name string, fallback string) []types.Type {
	if name.contains('.') {
		if ptypes := g.fn_decl_param_types[name] {
			return ptypes
		}
		short_method := short_receiver_method_name(name)
		if short_method.len > 0 {
			if ptypes := g.fn_decl_param_types[short_method] {
				return ptypes
			}
		}
	} else {
		for candidate in [fallback, name] {
			if ptypes := g.fn_decl_param_types[candidate] {
				return ptypes
			}
		}
	}
	return []types.Type{}
}

fn short_receiver_method_name(name string) string {
	if !name.contains('.') {
		return ''
	}
	receiver := name.all_before_last('.')
	if !receiver.contains('.') {
		return ''
	}
	return '${receiver.all_after_last('.')}.${name.all_after_last('.')}'
}

fn (mut g FlatGen) gen_arg_for_expected_type(arg_id flat.NodeId, expected types.Type) {
	arg_node := g.a.nodes[int(arg_id)]
	mut needs_addr := false
	if expected is types.Pointer && !(arg_node.kind == .prefix && arg_node.op == .amp) {
		arg_type := g.tc.resolve_type(arg_id)
		if arg_type !is types.Pointer {
			needs_addr = true
		}
	}
	if needs_addr {
		g.write('&')
	}
	if !needs_addr && g.gen_sum_variant_arg(arg_id, expected) {
		return
	}
	if !needs_addr && g.gen_optional_arg(arg_id, expected) {
		return
	}
	g.gen_expr_with_expected_type(arg_id, expected)
}

fn (mut g FlatGen) gen_optional_arg(arg_id flat.NodeId, expected types.Type) bool {
	mut base_type := types.Type(types.void_)
	if expected is types.OptionType {
		base_type = expected.base_type
	} else if expected is types.ResultType {
		base_type = expected.base_type
	} else {
		return false
	}
	if g.expr_is_optional_literal(arg_id, expected) {
		g.gen_expr_with_expected_type(g.collapsed_optional_literal(arg_id, expected), expected)
		return true
	}
	arg_type := g.usable_expr_type(arg_id)
	if arg_type is types.OptionType || arg_type is types.ResultType {
		if g.type_names_match(arg_type, expected) {
			g.gen_expr_with_expected_type(arg_id, expected)
			return true
		}
		arg_node := g.a.nodes[int(arg_id)]
		if arg_node.kind == .none_expr || g.expr_really_returns_optional(arg_id) {
			g.gen_expr_with_expected_type(arg_id, expected)
			return true
		}
	}
	ct := g.optional_type_name(expected)
	if base_type is types.Void {
		g.write('(${ct}){.ok = true}')
		return true
	}
	g.write('(${ct}){.ok = true, .value = ')
	g.gen_expr_with_expected_type(arg_id, base_type)
	g.write('}')
	return true
}

fn (mut g FlatGen) expr_is_optional_literal(id flat.NodeId, expected types.Type) bool {
	if int(id) < 0 {
		return false
	}
	node := g.a.nodes[int(id)]
	if node.kind != .struct_init && node.kind != .cast_expr {
		return false
	}
	return node.value.starts_with('?') || node.value.starts_with('!')
		|| node.value == g.optional_type_name(expected) || node.value.starts_with('Optional')
}

fn (mut g FlatGen) collapsed_optional_literal(id flat.NodeId, expected types.Type) flat.NodeId {
	mut current := id
	for _ in 0 .. 4 {
		value_id := g.optional_literal_value_id(current) or { break }
		if !g.expr_is_optional_literal(value_id, expected) {
			break
		}
		current = value_id
	}
	return current
}

fn (g &FlatGen) optional_literal_value_id(id flat.NodeId) ?flat.NodeId {
	if int(id) < 0 {
		return none
	}
	node := g.a.nodes[int(id)]
	if node.kind != .struct_init && node.kind != .cast_expr {
		return none
	}
	for i in 0 .. node.children_count {
		field := g.a.child_node(&node, i)
		if field.kind == .field_init && field.value == 'value' && field.children_count > 0 {
			return g.a.child(field, 0)
		}
	}
	return none
}

fn (g &FlatGen) fn_field_type(base_type types.Type, field_name string) ?types.FnType {
	field_type := g.field_type(base_type, field_name) or { return none }
	return fn_type_from(field_type)
}

fn (g &FlatGen) field_type(base_type types.Type, field_name string) ?types.Type {
	clean0 := types.unwrap_pointer(base_type)
	mut clean := clean0
	if clean0 is types.Alias {
		clean = clean0.base_type
	}
	mut struct_name := ''
	if clean is types.Struct {
		struct_name = clean.name
	} else if clean is types.Array {
		struct_name = 'array'
	} else if clean is types.Map {
		struct_name = 'map'
	} else if clean is types.String {
		struct_name = 'string'
	}
	if struct_name.len == 0 {
		return none
	}
	fields := g.tc.structs[struct_name] or { return none }
	for field in fields {
		if field.name == field_name {
			return field.typ
		}
	}
	return none
}

fn fn_type_from(t types.Type) ?types.FnType {
	if t is types.FnType {
		return t
	}
	if t is types.Alias {
		return fn_type_from(t.base_type)
	}
	return none
}

fn (mut g FlatGen) gen_call_args(fn_name string, node flat.Node, start int) {
	mut param_types := []types.Type{}
	if fn_name in g.tc.fn_param_types {
		param_types = g.tc.fn_param_types[fn_name]
	}
	is_variadic_fn := g.tc.fn_variadic[fn_name] or { false }
	variadic_idx := if is_variadic_fn && param_types.len > 0
		&& param_types[param_types.len - 1] is types.Array {
		param_types.len - 1
	} else {
		-1
	}
	num_args := node.children_count - start
	is_variadic := variadic_idx >= 0 && num_args > param_types.len
	for i in start .. node.children_count {
		if i > start {
			g.write(', ')
		}
		arg_idx := i - start
		arg_id := g.a.child(&node, i)
		arg_node := g.a.nodes[int(arg_id)]
		if arg_node.kind == .field_init {
			// `@[params]` struct argument: trailing `key: value` args form a struct literal
			ptyp := if arg_idx < param_types.len {
				param_types[arg_idx]
			} else {
				types.Type(types.void_)
			}
			g.gen_params_struct_arg(ptyp, node, i)
			break
		}
		if fn_name == 'array_push_many' && arg_idx == 1 && arg_node.kind == .array_literal {
			elem_type := if arg_node.children_count > 0 {
				g.tc.resolve_type(g.a.child(&arg_node, 0))
			} else {
				types.Type(types.int_)
			}
			g.gen_array_literal_value(arg_node, elem_type)
			continue
		}
		if is_variadic && arg_idx == variadic_idx {
			variadic_type := param_types[variadic_idx]
			if variadic_type is types.Array {
				c_elem := g.tc.c_type(variadic_type.elem_type)
				count := num_args - variadic_idx
				g.write('new_array_from_c_array(${count}, ${count}, sizeof(${c_elem}), (${c_elem}[]){')
				for j in i .. node.children_count {
					if j > i {
						g.write(', ')
					}
					g.gen_expr_with_expected_type(g.a.child(&node, j), variadic_type.elem_type)
				}
				g.write('})')
			}
			break
		}
		if variadic_idx >= 0 && arg_idx == variadic_idx && num_args == param_types.len {
			arg_type := g.tc.resolve_type(arg_id)
			if arg_type !is types.Array {
				variadic_type := param_types[variadic_idx]
				if variadic_type is types.Array {
					c_elem := g.tc.c_type(variadic_type.elem_type)
					g.write('new_array_from_c_array(1, 1, sizeof(${c_elem}), (${c_elem}[]){')
					g.gen_expr_with_expected_type(arg_id, variadic_type.elem_type)
					g.write('})')
					continue
				}
			}
		}
		mut needs_addr := false
		if arg_idx < param_types.len && param_types[arg_idx] is types.Pointer
			&& !(arg_node.kind == .prefix && arg_node.op == .amp) {
			arg_type := g.tc.resolve_type(arg_id)
			if arg_type !is types.Pointer {
				needs_addr = true
			}
		}
		is_rvalue := arg_node.kind == .call
			|| (arg_node.kind == .index && arg_node.value == 'range')
		if needs_addr && is_rvalue {
			pt := param_types[arg_idx]
			ct := g.tc.c_type(types.unwrap_pointer(pt))
			g.write('({${ct} _t${g.tmp_count} = ')
			g.gen_expr_with_expected_type(arg_id, types.unwrap_pointer(pt))
			g.write('; &_t${g.tmp_count};})')
			g.tmp_count++
		} else {
			if needs_addr {
				g.write('&')
			}
			emitted_variant := !needs_addr && arg_idx < param_types.len
				&& g.gen_sum_variant_arg(arg_id, param_types[arg_idx])
			if !emitted_variant {
				if arg_idx < param_types.len && g.gen_optional_arg(arg_id, param_types[arg_idx]) {
					// handled
				} else if arg_idx < param_types.len {
					g.gen_expr_with_expected_type(arg_id, param_types[arg_idx])
				} else {
					g.gen_expr(arg_id)
				}
			}
		}
		if variadic_idx >= 0 && num_args == variadic_idx {
			if node.children_count > start {
				g.write(', ')
			}
			variadic_type := param_types[variadic_idx]
			if variadic_type is types.Array {
				c_elem := g.tc.c_type(variadic_type.elem_type)
				g.write('new_array_from_c_array(0, 0, sizeof(${c_elem}), (${c_elem}[]){0})')
			}
		}
	}
	num_provided := node.children_count - start
	if num_provided < param_types.len {
		for i in num_provided .. param_types.len {
			if num_provided > 0 || i > num_provided {
				g.write(', ')
			}
			g.gen_default_value_for_type(param_types[i])
		}
	}
}

fn (g &FlatGen) is_flag_enum_method(fn_node &flat.Node) bool {
	if fn_node.kind != .selector {
		return false
	}
	method := fn_node.value
	if method !in ['has', 'all', 'set', 'clear'] {
		return false
	}
	base_type := g.tc.resolve_type(g.a.child(fn_node, 0))
	clean := types.unwrap_pointer(base_type)
	if clean is types.Enum {
		return true
	} else if clean is types.Primitive {
		return clean.props.has(.integer)
	} else if clean is types.Unknown {
		return true
	}
	return false
}

fn (mut g FlatGen) gen_flag_enum_call(node flat.Node) {
	fn_node := g.a.child_node(&node, 0)
	method := fn_node.value
	base_id := g.a.child(fn_node, 0)
	base_type := types.unwrap_pointer(g.tc.resolve_type(base_id))
	match method {
		'has' {
			g.write('((')
			g.gen_expr(base_id)
			g.write(' & ')
			if node.children_count > 1 {
				g.gen_flag_enum_arg(g.a.child(&node, 1), base_type)
			}
			g.write(') != 0)')
		}
		'all' {
			g.write('((')
			g.gen_expr(base_id)
			g.write(' & (')
			if node.children_count > 1 {
				g.gen_flag_enum_arg(g.a.child(&node, 1), base_type)
			}
			g.write(')) == (')
			if node.children_count > 1 {
				g.gen_flag_enum_arg(g.a.child(&node, 1), base_type)
			}
			g.write('))')
		}
		'set' {
			g.gen_expr(base_id)
			g.write(' |= ')
			if node.children_count > 1 {
				g.gen_flag_enum_arg(g.a.child(&node, 1), base_type)
			}
		}
		'clear' {
			g.gen_expr(base_id)
			g.write(' &= ~(')
			if node.children_count > 1 {
				g.gen_flag_enum_arg(g.a.child(&node, 1), base_type)
			}
			g.write(')')
		}
		else {}
	}
}

fn (mut g FlatGen) gen_flag_enum_arg(arg_id flat.NodeId, base_type types.Type) {
	if base_type is types.Enum {
		g.gen_expr_with_expected_type(arg_id, base_type)
	} else {
		g.gen_expr(arg_id)
	}
}

fn is_generic_type(typ string) bool {
	t := typ.trim_left('&?!')
	return t.len == 1 && t[0] >= `A` && t[0] <= `Z`
}

fn (g &FlatGen) has_generic_params(node flat.Node) bool {
	for i in 0 .. node.children_count {
		child := g.a.child_node(&node, i)
		if child.kind == .param && is_generic_type(child.typ) {
			return true
		}
	}
	return is_generic_type(node.typ)
}

fn (g &FlatGen) find_prim_method(method string) string {
	if 'u8.${method}' in g.tc.fn_param_types {
		return c_name('u8.${method}')
	}
	if 'int.${method}' in g.tc.fn_param_types {
		return c_name('int.${method}')
	}
	if 'i64.${method}' in g.tc.fn_param_types {
		return c_name('i64.${method}')
	}
	if 'u32.${method}' in g.tc.fn_param_types {
		return c_name('u32.${method}')
	}
	if 'u64.${method}' in g.tc.fn_param_types {
		return c_name('u64.${method}')
	}
	return ''
}

fn (g &FlatGen) find_alias_method(target string, method string) ?string {
	mut fallback := ''
	for alias, alias_target in g.tc.type_aliases {
		if alias_target != target {
			continue
		}
		alias_method := '${alias}.${method}'
		if alias_method !in g.tc.fn_param_types {
			if alias.contains('.') {
				short_method := '${alias.all_after_last('.')}.${method}'
				if short_method in g.tc.fn_param_types {
					return alias_method
				}
			}
			continue
		}
		if alias.contains('.') {
			return alias_method
		}
		if fallback.len == 0 {
			fallback = alias_method
		}
	}
	if fallback.len > 0 {
		return fallback
	}
	return none
}

fn (mut g FlatGen) gen_sum_variant_arg(arg_id flat.NodeId, expected types.Type) bool {
	actual0 := types.unwrap_pointer(g.tc.resolve_type(arg_id))
	mut actual := actual0
	if actual0 is types.Alias {
		actual = actual0.base_type
	}
	expected0 := expected
	mut expected_type := expected0
	if expected0 is types.Alias {
		expected_type = expected0.base_type
	}
	if expected_type is types.SumType {
		return false
	}
	if actual !is types.SumType {
		return false
	}
	sum_type := actual as types.SumType
	sum_name := sum_type.name
	variant := g.resolve_variant(sum_name, expected_type.name())
	variants := g.tc.sum_types[sum_name] or { return false }
	if variant !in variants {
		return false
	}
	is_ptr_arg := g.tc.resolve_type(arg_id) is types.Pointer
	is_ref_variant := g.variant_references_sum(variant, sum_name)
	if is_ref_variant {
		g.write('(*')
	}
	g.gen_expr(arg_id)
	if is_ptr_arg {
		g.write('->')
	} else {
		g.write('.')
	}
	g.write(g.sum_field_name(variant))
	if is_ref_variant {
		g.write(')')
	}
	return true
}

fn (mut g FlatGen) forward_decls() {
	mut cur_module := ''
	for i in 0 .. g.a.nodes.len {
		node := g.a.nodes[i]
		kind_id := node_kind_id(node)
		if kind_id == 77 {
			cur_module = ''
			g.tc.cur_module = cur_module
			continue
		}
		if kind_id == 73 {
			cur_module = node.value
			g.tc.cur_module = cur_module
			continue
		}

		is_entry_main := node.value == 'main' && (cur_module.len == 0 || cur_module == 'main')
		if kind_id == 61 && !is_entry_main {
			if !g.should_emit_fn_node_in_module(node, i, cur_module) {
				continue
			}
			qfn := qualified_fn_name_in_module(cur_module, node.value)
			g.tc.cur_module = cur_module
			ret_type := g.tc.parse_type(node.typ)
			g.write(g.optional_type_name(ret_type))
			g.write(' ')
			g.write(qfn)
			g.write('(')
			g.write_fn_node_params(node)
			g.writeln(');')
		} else if kind_id == 76
			&& (node.value.starts_with('C.v_filelock_') || node.value.starts_with('v_filelock_')) {
			g.tc.cur_module = cur_module
			ret_type := g.tc.parse_type(node.typ)
			g.write(g.optional_type_name(ret_type))
			g.write(' ')
			g.write(c_name(node.value))
			g.write('(')
			g.write_c_fn_node_params(node)
			g.writeln(');')
		}
	}
	g.writeln('')
}

fn (mut g FlatGen) write_fn_node_params(node flat.Node) {
	mut params_len := 0
	for i in 0 .. node.children_count {
		if g.a.child_node(&node, i).kind == .param {
			params_len++
		}
	}
	if params_len == 0 {
		g.write('void')
		return
	}
	mut written := 0
	for i in 0 .. node.children_count {
		param_id := g.a.child(&node, i)
		p := g.a.node(param_id)
		if p.kind != .param {
			continue
		}
		pt := g.tc.parse_type(p.typ)
		ct := if pt is types.OptionType || pt is types.ResultType {
			g.optional_type_name(pt)
		} else {
			g.tc.c_type(pt)
		}
		if ct.starts_with('fn_ptr:') {
			g.write(g.resolve_fn_ptr_type(ct))
		} else {
			g.write(ct)
		}
		if p.value.len > 0 {
			g.write(' ')
			param_name := if p.value == '_' { '_${written}' } else { c_name(p.value) }
			g.write(param_name)
		}
		written++
		if written < params_len {
			g.write(', ')
		}
	}
}

fn (mut g FlatGen) write_c_fn_node_params(node flat.Node) {
	if node.children_count == 0 {
		g.write('void')
		return
	}
	mut written := 0
	for i in 0 .. node.children_count {
		param_id := g.a.child(&node, i)
		p := g.a.node(param_id)
		if p.kind != .param {
			continue
		}
		raw_typ := if p.typ.len > 0 { p.typ } else { p.value }
		if raw_typ.len == 0 {
			continue
		}
		pt := g.tc.parse_type(raw_typ)
		ct := if pt is types.OptionType || pt is types.ResultType {
			g.optional_type_name(pt)
		} else {
			g.tc.c_type(pt)
		}
		if written > 0 {
			g.write(', ')
		}
		if ct.starts_with('fn_ptr:') {
			g.write(g.resolve_fn_ptr_type(ct))
		} else {
			g.write(ct)
		}
		if p.typ.len > 0 && p.value.len > 0 {
			g.write(' ')
			param_name := if p.value == '_' { '_${written}' } else { c_name(p.value) }
			g.write(param_name)
		}
		written++
	}
	if written == 0 {
		g.write('void')
	}
}

fn (mut g FlatGen) fn_ptr_typedefs() {
	for encoded, name in g.fn_ptr_types {
		parts := encoded['fn_ptr:'.len..].split('|')
		ret := parts[0]
		params := if parts.len > 1 { parts[1] } else { 'void' }
		g.writeln('typedef ${ret} (*${name})(${params});')
	}
	if g.fn_ptr_types.len > 0 {
		g.writeln('')
	}
}

fn (mut g FlatGen) multi_return_typedefs() {
	mut emitted := map[string]bool{}
	for _, ret in g.tc.fn_ret_types {
		g.emit_multi_return_typedef(ret, mut emitted)
	}
	if emitted.len > 0 {
		g.writeln('')
	}
}

fn (mut g FlatGen) emit_multi_return_typedef(ret types.Type, mut emitted map[string]bool) {
	if ret is types.OptionType {
		g.emit_multi_return_typedef(ret.base_type, mut emitted)
		return
	}
	if ret is types.ResultType {
		g.emit_multi_return_typedef(ret.base_type, mut emitted)
		return
	}
	if ret is types.MultiReturn {
		name := g.tc.c_type(ret)
		if name in emitted {
			return
		}
		emitted[name] = true
		g.writeln('typedef struct {')
		for i, typ in ret.types {
			g.writeln('\t${g.tc.c_type(typ)} arg${i};')
		}
		g.writeln('} ${name};')
	}
}

fn (mut g FlatGen) resolve_fn_ptr_type(typ string) string {
	if typ in g.fn_ptr_types {
		return g.fn_ptr_types[typ]
	}
	name := '_fn_ptr_${g.fn_ptr_types.len}'
	g.fn_ptr_types[typ] = name
	return name
}
