module c

import v3.flat
import v3.types

fn gen_expr_lvalue(mut g FlatGen, id flat.NodeId) {
	node := g.a.nodes[int(id)]
	if node.kind == .index {
		base_id := g.a.child(&node, 0)
		base_type := g.tc.resolve_type(base_id)
		if base_type is types.Map {
			c_key := g.tc.c_type(base_type.key_type)
			c_val := g.tc.c_type(base_type.value_type)
			zero := if base_type.value_type is types.Array {
				c_elem := g.tc.c_type(base_type.value_type.elem_type)
				'&(${c_val}[]){array_new(sizeof(${c_elem}), 0, 0)}'
			} else {
				'&(${c_val}[]){0}'
			}
			g.write('(*(${c_val}*)map__get_or_set(&')
			g.gen_expr(base_id)
			g.write(', &(${c_key}[]){')
			g.gen_expr(g.a.child(&node, 1))
			g.write('}, ${zero}))')
			return
		}
	}
	g.gen_expr(id)
}

fn (mut g FlatGen) gen_node(id flat.NodeId) {
	if int(id) < 0 {
		return
	}
	node := g.a.nodes[int(id)]
	match node.kind {
		.fn_decl, .c_fn_decl {
			return
		}
		.expr_stmt {
			child_id := g.a.child(&node, 0)
			child := g.a.nodes[int(child_id)]
			if g.is_runtime_array_flags_stmt(child_id) {
				return
			}
			if child.kind == .or_expr {
				g.gen_or_expr_stmt(child)
				return
			} else if child.kind == .infix && child.op == .left_shift {
				lhs_id := g.a.child(&child, 0)
				if child.value == 'push_many' {
					g.gen_array_push_many_stmt(lhs_id, g.a.child(&child, 1))
				} else if child.value == 'push' {
					push_rhs_id := g.a.child(&child, 1)
					mut c_elem := if child.typ.len > 0 {
						g.tc.c_type(g.tc.parse_type(child.typ))
					} else {
						'string'
					}
					lhs_arr_type := types.unwrap_pointer(g.tc.resolve_type(lhs_id))
					if lhs_arr := array_like_type(lhs_arr_type) {
						push_rhs_clean := types.unwrap_pointer(g.tc.resolve_type(push_rhs_id))
						if rhs_arr := array_like_type(push_rhs_clean) {
							if lhs_arr.elem_type !is types.Array
								&& g.tc.c_type(lhs_arr.elem_type) == g.tc.c_type(rhs_arr.elem_type) {
								g.gen_array_push_many_stmt(lhs_id, push_rhs_id)
								return
							}
						} else if rhs_fixed := array_fixed_type(push_rhs_clean) {
							if lhs_arr.elem_type !is types.Array
								&& g.tc.c_type(lhs_arr.elem_type) == g.tc.c_type(rhs_fixed.elem_type) {
								g.gen_array_push_many_stmt(lhs_id, push_rhs_id)
								return
							}
						}
						c_elem = g.tc.c_type(lhs_arr.elem_type)
					}
					lhs_is_ptr := g.tc.resolve_type(lhs_id) is types.Pointer
					amp := if lhs_is_ptr { '' } else { '&' }
					g.write('array_push(${amp}')
					gen_expr_lvalue(mut g, lhs_id)
					g.write(', &(${c_elem}[]){')
					g.gen_expr(push_rhs_id)
					g.writeln('});')
				} else {
					lhs_type := g.tc.resolve_type(lhs_id)
					clean := types.unwrap_pointer(lhs_type)
					if lhs_arr := array_like_type(clean) {
						rhs_id := g.a.child(&child, 1)
						rhs_type := g.tc.resolve_type(rhs_id)
						rhs_clean := types.unwrap_pointer(rhs_type)
						if _ := array_like_type(rhs_clean) {
							g.gen_array_push_many_stmt(lhs_id, rhs_id)
						} else if _ := array_fixed_type(rhs_clean) {
							g.gen_array_push_many_stmt(lhs_id, rhs_id)
						} else {
							c_elem := g.tc.c_type(lhs_arr.elem_type)
							lhs_is_ptr := g.tc.resolve_type(lhs_id) is types.Pointer
							amp := if lhs_is_ptr { '' } else { '&' }
							g.write('array_push(${amp}')
							gen_expr_lvalue(mut g, lhs_id)
							g.write(', &(${c_elem}[]){')
							g.gen_expr(rhs_id)
							g.writeln('});')
						}
					} else {
						g.gen_expr(child_id)
						g.writeln(';')
					}
				}
			} else {
				g.gen_expr(child_id)
				g.writeln(';')
			}
		}
		.decl_assign {
			g.gen_decl_assign(node)
		}
		.assign, .selector_assign {
			g.gen_assign(node)
		}
		.index_assign {
			g.gen_index_assign(node)
		}
		.return_stmt {
			if g.cur_fn_ret is types.Enum {
				g.expected_enum = g.cur_fn_ret.name
			}
			g.gen_defers()
			if node.children_count > 0 {
				ret_id := g.a.child(&node, 0)
				ret_node := g.a.nodes[int(ret_id)]
				if ret_node.kind == .call {
					fn_n := g.a.child_node(&ret_node, 0)
					if fn_n.value == 'error' || fn_n.value == 'error_with_code' {
						if g.cur_fn_ret_is_optional {
							ct := g.optional_type_name(g.cur_fn_ret)
							g.writeln('return (${ct}){.ok = false};')
						} else {
							g.write('return ')
							g.gen_expr(ret_id)
							g.writeln(';')
						}
						return
					}
				}
				if g.cur_fn_ret_is_optional {
					ct := g.optional_type_name(g.cur_fn_ret)
					base := g.cur_fn_ret_base
					if ret_node.kind == .none_expr {
						g.writeln('return (${ct}){.ok = false};')
						return
					}
					if base is types.Void {
						g.writeln('return (${ct}){.ok = false};')
					} else {
						raw_expr_type := g.tc.resolve_type(ret_id)
						if g.optional_result_matches_base(raw_expr_type, base) {
							g.write('return ')
							g.gen_expr(ret_id)
							g.writeln(';')
						} else {
							expr_type := g.usable_expr_type(ret_id)
							mut expr_value_type := expr_type
							if expr_type is types.OptionType {
								expr_value_type = expr_type.base_type
							} else if expr_type is types.ResultType {
								expr_value_type = expr_type.base_type
							}
							base_ct := g.tc.c_type(base)
							expr_ct := g.tc.c_type(expr_value_type)
							struct_init_ct := if ret_node.kind == .struct_init {
								g.struct_init_c_type_name(ret_node.value)
							} else {
								''
							}
							if expr_ct != base_ct && struct_init_ct != base_ct
								&& !g.type_names_match(expr_value_type, base)
								&& !g.call_constructs_type(ret_id, base)
								&& expr_value_type !is types.Primitive
								&& expr_value_type !is types.Unknown {
								g.writeln('return (${ct}){.ok = false};')
							} else {
								g.write('return (${ct}){.ok = true, .value = ')
								g.gen_expr(ret_id)
								g.writeln('};')
							}
						}
					}
				} else if g.cur_fn_ret is types.MultiReturn {
					if node.children_count > 1 {
						ct := g.tc.c_type(g.cur_fn_ret)
						g.write('return (${ct}){')
						for i in 0 .. node.children_count {
							if i > 0 {
								g.write(', ')
							}
							g.gen_expr(g.a.child(&node, i))
						}
						g.writeln('};')
					} else {
						expr_type := g.usable_expr_type(ret_id)
						if expr_type is types.MultiReturn {
							g.write('return ')
							g.gen_expr(ret_id)
							g.writeln(';')
						} else {
							ct := g.tc.c_type(g.cur_fn_ret)
							g.write('return (${ct}){')
							g.gen_expr(ret_id)
							g.writeln('};')
						}
					}
				} else if ret_node.kind == .assoc {
					g.gen_return_assoc(ret_node)
				} else {
					if g.cur_fn_ret is types.Interface {
						ct := g.tc.c_type(g.cur_fn_ret)
						g.writeln('return (${ct}){0};')
					} else {
						g.write('return ')
						g.gen_expr(ret_id)
						g.writeln(';')
					}
				}
			} else {
				if g.cur_fn_ret_is_optional {
					ct := g.optional_type_name(g.cur_fn_ret)
					g.writeln('return (${ct}){.ok = true};')
				} else if g.cur_fn_name == 'main' {
					g.writeln('return 0;')
				} else {
					g.writeln('return;')
				}
			}
			g.expected_enum = ''
		}
		.defer_stmt {
			g.defers << g.a.child(&node, 0)
		}
		.for_stmt {
			g.gen_for(node)
		}
		.for_in_stmt {
			g.gen_for_in(node)
		}
		.break_stmt {
			if node.value.len > 0 {
				g.writeln('goto ${c_name(node.value)}_break;')
			} else {
				g.writeln('break;')
			}
		}
		.continue_stmt {
			if node.value.len > 0 {
				g.writeln('goto ${c_name(node.value)}_continue;')
			} else {
				g.writeln('continue;')
			}
		}
		.block {
			g.writeln('{')
			g.tc.push_scope()
			defer_start := g.defers.len
			g.indent++
			for i in 0 .. node.children_count {
				g.gen_node(g.a.child(&node, i))
			}
			g.gen_defers_from(defer_start)
			g.trim_defers(defer_start)
			g.indent--
			g.tc.pop_scope()
			g.writeln('}')
		}
		.if_expr {
			g.gen_if(node)
		}
		.assert_stmt {
			g.write('if (!(')
			g.gen_expr(g.a.child(&node, 0))
			g.writeln(')) {')
			g.indent++
			g.writeln('fprintf(stderr, "assert failed\\n");')
			g.writeln('exit(1);')
			g.indent--
			g.writeln('}')
		}
		.goto_stmt {
			g.writeln('goto ${c_name(node.value)};')
		}
		.label_stmt {
			old_indent := g.indent
			g.indent = 0
			g.writeln('${c_name(node.value)}: ;')
			g.indent = old_indent
		}
		.empty, .asm_stmt {}
		else {
			// NOTE: match_stmt is intentionally absent — the transformer lowers every
			// match into an if/else-if chain (see transform.lower_match_stmts), so the
			// backend never sees one. Match lowering lives in the transformer, not here.
			eprintln('gen_node: unsupported node kind: ${node.kind}')
		}
	}
}

fn (g &FlatGen) expr_really_returns_optional(id flat.NodeId) bool {
	if int(id) < 0 {
		return false
	}
	node := g.a.nodes[int(id)]
	if node.kind == .none_expr {
		return true
	}
	if node.kind == .call {
		if fname := g.tc.resolved_call_name(id) {
			ret_type := g.tc.fn_ret_types[fname] or { return false }
			return ret_type is types.OptionType || ret_type is types.ResultType
		}
	}
	return false
}

fn (g &FlatGen) optional_result_matches_base(expr_type types.Type, base types.Type) bool {
	if expr_type is types.OptionType {
		return g.type_names_match(expr_type.base_type, base)
	}
	if expr_type is types.ResultType {
		return g.type_names_match(expr_type.base_type, base)
	}
	return false
}

fn (g &FlatGen) usable_expr_type(id flat.NodeId) types.Type {
	if typ := g.tc.expr_type(id) {
		if typ !is types.Unknown && typ !is types.Void {
			return typ
		}
	}
	return g.tc.resolve_type(id)
}

fn (g &FlatGen) type_names_match(a types.Type, b types.Type) bool {
	a_name := a.name()
	b_name := b.name()
	if a_name.len == 0 || b_name.len == 0 {
		return false
	}
	if a_name == b_name {
		return true
	}
	return a_name.all_after_last('.') == b_name.all_after_last('.')
}

fn (g &FlatGen) call_constructs_type(id flat.NodeId, target types.Type) bool {
	if int(id) < 0 {
		return false
	}
	node := g.a.nodes[int(id)]
	if node.kind != .call || node.children_count == 0 {
		return false
	}
	fn_node := g.a.child_node(&node, 0)
	if fn_node.kind != .ident {
		return false
	}
	target_name := target.name()
	if target_name.len == 0 {
		return false
	}
	short_target := target_name.all_after_last('.')
	return fn_node.value == target_name || fn_node.value == short_target
}

fn (g &FlatGen) is_runtime_array_flags_stmt(id flat.NodeId) bool {
	if int(id) < 0 {
		return false
	}
	node := g.a.nodes[int(id)]
	if node.kind != .call || node.children_count == 0 {
		return false
	}
	fn_node := g.a.child_node(&node, 0)
	if fn_node.kind != .selector || fn_node.value !in ['set', 'clear']
		|| fn_node.children_count == 0 {
		return false
	}
	flags_node := g.a.child_node(fn_node, 0)
	if flags_node.kind != .selector || flags_node.value != 'flags' || flags_node.children_count == 0 {
		return false
	}
	owner_id := g.a.child(flags_node, 0)
	owner_type := types.unwrap_pointer(g.tc.resolve_type(owner_id))
	return owner_type is types.Array || owner_type.name() == 'strings.Builder'
}

fn (mut g FlatGen) gen_decl_assign(node flat.Node) {
	if node.children_count >= 3 {
		rhs_type := g.tc.resolve_type(g.a.child(&node, 1))
		if rhs_type is types.MultiReturn {
			g.gen_multi_return_decl(node)
			return
		}
	}
	mut bad_decl_child := node.children_count % 2 == 1
	for i in 0 .. node.children_count {
		if int(g.a.child(&node, i)) < 0 {
			bad_decl_child = true
		}
	}
	if bad_decl_child {
		mut parts := []string{}
		for i in 0 .. node.children_count {
			child_id := g.a.child(&node, i)
			if int(child_id) < 0 {
				parts << '${i}:empty'
			} else {
				child := g.a.nodes[int(child_id)]
				parts << '${i}:${child.kind}:${child.value}:${child.typ}'
			}
		}
		panic('internal error: odd decl_assign in ${g.cur_fn_name}: count=${node.children_count} typ=${node.typ} value=${node.value} children=${parts.join('|')}')
	}
	decl_prefix := if node.value == 'static' { 'static ' } else { '' }
	mut i := 0
	for i < node.children_count {
		lhs_id := g.a.child(&node, i)
		rhs_id := g.a.child(&node, i + 1)
		lhs := g.a.nodes[int(lhs_id)]
		rhs := g.a.nodes[int(rhs_id)]
		if rhs.kind == .array_literal {
			elem_type := if rhs.children_count > 0 {
				g.tc.resolve_type(g.a.child(&rhs, 0))
			} else {
				types.Type(types.int_)
			}
			g.write('${decl_prefix}Array ')
			g.gen_decl_lhs(lhs_id)
			g.write(' = ')
			g.gen_array_literal_value(rhs, elem_type)
			g.writeln(';')
			if lhs.kind == .ident {
				g.tc.cur_scope.insert(lhs.value, types.Type(types.Array{
					elem_type: elem_type
				}))
			}
		} else if rhs.kind == .or_expr {
			g.gen_decl_or_expr(lhs, rhs)
		} else if rhs.kind == .array_init {
			raw_init_type := g.tc.parse_type(rhs.value)
			init_type := raw_init_type
			if init_type is types.ArrayFixed {
				c_elem := g.tc.c_type(init_type.elem_type)
				lhs_str := g.decl_lhs_str(lhs_id)
				len_expr := g.fixed_array_len_expr(rhs.value, init_type.len)
				if len_expr == '0' {
					g.writeln('${decl_prefix}${c_elem} ${lhs_str}[0];')
				} else {
					g.writeln('${decl_prefix}${c_elem} ${lhs_str}[${len_expr}] = {0};')
				}
				if lhs.kind == .ident {
					g.tc.cur_scope.insert(lhs.value, raw_init_type)
				}
			} else {
				c_elem := g.tc.c_type(init_type)
				mut init_len := '0'
				mut init_cap := '0'
				mut init_val := ''
				for j in 0 .. rhs.children_count {
					child := g.a.child_node(&rhs, j)
					if child.kind == .field_init {
						if child.value == 'len' {
							init_len = g.expr_to_string(g.a.child(child, 0))
						} else if child.value == 'cap' {
							init_cap = g.expr_to_string(g.a.child(child, 0))
						} else if child.value == 'init' {
							init_val = g.expr_to_string(g.a.child(child, 0))
						}
					}
				}
				lhs_str := g.decl_lhs_str(lhs_id)
				g.writeln('${decl_prefix}Array ${lhs_str} = array_new(sizeof(${c_elem}), ${init_len}, ${init_cap});')
				if init_val.len > 0 {
					g.writeln('for (int _ai = 0; _ai < ${lhs_str}.len; _ai++) ((${c_elem}*)${lhs_str}.data)[_ai] = ${init_val};')
				}
				if lhs.kind == .ident {
					g.tc.cur_scope.insert(lhs.value, types.Type(types.Array{
						elem_type: init_type
					}))
				}
			}
		} else if rhs.kind == .map_init {
			v_type := g.tc.resolve_type(rhs_id)
			c_typ := g.tc.c_type(v_type)
			g.write('${decl_prefix}${c_typ} ')
			g.gen_decl_lhs(lhs_id)
			g.write(' = ')
			g.gen_expr(rhs_id)
			g.writeln(';')
			if lhs.kind == .ident {
				g.tc.cur_scope.insert(lhs.value, v_type)
			}
			if rhs.children_count > 0 {
				if v_type is types.Map {
					c_key := g.tc.c_type(v_type.key_type)
					c_val := g.tc.c_type(v_type.value_type)
					for j := 0; j < rhs.children_count; j += 2 {
						g.write('map__set(&')
						g.gen_decl_lhs(lhs_id)
						g.write(', &(${c_key}[]){')
						g.gen_expr(g.a.child(&rhs, j))
						g.write('}, &(${c_val}[]){')
						g.gen_expr(g.a.child(&rhs, j + 1))
						g.writeln('});')
					}
				}
			}
		} else {
			v_type := if node.typ.len > 0 {
				g.tc.parse_type(node.typ)
			} else if rhs.kind == .if_expr {
				g.if_expr_type(&rhs)
			} else {
				g.tc.resolve_type(rhs_id)
			}
			ct0 := g.tc.c_type(v_type)
			ct := if v_type is types.OptionType || v_type is types.ResultType {
				g.optional_type_name(v_type)
			} else {
				ct0
			}
			if ct.starts_with('fn_ptr:') {
				fp_name := g.resolve_fn_ptr_type(ct)
				g.write('${decl_prefix}${fp_name} ')
			} else {
				g.write('${decl_prefix}${ct} ')
			}
			g.gen_decl_lhs(lhs_id)
			g.write(' = ')
			g.gen_expr(rhs_id)
			g.writeln(';')
			if lhs.kind == .ident {
				g.tc.cur_scope.insert(lhs.value, v_type)
			}
		}
		i += 2
	}
}

fn (mut g FlatGen) gen_multi_return_decl(node flat.Node) {
	rhs_id := g.a.child(&node, 1)
	rhs_type := g.tc.resolve_type(rhs_id)
	ct := g.tc.c_type(rhs_type)
	tmp := g.tmp_name()
	g.write('${ct} ${tmp} = ')
	g.gen_expr(rhs_id)
	g.writeln(';')
	num_lhs := node.children_count - 1
	mut multi_types := []types.Type{}
	if rhs_type is types.MultiReturn {
		multi_types = rhs_type.types.clone()
	}
	for j in 0 .. num_lhs {
		lhs_idx := if j == 0 { 0 } else { j + 1 }
		lhs_id := g.a.child(&node, lhs_idx)
		lhs := g.a.nodes[int(lhs_id)]
		if lhs.kind == .ident && lhs.value == '_' {
			continue
		}
		field_type := if j < multi_types.len {
			g.tc.c_type(multi_types[j])
		} else {
			'int'
		}
		lhs_name := c_name(lhs.value)
		g.writeln('${field_type} ${lhs_name} = ${tmp}.arg${j};')
		if lhs.kind == .ident {
			inner := if j < multi_types.len {
				multi_types[j]
			} else {
				types.Type(types.int_)
			}
			g.tc.cur_scope.insert(lhs.value, inner)
		}
	}
}

fn (mut g FlatGen) gen_assign(node flat.Node) {
	if node.children_count >= 3 {
		rhs_id := g.a.child(&node, 1)
		rhs_type := g.tc.resolve_type(rhs_id)
		if rhs_type is types.MultiReturn {
			g.gen_multi_return_assign(node)
			return
		}
	}
	mut i := 0
	for i < node.children_count {
		lhs := g.a.nodes[int(g.a.child(&node, i))]
		if lhs.kind == .ident && lhs.value == '_' {
			g.write('(void)(')
			g.gen_expr(g.a.child(&node, i + 1))
			g.writeln(');')
		} else if node.op == .left_shift_assign && lhs.kind == .ident {
			if node.value == 'push_many' {
				g.gen_array_push_many_stmt(g.a.child(&node, i), g.a.child(&node, i + 1))
			} else if node.value == 'push' {
				lhs_id := g.a.child(&node, i)
				rhs_id := g.a.child(&node, i + 1)
				lhs_arr_type := types.unwrap_pointer(g.tc.resolve_type(lhs_id))
				if lhs_arr := array_like_type(lhs_arr_type) {
					push_rhs_clean := types.unwrap_pointer(g.tc.resolve_type(rhs_id))
					if rhs_arr := array_like_type(push_rhs_clean) {
						if lhs_arr.elem_type !is types.Array
							&& g.tc.c_type(lhs_arr.elem_type) == g.tc.c_type(rhs_arr.elem_type) {
							g.gen_array_push_many_stmt(lhs_id, rhs_id)
							i += 2
							continue
						}
					} else if rhs_fixed := array_fixed_type(push_rhs_clean) {
						if lhs_arr.elem_type !is types.Array
							&& g.tc.c_type(lhs_arr.elem_type) == g.tc.c_type(rhs_fixed.elem_type) {
							g.gen_array_push_many_stmt(lhs_id, rhs_id)
							i += 2
							continue
						}
					}
				}
				lhs_is_ptr := g.tc.resolve_type(g.a.child(&node, i)) is types.Pointer
				amp := if lhs_is_ptr { '' } else { '&' }
				c_elem := g.tc.c_type(g.tc.parse_type(node.typ))
				g.write('array_push(${amp}${c_name(lhs.value)}, &(${c_elem}[]){')
				g.gen_expr(g.a.child(&node, i + 1))
				g.writeln('});')
			} else {
				// Array appends are annotated by the transformer; an un-annotated
				// `<<=` here is the integer bit-shift-assign operator.
				g.gen_expr(g.a.child(&node, i))
				g.write(' <<= ')
				g.gen_expr(g.a.child(&node, i + 1))
				g.writeln(';')
			}
		} else {
			rhs_id := g.a.child(&node, i + 1)
			rhs_node := g.a.nodes[int(rhs_id)]
			if rhs_node.kind == .or_expr {
				g.gen_assign_or_expr(node, i, rhs_node)
				i += 2
				continue
			}
			if rhs_node.kind == .array_literal && rhs_node.children_count > 0 {
				elem_type := g.tc.resolve_type(g.a.child(&rhs_node, 0))
				g.gen_expr(g.a.child(&node, i))
				g.write(' = ')
				g.gen_array_literal_value(rhs_node, elem_type)
				g.writeln(';')
			} else {
				lhs_type := g.tc.resolve_type(g.a.child(&node, i))
				rhs_type := g.tc.resolve_type(rhs_id)
				if node.op == .plus_assign && (lhs_type is types.String || rhs_type is types.String) {
					g.gen_expr(g.a.child(&node, i))
					g.write(' = string__plus(')
					g.gen_expr(g.a.child(&node, i))
					g.write(', ')
					g.gen_expr(rhs_id)
					g.writeln(');')
					i += 2
					continue
				}
				if lhs_type is types.Enum {
					g.expected_enum = lhs_type.name
				}
				if g.assign_lhs_needs_deref(g.a.child(&node, i), lhs_type, rhs_type, node.op) {
					g.write('*')
				}
				g.gen_expr(g.a.child(&node, i))
				g.write(' ${g.op_str(node.op)} ')
				g.gen_expr(rhs_id)
				g.writeln(';')
				g.expected_enum = ''
			}
		}
		i += 2
	}
}

fn (g &FlatGen) assign_lhs_needs_deref(lhs_id flat.NodeId, lhs_type types.Type, rhs_type types.Type, op flat.Op) bool {
	if op != .assign {
		return false
	}
	lhs := g.a.nodes[int(lhs_id)]
	if lhs.kind != .ident {
		return false
	}
	if lhs_type is types.Pointer {
		return lhs_type.base_type.name() == rhs_type.name()
	}
	return false
}

fn (mut g FlatGen) gen_multi_return_assign(node flat.Node) {
	rhs_id := g.a.child(&node, 1)
	rhs_type := g.tc.resolve_type(rhs_id)
	ct := g.tc.c_type(rhs_type)
	tmp := g.tmp_name()
	g.write('${ct} ${tmp} = ')
	g.gen_expr(rhs_id)
	g.writeln(';')
	num_lhs := node.children_count - 1
	for j in 0 .. num_lhs {
		lhs_idx := if j == 0 { 0 } else { j + 1 }
		lhs_id := g.a.child(&node, lhs_idx)
		lhs := g.a.nodes[int(lhs_id)]
		if lhs.kind == .ident && lhs.value == '_' {
			continue
		}
		gen_expr_lvalue(mut g, lhs_id)
		g.writeln(' = ${tmp}.arg${j};')
	}
}

fn (mut g FlatGen) gen_decl_lhs(id flat.NodeId) {
	node := g.a.nodes[int(id)]
	if node.kind == .ident {
		g.write(c_name(node.value))
	} else {
		g.gen_expr(id)
	}
}

fn (mut g FlatGen) decl_lhs_str(id flat.NodeId) string {
	node := g.a.nodes[int(id)]
	if node.kind == .ident {
		return c_name(node.value)
	}
	return g.expr_to_string(id)
}

fn (mut g FlatGen) gen_assign_or_expr(node flat.Node, lhs_idx int, or_node flat.Node) {
	expr_id := g.a.child(&or_node, 0)
	or_body_id := g.a.child(&or_node, 1)
	or_body := g.a.nodes[int(or_body_id)]
	tmp := g.tmp_name()
	expr_type := g.tc.resolve_type(expr_id)
	opt_ct := g.optional_type_name(expr_type)
	g.write('${opt_ct} ${tmp} = ')
	g.gen_expr(expr_id)
	g.writeln(';')
	g.writeln('if (${tmp}.ok) {')
	g.indent++
	g.gen_expr(g.a.child(&node, lhs_idx))
	g.writeln(' = ${tmp}.value;')
	g.indent--
	g.writeln('} else {')
	g.tc.push_scope()
	g.tc.cur_scope.insert('err', types.Type(types.Struct{
		name: 'IError'
	}))
	g.indent++
	g.writeln('IError err = (IError){0};')
	if or_node.value == '!' || or_node.value == '?' {
		if g.cur_fn_ret_is_optional {
			fn_opt_ct := g.optional_type_name(g.cur_fn_ret)
			g.writeln('return (${fn_opt_ct}){.ok = false};')
		} else {
			g.writeln('v_panic(err.message);')
		}
	} else {
		for j in 0 .. or_body.children_count {
			child_id := g.a.child(&or_body, j)
			g.gen_node(child_id)
		}
	}
	g.indent--
	g.tc.pop_scope()
	g.writeln('}')
}

fn (mut g FlatGen) gen_decl_or_expr(lhs flat.Node, or_node flat.Node) {
	expr_id := g.a.child(&or_node, 0)
	or_body_id := g.a.child(&or_node, 1)
	or_body := g.a.nodes[int(or_body_id)]
	expr_node := g.a.nodes[int(expr_id)]
	if expr_node.kind == .index {
		base_type := g.tc.resolve_type(g.a.child(&expr_node, 0))
		clean := types.unwrap_pointer(base_type)
		if clean is types.Map {
			g.gen_decl_or_map_index(lhs, expr_node, clean, or_body)
			return
		}
	}
	tmp := g.tmp_name()
	expr_type := g.tc.resolve_type(expr_id)
	opt_ct := g.optional_type_name(expr_type)
	val_ct, val_type := g.optional_value_ct(expr_type)
	g.tc.cur_scope.insert(lhs.value, val_type)
	g.write('${opt_ct} ${tmp} = ')
	g.gen_expr(expr_id)
	g.writeln(';')
	g.writeln('${val_ct} ${c_name(lhs.value)};')
	g.writeln('if (${tmp}.ok) {')
	g.indent++
	g.writeln('${c_name(lhs.value)} = ${tmp}.value;')
	g.indent--
	g.writeln('} else {')
	g.tc.push_scope()
	g.tc.cur_scope.insert('err', types.Type(types.Struct{
		name: 'IError'
	}))
	g.indent++
	g.writeln('IError err = (IError){0};')
	if or_node.value == '!' || or_node.value == '?' {
		if g.cur_fn_ret_is_optional {
			fn_opt_ct := g.optional_type_name(g.cur_fn_ret)
			g.writeln('return (${fn_opt_ct}){.ok = false};')
		} else {
			g.writeln('v_panic(err.message);')
		}
	} else if or_body.children_count > 0 {
		for i in 0 .. or_body.children_count {
			child_id := g.a.child(&or_body, i)
			child := g.a.nodes[int(child_id)]
			if i == or_body.children_count - 1 && child.kind == .expr_stmt {
				inner := g.a.child_node(&child, 0)
				if inner.kind == .call && g.is_noreturn_call(inner) {
					g.gen_node(child_id)
				} else {
					g.write('${c_name(lhs.value)} = ')
					g.gen_expr(g.a.child(&child, 0))
					g.writeln(';')
				}
			} else {
				g.gen_node(child_id)
			}
		}
	}
	g.indent--
	g.tc.pop_scope()
	g.writeln('}')
}

fn (mut g FlatGen) gen_decl_or_map_index(lhs flat.Node, expr_node flat.Node, m types.Map, or_body flat.Node) {
	tmp := g.tmp_name()
	c_val := g.tc.c_type(m.value_type)
	c_key := g.tc.c_type(m.key_type)
	g.tc.cur_scope.insert(lhs.value, m.value_type)
	g.write('void* ${tmp} = map__get_check(&')
	g.gen_expr(g.a.child(&expr_node, 0))
	g.write(', &(${c_key}[]){')
	g.gen_expr(g.a.child(&expr_node, 1))
	g.writeln('});')
	g.writeln('${c_val} ${c_name(lhs.value)};')
	g.writeln('if (${tmp}) {')
	g.indent++
	g.writeln('${c_name(lhs.value)} = *(${c_val}*)${tmp};')
	g.indent--
	g.writeln('} else {')
	g.indent++
	if or_body.children_count > 0 {
		for i in 0 .. or_body.children_count {
			child_id := g.a.child(&or_body, i)
			child := g.a.nodes[int(child_id)]
			if i == or_body.children_count - 1 && child.kind == .expr_stmt {
				inner := g.a.child_node(&child, 0)
				if inner.kind == .call && g.is_noreturn_call(inner) {
					g.gen_node(child_id)
				} else {
					g.write('${c_name(lhs.value)} = ')
					g.gen_expr(g.a.child(&child, 0))
					g.writeln(';')
				}
			} else {
				g.gen_node(child_id)
			}
		}
	}
	g.indent--
	g.writeln('}')
}

fn (g &FlatGen) is_noreturn_call(node &flat.Node) bool {
	if node.kind != .call || node.children_count == 0 {
		return false
	}
	fn_node := g.a.child_node(node, 0)
	return fn_node.value in ['panic', 'exit']
}

fn (mut g FlatGen) tmp_name() string {
	g.tmp_count++
	return '_t${g.tmp_count}'
}

fn (mut g FlatGen) gen_or_expr(node flat.Node) {
	expr_id := g.a.child(&node, 0)
	or_body_id := g.a.child(&node, 1)
	or_body := g.a.nodes[int(or_body_id)]
	expr_node := g.a.nodes[int(expr_id)]
	if expr_node.kind == .index {
		base_type := g.tc.resolve_type(g.a.child(&expr_node, 0))
		clean := types.unwrap_pointer(base_type)
		if clean is types.Map {
			g.gen_or_map_index(expr_node, clean, or_body)
			return
		}
	}
	tmp := g.tmp_name()
	expr_type := g.tc.resolve_type(expr_id)
	opt_ct := g.optional_type_name(expr_type)
	g.write('({${opt_ct} ${tmp} = ')
	g.gen_expr(expr_id)
	g.write('; ${tmp}.ok ? ${tmp}.value : ({IError err = (IError){0}; (void)err; ')
	g.gen_or_body(or_body)
	g.write(';});})')
}

fn (mut g FlatGen) gen_or_body(or_body flat.Node) {
	if or_body.children_count == 1 {
		last_id := g.a.child(&or_body, or_body.children_count - 1)
		last := g.a.nodes[int(last_id)]
		if last.kind == .expr_stmt {
			g.gen_expr(g.a.child(&last, 0))
		} else {
			g.gen_expr(last_id)
		}
	} else {
		g.write('({')
		for i in 0 .. or_body.children_count {
			child_id := g.a.child(&or_body, i)
			child := g.a.nodes[int(child_id)]
			if i == or_body.children_count - 1 && child.kind == .expr_stmt {
				g.gen_expr(g.a.child(&child, 0))
				g.write(';')
			} else {
				g.gen_node(child_id)
			}
		}
		g.write('})')
	}
}

fn (mut g FlatGen) gen_or_map_index(expr_node flat.Node, m types.Map, or_body flat.Node) {
	tmp := g.tmp_name()
	c_val := g.tc.c_type(m.value_type)
	c_key := g.tc.c_type(m.key_type)
	g.write('({void* ${tmp} = map__get_check(&')
	g.gen_expr(g.a.child(&expr_node, 0))
	g.write(', &(${c_key}[]){')
	g.gen_expr(g.a.child(&expr_node, 1))
	g.write('}); ${tmp} ? *(${c_val}*)${tmp} : ')
	g.gen_or_body(or_body)
	g.write(';})')
}

fn (mut g FlatGen) gen_or_expr_stmt(node flat.Node) {
	expr_id := g.a.child(&node, 0)
	or_body_id := g.a.child(&node, 1)
	or_body := g.a.nodes[int(or_body_id)]
	tmp := g.tmp_name()
	expr_type := g.tc.resolve_type(expr_id)
	opt_ct := g.optional_type_name(expr_type)
	g.writeln('${opt_ct} ${tmp} = ')
	g.gen_expr(expr_id)
	g.writeln(';')
	g.writeln('if (!${tmp}.ok) {')
	g.tc.push_scope()
	g.tc.cur_scope.insert('err', types.Type(types.Struct{
		name: 'IError'
	}))
	g.indent++
	g.writeln('IError err = (IError){0};')
	if node.value == '!' || node.value == '?' {
		if g.cur_fn_ret_is_optional {
			fn_opt_ct := g.optional_type_name(g.cur_fn_ret)
			g.writeln('return (${fn_opt_ct}){.ok = false};')
		} else {
			g.writeln('v_panic(err.message);')
		}
	} else {
		for i in 0 .. or_body.children_count {
			g.gen_node(g.a.child(&or_body, i))
		}
	}
	g.indent--
	g.tc.pop_scope()
	g.writeln('}')
}
