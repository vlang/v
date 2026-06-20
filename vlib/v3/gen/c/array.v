module c

import v3.flat
import v3.types

fn array_like_type(t types.Type) ?types.Array {
	if t is types.Array {
		return t
	}
	if t is types.Alias {
		base := t.base_type
		if base is types.Array {
			return base
		}
	}
	return none
}

fn array_fixed_type(t types.Type) ?types.ArrayFixed {
	if t is types.ArrayFixed {
		return t
	}
	if t is types.Alias {
		base := t.base_type
		if base is types.ArrayFixed {
			return base
		}
	}
	return none
}

fn (mut g FlatGen) gen_array_literal_value(node flat.Node, elem_type types.Type) {
	c_elem := g.tc.c_type(elem_type)
	count := node.children_count
	g.write('new_array_from_c_array(${count}, ${count}, sizeof(${c_elem}), (${c_elem}[]){')
	for i in 0 .. count {
		if i > 0 {
			g.write(', ')
		}
		g.gen_expr(g.a.child(&node, i))
	}
	g.write('})')
}

fn (mut g FlatGen) gen_fixed_array_data_arg(id flat.NodeId, arr types.ArrayFixed) {
	node := g.a.nodes[int(id)]
	if node.kind == .array_literal {
		c_elem := g.tc.c_type(arr.elem_type)
		g.write('(${c_elem}[]){')
		for i in 0 .. node.children_count {
			if i > 0 {
				g.write(', ')
			}
			g.gen_expr(g.a.child(&node, i))
		}
		g.write('}')
		return
	}
	g.gen_expr(id)
}

fn (mut g FlatGen) gen_array_push_many_stmt(lhs_id flat.NodeId, rhs_id flat.NodeId) {
	lhs_is_ptr := g.tc.resolve_type(lhs_id) is types.Pointer
	amp := if lhs_is_ptr { '' } else { '&' }
	rhs_type := types.unwrap_pointer(g.tc.resolve_type(rhs_id))
	if rhs_fixed := array_fixed_type(rhs_type) {
		g.write('array_push_many_ptr(${amp}')
		g.gen_expr_lvalue(lhs_id)
		g.write(', ')
		g.gen_fixed_array_data_arg(rhs_id, rhs_fixed)
		len_expr := g.fixed_array_len_value(rhs_fixed)
		g.writeln(', ${len_expr});')
		return
	}
	g.write('array_push_many(${amp}')
	g.gen_expr_lvalue(lhs_id)
	g.write(', ')
	g.gen_expr(rhs_id)
	g.writeln(');')
}

fn (mut g FlatGen) gen_slice_expr(node flat.Node, base_id flat.NodeId, base_type types.Type) {
	start_node := g.a.child_node(&node, 1)
	has_start := start_node.kind != .empty
	has_end := node.children_count > 2
	base_str := g.expr_to_string(base_id)
	start_str := if has_start { g.expr_to_string(g.a.child(&node, 1)) } else { '0' }
	end_str := if has_end {
		g.expr_to_string(g.a.child(&node, 2))
	} else {
		'${base_str}.len'
	}
	if base_type is types.String {
		g.write('string__substr(${base_str}, ${start_str}, ${end_str})')
	} else if base_type is types.Array {
		g.write('array_slice(${base_str}, ${start_str}, ${end_str})')
	} else {
		g.write('string__substr(${base_str}, ${start_str}, ${end_str})')
	}
}

fn (mut g FlatGen) gen_array_method_call(node flat.Node, fn_node &flat.Node, arr types.Array) {
	c_elem := g.tc.c_type(arr.elem_type)
	base_id := g.a.child(fn_node, 0)
	base_node := g.a.nodes[int(base_id)]
	is_ptr := if base_node.kind == .ident {
		g.tc.resolve_type(base_id) is types.Pointer
	} else {
		false
	}
	dot := if is_ptr { '->' } else { '.' }
	match fn_node.value {
		'clone' {
			g.write('array_clone(')
			g.gen_expr(base_id)
			g.write(')')
		}
		'last' {
			g.write('*(${c_elem}*)array_get(')
			g.gen_expr(base_id)
			g.write(', ')
			g.gen_expr(base_id)
			g.write('.len - 1)')
		}
		'first' {
			g.write('*(${c_elem}*)array_get(')
			g.gen_expr(base_id)
			g.write(', 0)')
		}
		'delete_last' {
			g.gen_expr(base_id)
			g.write('${dot}len--')
		}
		'pop' {
			g.write('({ ${c_elem} _pop${g.tmp_count} = *(${c_elem}*)array_get(')
			g.gen_expr(base_id)
			g.write(', ')
			g.gen_expr(base_id)
			g.write('${dot}len - 1); ')
			g.gen_expr(base_id)
			g.write('${dot}len--; _pop${g.tmp_count}; })')
			g.tmp_count++
		}
		'clear' {
			g.gen_expr(base_id)
			g.write('${dot}len = 0')
		}
		'push_many' {
			amp := if is_ptr { '' } else { '&' }
			g.write('array_push_many_ptr(${amp}')
			g.gen_expr(base_id)
			g.write(', ')
			g.gen_expr(g.a.child(&node, 1))
			g.write(', ')
			g.gen_expr(g.a.child(&node, 2))
			g.write(')')
		}
		'repeat' {
			g.write('array__repeat_to_depth(')
			g.gen_expr(base_id)
			g.write(', ')
			g.gen_expr(g.a.child(&node, 1))
			g.write(', 0)')
		}
		'repeat_to_depth' {
			g.write('array__repeat_to_depth(')
			g.gen_expr(base_id)
			g.write(', ')
			g.gen_expr(g.a.child(&node, 1))
			g.write(', ')
			g.gen_expr(g.a.child(&node, 2))
			g.write(')')
		}
		'trim' {
			g.gen_expr(base_id)
			g.write('${dot}len = ')
			g.gen_expr(g.a.child(&node, 1))
		}
		'ensure_cap' {
			amp := if is_ptr { '' } else { '&' }
			g.write('array_ensure_cap(${amp}')
			g.gen_expr(base_id)
			g.write(', ')
			g.gen_expr(g.a.child(&node, 1))
			g.write(')')
		}
		'delete' {
			amp := if is_ptr { '' } else { '&' }
			g.write('array_delete(${amp}')
			g.gen_expr(base_id)
			g.write(', ')
			g.gen_expr(g.a.child(&node, 1))
			g.write(')')
		}
		'free' {
			g.write('array__free(')
			if !is_ptr {
				g.write('&')
			}
			g.gen_expr(base_id)
			g.write(')')
		}
		'str' {
			amp := if is_ptr { '' } else { '&' }
			g.write('strings__Builder__str(${amp}')
			g.gen_expr(base_id)
			g.write(')')
		}
		'join' {
			g.write('array_string_join(')
			g.gen_expr(base_id)
			g.write(', ')
			g.gen_expr(g.a.child(&node, 1))
			g.write(')')
		}
		'bytestr' {
			g.write('u8__vstring_with_len((u8*)')
			g.gen_expr(base_id)
			g.write('${dot}data, ')
			g.gen_expr(base_id)
			g.write('${dot}len)')
		}
		'contains' {
			contains_fn := 'array_contains_${array_lookup_suffix(arr.elem_type)}'
			g.write('${contains_fn}(')
			g.gen_expr(base_id)
			g.write(', ')
			g.gen_expr(g.a.child(&node, 1))
			g.write(')')
		}
		'index' {
			index_fn := 'array_index_${array_lookup_suffix(arr.elem_type)}'
			g.write('${index_fn}(')
			g.gen_expr(base_id)
			g.write(', ')
			g.gen_expr(g.a.child(&node, 1))
			g.write(')')
		}
		else {
			best_mname := g.array_method_fallback(fn_node.value)
			if best_mname.len > 0 {
				g.write(c_name(best_mname))
				g.write('(')
				ptypes := g.tc.fn_param_types[best_mname]
				wants_ptr := ptypes.len > 0 && ptypes[0] is types.Pointer
				if wants_ptr && !is_ptr {
					g.write('&')
				} else if !wants_ptr && is_ptr {
					g.write('*')
				}
				g.gen_expr(base_id)
				for i in 1 .. node.children_count {
					g.write(', ')
					g.gen_expr(g.a.child(&node, i))
				}
				g.write(')')
			} else {
				g.gen_expr(g.a.child(&node, 0))
				g.write('(')
				g.gen_expr(base_id)
				g.write(')')
			}
		}
	}
}

fn array_lookup_suffix(elem_type types.Type) string {
	if elem_type is types.String {
		return 'string'
	}
	if elem_type is types.Primitive {
		if elem_type.props.has(.unsigned) && elem_type.size == 8 {
			return 'u8'
		}
	}
	return 'int'
}

fn (mut g FlatGen) array_method_fallback(method string) string {
	if method in g.array_method_cache {
		return g.array_method_cache[method]
	}
	suffix := '.${method}'
	mut best_mname := ''
	for mname, _ in g.tc.fn_param_types {
		if mname.ends_with(suffix) {
			if best_mname.len == 0 || mname.len > best_mname.len {
				best_mname = mname
			}
		}
	}
	g.array_method_cache[method] = best_mname
	return best_mname
}

fn (mut g FlatGen) gen_map_delete(node flat.Node, fn_node &flat.Node, m types.Map) {
	c_key := g.tc.c_type(m.key_type)
	g.write('map__delete(&')
	g.gen_expr(g.a.child(fn_node, 0))
	g.write(', &(${c_key}[]){')
	g.gen_expr(g.a.child(&node, 1))
	g.write('})')
}

fn (mut g FlatGen) gen_index_assign(node flat.Node) {
	lhs_id := g.a.child(&node, 0)
	lhs := g.a.nodes[int(lhs_id)]
	if lhs.kind == .index {
		base_id := g.a.child(&lhs, 0)
		base_type := g.tc.resolve_type(base_id)
		clean_base := types.unwrap_pointer(base_type)
		if clean_base is types.Map {
			c_key := g.tc.c_type(clean_base.key_type)
			c_val := g.tc.c_type(clean_base.value_type)
			is_ptr := base_type is types.Pointer
			if is_ptr {
				g.write('map__set(')
			} else {
				g.write('map__set(&')
			}
			g.gen_expr(base_id)
			g.write(', &(${c_key}[]){')
			g.gen_expr(g.a.child(&lhs, 1))
			g.write('}, &(${c_val}[]){')
			g.gen_expr(g.a.child(&node, 1))
			g.writeln('});')
			return
		}
		if base_type is types.Pointer {
			ptr_type := base_type
			if ptr_type.base_type is types.Void {
				g.write('((u8*)')
				g.gen_expr(base_id)
				g.write(')[')
				g.gen_expr(g.a.child(&lhs, 1))
				g.write('] = ')
				g.gen_expr(g.a.child(&node, 1))
				g.writeln(';')
				return
			}
		}
		mut arr_type := types.Array{}
		mut is_array_base := false
		if base_type is types.Array {
			arr_type = base_type
			is_array_base = true
		} else if base_type is types.Pointer {
			ptr_type := base_type
			ptr_base := ptr_type.base_type
			if ptr_base is types.Array {
				arr_type = ptr_base
				is_array_base = true
			}
		}
		if is_array_base {
			c_elem := g.tc.c_type(arr_type.elem_type)
			g.write('array_set(')
			if base_type is types.Pointer {
				g.write('*')
			}
			g.gen_expr(base_id)
			g.write(', ')
			g.gen_expr(g.a.child(&lhs, 1))
			g.write(', &(${c_elem}[]){')
			g.gen_expr(g.a.child(&node, 1))
			g.writeln('});')
			return
		}
	}
	g.gen_assign(node)
}
