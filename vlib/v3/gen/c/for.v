module c

import v3.flat
import v3.types

// gen_for emits for output for c.
fn (mut g FlatGen) gen_for(node flat.Node) {
	g.tc.push_scope()
	defer_start := g.defers.len
	init_node := g.a.child_node(&node, 0)
	cond_id := g.a.child(&node, 1)
	cond_node := g.a.nodes[int(cond_id)]
	post_node := g.a.child_node(&node, 2)

	if init_node.kind == .empty && cond_node.kind == .empty && post_node.kind == .empty {
		g.writeln('for (;;) {')
	} else if init_node.kind == .empty && post_node.kind == .empty {
		g.write('while (')
		g.gen_expr(cond_id)
		g.writeln(') {')
	} else {
		g.write('for (')
		if init_node.kind != .empty {
			g.gen_node_inline(g.a.child(&node, 0))
		}
		g.write('; ')
		if cond_node.kind != .empty {
			g.gen_expr(cond_id)
		}
		g.write('; ')
		if post_node.kind != .empty {
			g.gen_node_inline(g.a.child(&node, 2))
		}
		g.writeln(') {')
	}
	g.indent++
	for i in 3 .. node.children_count {
		g.gen_node(g.a.child(&node, i))
	}
	g.gen_defers_from(defer_start)
	g.trim_defers(defer_start)
	g.indent--
	g.writeln('}')
	g.tc.pop_scope()
}

// gen_for_in emits for in output for c.
fn (mut g FlatGen) gen_for_in(node flat.Node) {
	g.tc.push_scope()
	header_count := node.value.int()
	val_id := g.a.child(&node, 1)
	var_node := if int(val_id) >= 0 {
		g.a.child_node(&node, 1)
	} else {
		g.a.child_node(&node, 0)
	}
	var_name := g.c_loop_local_name(var_node.value)
	g.tc.cur_scope.insert(var_node.value, types.Type(types.int_))
	body_start := header_count

	if header_count == 4 {
		panic('internal error: range for-in reached C backend after transform')
	} else if header_count == 3 {
		container := g.a.child_node(&node, 2)
		if container.kind == .range {
			panic('internal error: range for-in reached C backend after transform')
		} else {
			container_type := g.usable_expr_type(g.a.child(&node, 2))
			has_index := int(val_id) >= 0
			idx_var := if has_index {
				g.c_loop_local_name(g.a.child_node(&node, 0).value)
			} else {
				'__iter_${var_name}'
			}
			elem_var := if has_index {
				g.c_loop_local_name(g.a.child_node(&node, 1).value)
			} else {
				var_name
			}
			clean_container_type := types.unwrap_pointer(container_type)
			mut map_snapshot_var := ''
			if clean_container_type is types.Map {
				c_key := g.value_c_type(clean_container_type.key_type)
				c_val := g.value_c_type(clean_container_type.value_type)
				container_str := g.expr_to_string(g.a.child(&node, 2))
				iter_var := '__mi_${g.tmp_count}'
				g.tmp_count++
				key_var := if has_index { idx_var } else { '__mk_${g.tmp_count}' }
				val_var_ := if has_index { elem_var } else { var_name }
				use_snapshot := g.for_in_body_contains_delete_call(node, body_start)
				key_values := if use_snapshot {
					map_snapshot_var = '__for_map_${g.tmp_count}'
					g.tmp_count++
					if container_type is types.Pointer {
						g.writeln('map ${map_snapshot_var} = map__clone(${container_str});')
					} else {
						map_src := '__for_map_src_${g.tmp_count}'
						g.tmp_count++
						g.writeln('map ${map_src} = ${container_str};')
						g.writeln('map ${map_snapshot_var} = map__clone(&${map_src});')
					}
					'${map_snapshot_var}.key_values'
				} else {
					access := if container_type is types.Pointer { '->' } else { '.' }
					'(${container_str})${access}key_values'
				}
				g.writeln('for (int ${iter_var} = 0; ${iter_var} < ${key_values}.len; ${iter_var}++) {')
				g.indent++
				g.writeln('if (${key_values}.all_deleted && ${key_values}.all_deleted[${iter_var}]) continue;')
				key_slot := '${key_values}.keys + ${iter_var} * ${key_values}.key_bytes'
				if key_fixed := array_fixed_type(clean_container_type.key_type) {
					c_elem, dims := g.fixed_array_decl_parts(key_fixed)
					g.writeln('${c_elem} ${key_var}${dims};')
					g.writeln('memmove(${key_var}, ${key_slot}, sizeof(${key_var}));')
				} else {
					g.writeln('${c_key} ${key_var} = *(${c_key}*)(${key_slot});')
				}
				val_slot := '${key_values}.values + ${iter_var} * ${key_values}.value_bytes'
				if val_fixed := array_fixed_type(clean_container_type.value_type) {
					c_elem, dims := g.fixed_array_decl_parts(val_fixed)
					g.writeln('${c_elem} ${val_var_}${dims};')
					g.writeln('memmove(${val_var_}, ${val_slot}, sizeof(${val_var_}));')
				} else {
					g.writeln('${c_val} ${val_var_} = *(${c_val}*)(${val_slot});')
				}
				if has_index {
					g.tc.cur_scope.insert(key_var, clean_container_type.key_type)
				}
				g.tc.cur_scope.insert(val_var_, clean_container_type.value_type)
			} else if container_type is types.Array {
				c_elem := g.value_c_type(container_type.elem_type)
				mut container_str := g.expr_to_string(g.a.child(&node, 2))
				// A call-valued container (e.g. `threads.wait()`, `xs.map(..)`) is not
				// idempotent and is referenced multiple times below; bind it to a temp so
				// it runs exactly once.
				if g.a.nodes[int(g.a.child(&node, 2))].kind == .call {
					arr_tmp := '__for_arr_${g.tmp_count}'
					g.tmp_count++
					g.writeln('Array ${arr_tmp} = ${container_str};')
					container_str = arr_tmp
				}
				g.writeln('for (int ${idx_var} = 0; ${idx_var} < ${container_str}.len; ${idx_var}++) {')
				g.indent++
				g.write('${c_elem} ${elem_var} = *(')
				g.write(c_elem)
				g.writeln('*)array_get(${container_str}, ${idx_var});')
				g.tc.cur_scope.insert(elem_var, container_type.elem_type)
			} else if container_type is types.String {
				container_str := g.expr_to_string(g.a.child(&node, 2))
				g.writeln('for (int ${idx_var} = 0; ${idx_var} < ${container_str}.len; ${idx_var}++) {')
				g.indent++
				g.writeln('u8 ${elem_var} = ((u8*)${container_str}.str)[${idx_var}];')
				g.tc.cur_scope.insert(elem_var, types.Type(types.u8_))
			} else if container_type is types.ArrayFixed {
				af := container_type
				c_elem := g.value_c_type(af.elem_type)
				arr_len := g.fixed_array_len_value(af)
				g.writeln('for (int ${idx_var} = 0; ${idx_var} < ${arr_len}; ${idx_var}++) {')
				g.indent++
				g.write('${c_elem} ${elem_var} = ')
				g.gen_expr(g.a.child(&node, 2))
				g.writeln('[${idx_var}];')
				g.tc.cur_scope.insert(elem_var, af.elem_type)
			} else {
				g.writeln('for (int ${idx_var} = 0; ${idx_var} < 0; ${idx_var}++) {')
				g.indent++
				g.writeln('int ${elem_var} = 0;')
				g.tc.cur_scope.insert(elem_var, types.Type(types.int_))
			}
			if has_index && container_type !is types.Map {
				g.tc.cur_scope.insert(idx_var, types.Type(types.int_))
			}
			for i in body_start .. node.children_count {
				g.gen_node(g.a.child(&node, i))
			}
			g.indent--
			g.writeln('}')
			if map_snapshot_var.len > 0 {
				g.writeln('map__free(&${map_snapshot_var});')
			}
			g.tc.pop_scope()
			return
		}
	} else {
		g.tc.pop_scope()
		return
	}
	g.indent++
	for i in body_start .. node.children_count {
		g.gen_node(g.a.child(&node, i))
	}
	g.indent--
	g.writeln('}')
	g.tc.pop_scope()
}

fn (g &FlatGen) for_in_body_contains_delete_call(node flat.Node, body_start int) bool {
	for i in body_start .. node.children_count {
		if g.node_contains_delete_call(g.a.child(&node, i)) {
			return true
		}
	}
	return false
}

fn (g &FlatGen) node_contains_delete_call(id flat.NodeId) bool {
	if int(id) < 0 || int(id) >= g.a.nodes.len {
		return false
	}
	node := g.a.nodes[int(id)]
	if node.kind == .call && node.children_count > 0 {
		fn_node := g.a.child_node(&node, 0)
		if fn_node.kind == .selector && fn_node.value == 'delete' {
			return true
		}
	}
	for i in 0 .. node.children_count {
		if g.node_contains_delete_call(g.a.child(&node, i)) {
			return true
		}
	}
	return false
}

fn (g &FlatGen) c_loop_local_name(name string) string {
	if name.contains('.') {
		return c_name(name.all_after_last('.'))
	}
	if name.contains('__') {
		prefix := name.all_before_last('__')
		suffix := name.all_after_last('__')
		if suffix == 'index' {
			return c_name(suffix)
		}
		if g.has_import_alias(prefix) {
			return c_name(suffix)
		}
		for _, mod_name in g.modules {
			short_mod := if mod_name.contains('.') { mod_name.all_after_last('.') } else { mod_name }
			if prefix == short_mod {
				return c_name(suffix)
			}
		}
	}
	return c_name(name)
}

// gen_node_inline emits node inline output for c.
fn (mut g FlatGen) gen_node_inline(id flat.NodeId) {
	node := g.a.nodes[int(id)]
	match node.kind {
		.expr_stmt {
			g.gen_expr(g.a.child(&node, 0))
		}
		.decl_assign {
			lhs_id := g.a.child(&node, 0)
			rhs_id := g.a.child(&node, 1)
			lhs := g.a.nodes[int(lhs_id)]
			v_type := g.tc.resolve_type(rhs_id)
			typ := g.tc.c_type(v_type)
			g.write('${typ} ')
			if lhs.kind == .ident {
				g.write(g.c_loop_local_name(lhs.value))
			} else {
				g.gen_expr(lhs_id)
			}
			g.write(' = ')
			g.gen_expr(rhs_id)
			if lhs.kind == .ident {
				g.tc.cur_scope.insert(lhs.value, v_type)
			}
		}
		.assign {
			g.gen_expr(g.a.child(&node, 0))
			g.write(' ${g.op_str(node.op)} ')
			g.gen_expr(g.a.child(&node, 1))
		}
		else {}
	}
}
