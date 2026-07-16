module c

import v3.flat
import v3.types

fn (mut g FlatGen) take_pending_loop_label() string {
	label := g.pending_loop_label
	g.pending_loop_label = ''
	return label
}

fn (mut g FlatGen) push_loop_label_depth(label string) LoopLabelState {
	if label.len == 0 {
		return LoopLabelState{}
	}
	mut state := LoopLabelState{
		label: label
	}
	if prev_depth := g.loop_label_depths[label] {
		state.had_prev = true
		state.prev_depth = prev_depth
	}
	g.loop_label_depths[label] = g.loop_depth + 1
	return state
}

fn (mut g FlatGen) pop_loop_label_depth(state LoopLabelState) {
	if state.label.len == 0 {
		return
	}
	if state.had_prev {
		g.loop_label_depths[state.label] = state.prev_depth
	} else {
		g.loop_label_depths.delete(state.label)
	}
}

fn (g &FlatGen) labelled_continue_skip_drops_var(label string) string {
	return '__v_${g.cname(label)}_continue_skip_drops'
}

fn (mut g FlatGen) gen_labelled_continue_skip_drops_var(label string) {
	if label.len > 0 {
		g.writeln('bool ${g.labelled_continue_skip_drops_var(label)} = false;')
	}
}

fn (mut g FlatGen) gen_loop_iteration_ownership_drops_for_label(label string) {
	if label.len == 0 {
		g.gen_loop_iteration_ownership_drops()
		return
	}
	skip_drops := g.labelled_continue_skip_drops_var(label)
	g.writeln('if (!${skip_drops}) {')
	g.indent++
	g.gen_loop_iteration_ownership_drops()
	g.indent--
	g.writeln('}')
	g.writeln('${skip_drops} = false;')
}

// gen_for emits for output for c.
fn (mut g FlatGen) gen_for(node flat.Node) {
	label_state := g.push_loop_label_depth(g.take_pending_loop_label())
	g.push_scope()
	defer_start := g.defers.len
	init_node := g.a.child_node(&node, 0)
	cond_id := g.a.child(&node, 1)
	cond_node := g.a.nodes[int(cond_id)]
	post_node := g.a.child_node(&node, 2)
	wrap_init := init_node.kind != .empty

	if wrap_init {
		g.writeln('{')
		g.indent++
		if init_node.kind == .block && init_node.value == 'for_init_expanded' {
			for i in 0 .. init_node.children_count {
				g.gen_node(g.a.child(init_node, i))
			}
		} else {
			g.gen_node(g.a.child(&node, 0))
		}
	}

	if init_node.kind == .empty && cond_node.kind == .empty && post_node.kind == .empty {
		g.writeln('for (;;) {')
	} else if init_node.kind == .empty && post_node.kind == .empty {
		g.write('while (')
		g.gen_expr(cond_id)
		g.writeln(') {')
	} else {
		g.write('for (; ')
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
	g.gen_labelled_continue_skip_drops_var(label_state.label)
	g.loop_depth++
	for i in 3 .. node.children_count {
		g.gen_node(g.a.child(&node, i))
	}
	g.loop_depth--
	g.gen_defers_from(defer_start)
	g.gen_loop_iteration_ownership_drops_for_label(label_state.label)
	g.trim_defers(defer_start)
	g.indent--
	g.writeln('}')
	if wrap_init {
		g.gen_scope_ownership_drops()
		g.indent--
		g.writeln('}')
	}
	g.pop_scope()
	g.pop_loop_label_depth(label_state)
}

// gen_for_in emits for in output for c.
fn (mut g FlatGen) gen_for_in(node flat.Node) {
	label_state := g.push_loop_label_depth(g.take_pending_loop_label())
	defer {
		g.pop_loop_label_depth(label_state)
	}
	g.push_scope()
	header_count := node.value.int()
	val_id := g.a.child(&node, 1)
	var_node := if int(val_id) >= 0 {
		g.a.child_node(&node, 1)
	} else {
		g.a.child_node(&node, 0)
	}
	has_index := int(val_id) >= 0
	idx_binding_name := if has_index { g.a.child_node(&node, 0).value } else { '' }
	elem_binding_name := var_node.value
	var_name := g.c_loop_local_name(var_node.value)
	var_owner := g.tc.cur_scope.insert_with_owner(var_node.value, types.Type(types.int_))
	g.declare_local_pointer_storage(var_owner, false)
	body_start := header_count

	if header_count == 4 {
		low_id := g.a.child(&node, 2)
		high_id := g.a.child(&node, 3)
		g.gen_range_for_in(node, g.a.child(&node, 0), low_id, high_id, body_start,
			label_state.label)
		return
	} else if header_count == 3 {
		container := g.a.child_node(&node, 2)
		if container.kind == .range {
			if container.children_count >= 2 {
				g.gen_range_for_in(node, g.a.child(&node, 0), g.a.child(container, 0), g.a.child(container,
					1), body_start, label_state.label)
				return
			}
		} else {
			container_id := g.a.child(&node, 2)
			mut container_type := g.usable_expr_type(container_id)
			if const_storage_type := g.const_storage_type_from_node(container) {
				if fixed := array_fixed_type(types.unwrap_pointer(const_storage_type)) {
					container_type = types.Type(fixed)
				}
			}
			mut idx_var := ''
			if has_index {
				if idx_binding_name == '_' {
					idx_var = '__for_idx_${g.tmp_count}'
					g.tmp_count++
				} else {
					idx_var = g.c_loop_local_name(idx_binding_name)
				}
			} else {
				idx_var = '__iter_${var_name}'
			}
			elem_var := if has_index {
				g.c_loop_local_name(g.a.child_node(&node, 1).value)
			} else {
				var_name
			}
			mut clean_container_type := types.unwrap_pointer(container_type)
			for clean_container_type is types.Alias {
				clean_container_type =
					types.unwrap_pointer((clean_container_type as types.Alias).base_type)
			}
			mut map_snapshot_var := ''
			mut map_mut_value_copyback := ''
			mut map_copyback_dirty_var := ''
			mut map_copyback_guard := MapLoopCopybackGuard{}
			if clean_container_type is types.Map {
				c_key := g.map_key_temp_c_type(clean_container_type.key_type)
				c_val := g.value_c_type(clean_container_type.value_type)
				map_value_by_ref := node.op == .amp || container_type is types.Pointer
				container_str := g.expr_to_string(g.a.child(&node, 2))
				original_map_ref := if container_type is types.Pointer {
					container_str
				} else {
					'&${container_str}'
				}
				iter_var := '__mi_${g.tmp_count}'
				g.tmp_count++
				key_var := if has_index { idx_var } else { '__mk_${g.tmp_count}' }
				val_var_ := if has_index { elem_var } else { var_name }
				use_snapshot := g.for_in_body_contains_delete_call(node, body_start)
				mut key_ref := '&${key_var}'
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
				g.gen_labelled_continue_skip_drops_var(label_state.label)
				g.writeln('if (${key_values}.all_deleted && ${key_values}.all_deleted[${iter_var}]) continue;')
				key_slot := '${key_values}.keys + ${iter_var} * ${key_values}.key_bytes'
				if key_fixed := array_fixed_type(clean_container_type.key_type) {
					c_elem, dims := g.fixed_array_decl_parts(key_fixed)
					g.writeln('${c_elem} ${key_var}${dims};')
					g.writeln('memmove(${key_var}, ${key_slot}, sizeof(${key_var}));')
					key_ref = key_var
				} else {
					g.writeln('${c_key} ${key_var} = *(${c_key}*)(${key_slot});')
					if clean_container_type.key_type is types.String {
						g.writeln('${key_var} = string__clone(${key_var});')
					}
				}
				snapshot_val_slot := '${key_values}.values + ${iter_var} * ${key_values}.value_bytes'
				mut val_slot := snapshot_val_slot
				mut val_is_fixed_copy := false
				if use_snapshot && map_value_by_ref {
					val_slot_var := '__for_map_val_${g.tmp_count}'
					g.tmp_count++
					g.writeln('void* ${val_slot_var} = map__get_check(${original_map_ref}, &${key_var});')
					g.writeln('if (${val_slot_var} == 0) ${val_slot_var} = (void*)(${snapshot_val_slot});')
					val_slot = val_slot_var
				}
				if val_fixed := array_fixed_type(clean_container_type.value_type) {
					c_elem, dims := g.fixed_array_decl_parts(val_fixed)
					g.writeln('${c_elem} ${val_var_}${dims};')
					g.writeln('memmove(${val_var_}, ${val_slot}, sizeof(${val_var_}));')
					val_is_fixed_copy = true
					if node.op == .amp {
						mut copyback_slot := val_slot
						if use_snapshot {
							copyback_slot = '__for_map_copyback_${g.tmp_count}'
							g.tmp_count++
							if map_copyback_dirty_var.len == 0 {
								map_copyback_dirty_var = '__for_map_dirty_${g.tmp_count}'
								g.tmp_count++
							}
						}
						map_mut_value_copyback = 'memmove(${copyback_slot}, ${val_var_}, sizeof(${val_var_}));'
						if use_snapshot {
							map_mut_value_copyback = 'if (!${map_copyback_dirty_var}) { void* ${copyback_slot} = map__get_check(${original_map_ref}, &${key_var}); if (${copyback_slot} != 0) { ${map_mut_value_copyback} } }'
						}
					}
				} else if map_value_by_ref {
					g.writeln('${c_val}* ${val_var_} = (${c_val}*)(${val_slot});')
				} else {
					g.writeln('${c_val} ${val_var_} = *(${c_val}*)(${val_slot});')
					if node.op == .amp {
						mut copyback_slot := val_slot
						if use_snapshot {
							copyback_slot = '__for_map_copyback_${g.tmp_count}'
							g.tmp_count++
							if map_copyback_dirty_var.len == 0 {
								map_copyback_dirty_var = '__for_map_dirty_${g.tmp_count}'
								g.tmp_count++
							}
						}
						map_mut_value_copyback = 'memmove(${copyback_slot}, &${val_var_}, sizeof(${val_var_}));'
						if use_snapshot {
							map_mut_value_copyback = 'if (!${map_copyback_dirty_var}) { void* ${copyback_slot} = map__get_check(${original_map_ref}, &${key_var}); if (${copyback_slot} != 0) { ${map_mut_value_copyback} } }'
						}
					}
				}
				if map_copyback_dirty_var.len > 0 {
					map_copyback_guard = MapLoopCopybackGuard{
						map_ref:   original_map_ref
						key_ref:   key_ref
						dirty_var: map_copyback_dirty_var
					}
				}
				if has_index {
					key_owner := g.tc.cur_scope.insert_with_owner(idx_binding_name,
						clean_container_type.key_type)
					g.declare_local_pointer_storage(key_owner,
						clean_container_type.key_type is types.Pointer
						|| c_type_is_pointer_storage(c_key))
				}
				val_scope_type := if map_value_by_ref && !val_is_fixed_copy {
					types.Type(types.Pointer{
						base_type: clean_container_type.value_type
					})
				} else {
					clean_container_type.value_type
				}
				val_owner := g.tc.cur_scope.insert_with_owner(elem_binding_name, val_scope_type)
				g.declare_local_pointer_storage(val_owner, val_scope_type is types.Pointer
					|| (!val_is_fixed_copy && clean_container_type.value_type is types.Pointer)
					|| c_type_is_pointer_storage(c_val))
				g.declare_local_c_type(val_owner, if map_value_by_ref && !val_is_fixed_copy {
					'${c_val}*'
				} else {
					c_val
				})
			} else if container_type is types.Array {
				c_elem := g.value_c_type(container_type.elem_type)
				container_node := g.a.nodes[int(container_id)]
				mut container_str := if shared_payload := g.shared_array_payload_lvalue(container_id) {
					shared_payload
				} else {
					g.expr_to_string(container_id)
				}
				if storage_expr := shared_storage_from_payload_value_expr(container_str) {
					container_str = '${storage_expr}->val'
				}
				clean_container_str := container_str.trim_space()
				if clean_container_str.starts_with('*') && clean_container_str.ends_with('->val') {
					container_str = clean_container_str[1..]
				}
				// A call-valued container (e.g. `threads.wait()`, `xs.map(..)`) is not
				// idempotent and is referenced multiple times below; bind it to a temp so
				// it runs exactly once.
				if container_node.kind == .call {
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
				elem_scope_type := if node.op == .amp {
					types.Type(types.Pointer{
						base_type: container_type.elem_type
					})
				} else {
					container_type.elem_type
				}
				elem_owner := g.tc.cur_scope.insert_with_owner(elem_binding_name, elem_scope_type)
				g.declare_local_pointer_storage(elem_owner, elem_scope_type is types.Pointer
					|| c_type_is_pointer_storage(c_elem))
				g.declare_ierror_pointer_alias(elem_var,
					g.for_in_array_literal_element_needs_ierror_copy(container_node))
			} else if container_type is types.String {
				container_str := g.expr_to_string(g.a.child(&node, 2))
				g.writeln('for (int ${idx_var} = 0; ${idx_var} < ${container_str}.len; ${idx_var}++) {')
				g.indent++
				g.writeln('u8 ${elem_var} = ((u8*)${container_str}.str)[${idx_var}];')
				elem_owner := g.tc.cur_scope.insert_with_owner(elem_binding_name,
					types.Type(types.u8_))
				g.declare_local_pointer_storage(elem_owner, false)
			} else if container_type is types.ArrayFixed {
				af := container_type
				c_elem := g.value_c_type(af.elem_type)
				arr_len := g.fixed_array_len_value(af)
				g.writeln('for (int ${idx_var} = 0; ${idx_var} < ${arr_len}; ${idx_var}++) {')
				g.indent++
				g.write('${c_elem} ${elem_var} = ')
				g.gen_expr(g.a.child(&node, 2))
				g.writeln('[${idx_var}];')
				elem_owner := g.tc.cur_scope.insert_with_owner(elem_binding_name, af.elem_type)
				g.declare_local_pointer_storage(elem_owner, af.elem_type is types.Pointer
					|| c_type_is_pointer_storage(c_elem))
			} else {
				g.writeln('for (int ${idx_var} = 0; ${idx_var} < 0; ${idx_var}++) {')
				g.indent++
				g.writeln('int ${elem_var} = 0;')
				elem_owner := g.tc.cur_scope.insert_with_owner(elem_binding_name,
					types.Type(types.int_))
				g.declare_local_pointer_storage(elem_owner, false)
			}
			if has_index && container_type !is types.Map {
				idx_owner := g.tc.cur_scope.insert_with_owner(idx_binding_name,
					types.Type(types.int_))
				g.declare_local_pointer_storage(idx_owner, false)
			}
			if clean_container_type !is types.Map {
				g.gen_labelled_continue_skip_drops_var(label_state.label)
			}
			g.loop_depth++
			if map_copyback_guard.dirty_var.len > 0 {
				g.writeln('bool ${map_copyback_guard.dirty_var} = false;')
				g.map_loop_copyback_guards << map_copyback_guard
			}
			if map_mut_value_copyback.len > 0 {
				g.loop_control_copybacks << LoopControlCopyback{
					stmt:       map_mut_value_copyback
					loop_depth: g.loop_depth
				}
			}
			for i in body_start .. node.children_count {
				g.gen_node(g.a.child(&node, i))
			}
			if map_copyback_guard.dirty_var.len > 0 {
				g.map_loop_copyback_guards.delete_last()
			}
			if map_mut_value_copyback.len > 0 {
				g.writeln(map_mut_value_copyback)
				g.loop_control_copybacks.delete_last()
			}
			g.gen_loop_iteration_ownership_drops_for_label(label_state.label)
			g.loop_depth--
			g.indent--
			g.writeln('}')
			if map_snapshot_var.len > 0 {
				g.writeln('map__free(&${map_snapshot_var});')
			}
			g.pop_scope()
			return
		}
	} else {
		g.pop_scope()
		return
	}
	g.indent++
	g.gen_labelled_continue_skip_drops_var(label_state.label)
	g.loop_depth++
	for i in body_start .. node.children_count {
		g.gen_node(g.a.child(&node, i))
	}
	g.gen_loop_iteration_ownership_drops_for_label(label_state.label)
	g.loop_depth--
	g.indent--
	g.writeln('}')
	g.pop_scope()
}

fn (g &FlatGen) for_in_array_literal_element_needs_ierror_copy(container flat.Node) bool {
	if container.kind != .array_literal {
		return false
	}
	for i in 0 .. container.children_count {
		if g.ierror_pointer_payload_expr_needs_heap_copy(g.a.nodes[int(g.a.child(&container, i))]) {
			return true
		}
	}
	return false
}

fn (mut g FlatGen) gen_range_for_in(node flat.Node, key_id flat.NodeId, low_id flat.NodeId, high_id flat.NodeId, body_start int, label string) {
	key := g.a.node(key_id)
	if key.kind != .ident || key.value.len == 0 {
		g.pop_scope()
		return
	}
	key_name := if key.value == '_' {
		'__discard_${int(key_id)}'
	} else {
		g.c_loop_local_name(key.value)
	}
	low_type := g.usable_expr_type(low_id)
	range_type := if low_type is types.Primitive || low_type is types.ISize
		|| low_type is types.USize {
		low_type
	} else {
		types.Type(types.int_)
	}
	ct := g.value_c_type(range_type)
	low_name := '__range_low_${g.tmp_count}'
	g.tmp_count++
	g.write('${ct} ${low_name} = ')
	g.gen_expr(low_id)
	g.writeln(';')
	high_name := '__range_high_${g.tmp_count}'
	g.tmp_count++
	g.write('${ct} ${high_name} = ')
	g.gen_expr(high_id)
	g.writeln(';')
	g.tc.cur_scope.insert(key.value, range_type)
	g.writeln('for (${ct} ${key_name} = ${low_name}; ${key_name} < ${high_name}; ${key_name}++) {')
	g.indent++
	g.gen_labelled_continue_skip_drops_var(label)
	g.loop_depth++
	for i in body_start .. node.children_count {
		g.gen_node(g.a.child(&node, i))
	}
	g.gen_loop_iteration_ownership_drops_for_label(label)
	g.loop_depth--
	g.indent--
	g.writeln('}')
	g.pop_scope()
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
		if fn_node.kind == .ident && fn_node.value in ['map.delete', 'map__delete'] {
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

fn (mut g FlatGen) gen_map_loop_copyback_dirty_checks(map_ptr_expr string, key_ptr_expr string) {
	for guard in g.map_loop_copyback_guards {
		g.writeln('if (!${guard.dirty_var} && (${map_ptr_expr}) == (${guard.map_ref}) && (${guard.map_ref})->key_eq_fn(${key_ptr_expr}, ${guard.key_ref})) ${guard.dirty_var} = true;')
	}
}

fn (g &FlatGen) c_loop_local_name(name string) string {
	if name.contains('.') {
		return g.cname(name.all_after_last('.'))
	}
	if name.contains('__') {
		prefix := name.all_before_last('__')
		suffix := name.all_after_last('__')
		if suffix == 'index' {
			return g.cname(suffix)
		}
		if g.has_import_alias(prefix) {
			return g.cname(suffix)
		}
		for _, mod_name in g.modules {
			short_mod := if mod_name.contains('.') { mod_name.all_after_last('.') } else { mod_name }
			if prefix == short_mod {
				return g.cname(suffix)
			}
		}
	}
	return g.cname(name)
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
				owner := g.tc.cur_scope.insert_with_owner(lhs.value, v_type)
				g.track_local_pointer_storage_decl(lhs, owner, v_type, typ)
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
