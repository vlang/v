module c

import v3.flat
import v3.types

// array_like_type supports array like type handling for c.
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

// array_fixed_type supports array fixed type handling for c.
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

fn fixed_array_index_info(t types.Type) (bool, bool, types.ArrayFixed) {
	if fixed := array_fixed_type(t) {
		return true, false, fixed
	}
	if t is types.Pointer {
		if fixed := array_fixed_type(t.base_type) {
			return true, true, fixed
		}
	}
	return false, false, types.ArrayFixed{}
}

// gen_array_literal_value emits array literal value output for c.
fn (mut g FlatGen) gen_array_literal_value(node flat.Node, elem_type types.Type) {
	c_elem := g.tc.c_type(elem_type)
	count := node.children_count
	if count == 0 {
		g.write('array_new(sizeof(${c_elem}), 0, 0)')
		return
	}
	g.write('new_array_from_c_array(${count}, ${count}, sizeof(${c_elem}), (${c_elem}[]){')
	for i in 0 .. count {
		if i > 0 {
			g.write(', ')
		}
		// Emit each element against the concrete element type, not the enclosing
		// `expected_expr_type` (which is the whole array). A bare generic struct element
		// (`Box{..}` in a `[]Box[int]` literal) otherwise sees the array type, fails the
		// `generic_struct_init_instance_type` array skip, and is emitted as the bare `Box`
		// while the array storage is `Box_int` — incompatible C.
		g.gen_expr_with_expected_type(g.a.child(&node, i), elem_type)
	}
	g.write('})')
}

fn (mut g FlatGen) gen_array_literal_ptr_arg(node flat.Node, elem_type types.Type) {
	c_elem := g.tc.c_type(elem_type)
	g.write('(${c_elem}[]){')
	for i in 0 .. node.children_count {
		if i > 0 {
			g.write(', ')
		}
		g.gen_expr_with_expected_type(g.a.child(&node, i), elem_type)
	}
	if node.children_count == 0 {
		g.write('0')
	}
	g.write('}')
}

fn (mut g FlatGen) gen_pointer_arg_from_array_literal(node flat.Node, expected types.Type) bool {
	if node.kind != .array_literal {
		return false
	}
	if expected is types.Pointer {
		g.gen_array_literal_ptr_arg(node, expected.base_type)
		return true
	}
	return false
}

// gen_fixed_array_data_arg emits fixed array data arg output for c.
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
	if node.kind == .postfix && node.children_count > 0 {
		child_id := g.a.child(&node, 0)
		child := g.a.nodes[int(child_id)]
		if child.kind == .array_literal {
			g.gen_fixed_array_data_arg(child_id, arr)
			return
		}
	}
	// A fixed-array value (e.g. `[4]u8` color) is sometimes represented as a dynamic
	// `Array`; a C fixed-array parameter decays to `elem*`, so pass the data pointer.
	if g.tc.resolve_type(id) is types.Array {
		elem_ct := g.tc.c_type(arr.elem_type)
		g.write('(${elem_ct}*)(')
		g.gen_expr(id)
		g.write(').data')
		return
	}
	g.gen_expr(id)
}

// gen_array_push_many_stmt emits array push many stmt output for c.
fn (mut g FlatGen) gen_array_push_many_stmt(lhs_id flat.NodeId, rhs_id flat.NodeId) {
	lhs_is_ptr := g.tc.resolve_type(lhs_id) is types.Pointer
	amp := if lhs_is_ptr { '' } else { '&' }
	rhs_type := types.unwrap_pointer(g.tc.resolve_type(rhs_id))
	if rhs_fixed := array_fixed_type(rhs_type) {
		g.write('array_push_many_ptr(${amp}')
		gen_expr_lvalue(mut g, lhs_id)
		g.write(', ')
		g.gen_fixed_array_data_arg(rhs_id, rhs_fixed)
		len_expr := g.fixed_array_len_value(rhs_fixed)
		g.writeln(', ${len_expr});')
		return
	}
	tmp := g.tmp_name()
	g.write('{ Array ${tmp} = ')
	g.gen_expr(rhs_id)
	g.writeln(';')
	g.write('array__push_many(${amp}')
	gen_expr_lvalue(mut g, lhs_id)
	g.writeln(', ${tmp}.data, ${tmp}.len); }')
}

// gen_slice_expr emits slice expr output for c.
fn (mut g FlatGen) gen_slice_expr(node flat.Node, base_id flat.NodeId, base_type types.Type) {
	start_node := g.a.child_node(&node, 1)
	has_start := start_node.kind != .empty
	has_end := node.children_count > 2
	base_str := g.expr_to_string(base_id)
	is_array, is_ptr, _ := array_index_info(base_type)
	is_fixed_array, fixed_is_ptr, fixed := fixed_array_index_info(base_type)
	start_str := if has_start { g.expr_to_string(g.a.child(&node, 1)) } else { '0' }
	end_str := if has_end {
		g.expr_to_string(g.a.child(&node, 2))
	} else if is_fixed_array {
		g.fixed_array_len_value(fixed)
	} else if is_array && is_ptr {
		'${base_str}->len'
	} else {
		'${base_str}.len'
	}
	if base_type is types.String {
		g.write('string__substr(${base_str}, ${start_str}, ${end_str})')
	} else if is_fixed_array {
		c_elem := g.tc.c_type(fixed.elem_type)
		data_str := if fixed_is_ptr { '(*${base_str})' } else { base_str }
		// Evaluate the slice bounds once so side-effecting expressions such as
		// `arr[i++..limit()]` are not run multiple times in the generated C.
		start_tmp := g.tmp_name()
		count_tmp := g.tmp_name()
		g.write('({ int ${start_tmp} = (${start_str}); int ${count_tmp} = (${end_str}) - ${start_tmp}; new_array_from_c_array(${count_tmp}, ${count_tmp}, sizeof(${c_elem}), &${data_str}[${start_tmp}]); })')
	} else if is_array {
		arr_str := if is_ptr { '*${base_str}' } else { base_str }
		g.write('array_slice(${arr_str}, ${start_str}, ${end_str})')
	} else {
		g.write('string__substr(${base_str}, ${start_str}, ${end_str})')
	}
}

// gen_array_method_call emits array method call output for c.
fn (mut g FlatGen) gen_array_method_call(node flat.Node, fn_node &flat.Node, arr types.Array) {
	c_elem := g.value_c_type(arr.elem_type)
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
			g.write('array__clone(')
			if !is_ptr {
				g.write('&')
			}
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
		'prepend' {
			amp := if is_ptr { '' } else { '&' }
			g.write('array__prepend(${amp}')
			g.gen_expr(base_id)
			g.write(', &(${c_elem}[]){')
			g.gen_expr(g.a.child(&node, 1))
			g.write('})')
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
			g.write('Array_string__join(')
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
		'wait' {
			// Only a thread array supports `.wait()` (joining every spawned thread and,
			// for non-void payloads, collecting their return values into a fresh `[]T`).
			// The element carries the thread payload in its name (`thread`/`thread T`).
			// Any other element type is not a thread, so route it through the normal
			// method fallback instead of joining arbitrary array data as pthread_t handles.
			mut is_thread := false
			elem := arr.elem_type
			if elem is types.Struct {
				tn := elem.name.trim_space()
				is_thread = tn == 'thread' || tn.starts_with('thread ')
			}
			if is_thread {
				g.gen_thread_array_wait(base_id, is_ptr, arr.elem_type)
			} else {
				g.gen_array_method_call_fallback(node, fn_node.value, base_id, is_ptr)
			}
		}
		else {
			g.gen_array_method_call_fallback(node, fn_node.value, base_id, is_ptr)
		}
	}
}

// gen_array_method_call_fallback emits a call for an array method that has no dedicated
// codegen arm: it resolves a `[]T.method` function when one is registered, and otherwise
// emits the selector itself as a direct call. Shared by the catch-all `else` arm and by
// `.wait()` on non-thread arrays (which is unsupported and falls through here rather than
// joining elements as thread handles).
fn (mut g FlatGen) gen_array_method_call_fallback(node flat.Node, mname string, base_id flat.NodeId, is_ptr bool) {
	best_mname := g.array_method_fallback(mname)
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

// gen_thread_array_wait emits a call to the (lazily generated) wait function for a
// `[]thread T` receiver. The element type carries the thread's return type in its
// name (`thread T`); a bare `thread` denotes a void payload.
fn (mut g FlatGen) gen_thread_array_wait(base_id flat.NodeId, is_ptr bool, elem_type types.Type) {
	mut ret_name := ''
	if elem_type is types.Struct {
		trimmed := elem_type.name.trim_space()
		if trimmed != 'thread' && trimmed.starts_with('thread ') {
			ret_name = trimmed[7..].trim_space()
		}
	}
	fn_name := g.ensure_thread_arr_wait_fn(ret_name)
	g.write('${fn_name}(')
	if is_ptr {
		g.write('*')
	}
	g.gen_expr(base_id)
	g.write(')')
}

// ensure_thread_arr_wait_fn registers (once per payload type) a function that joins
// every thread handle in the array and, for a non-void payload, copies each thread's
// heap-returned value into a result `[]T` (freeing the per-thread allocation).
fn (mut g FlatGen) ensure_thread_arr_wait_fn(ret_name string) string {
	is_void := ret_name.len == 0
	// Match the ABI return type the spawn wrapper stores (gen_spawn_expr): an
	// option/result payload is `Optional_T`, a fixed-array payload its `_v_ret_*`
	// wrapper — not the bare `c_type`, or the malloc'd and read-back layouts diverge.
	ret_ct := if is_void { 'void' } else { g.fn_return_type_name(g.tc.parse_type(ret_name)) }
	key := 'threadwait|${ret_ct}'
	if name := g.spawn_wrapper_names[key] {
		return name
	}
	// Sanitize the payload C type (`Foo*`, `void*`, ...) into an identifier fragment
	// — `c_name` does not strip `*`, so a raw pointer return type would otherwise put
	// an asterisk in the helper symbol.
	name := c_name('__v_thread_arr_wait_${types.c_type_name_part(ret_ct)}')
	g.spawn_wrapper_names[key] = name
	if is_void {
		g.spawn_wrapper_defs << 'static void ${name}(Array a) { for (int __i = 0; __i < a.len; __i++) { void* __r = NULL; pthread_join((pthread_t)(((void**)a.data)[__i]), &__r); if (__r) free(__r); } }'
	} else {
		g.spawn_wrapper_defs << 'static Array ${name}(Array a) { Array __res = array_new(sizeof(${ret_ct}), a.len, a.len); for (int __i = 0; __i < a.len; __i++) { void* __r = NULL; pthread_join((pthread_t)(((void**)a.data)[__i]), &__r); if (__r) { ((${ret_ct}*)__res.data)[__i] = *(${ret_ct}*)__r; free(__r); } } return __res; }'
	}
	return name
}

// array_lookup_suffix supports array lookup suffix handling for c.
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

// array_method_fallback supports array method fallback handling for FlatGen.
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

// gen_map_ref_arg emits map ref arg output for c.
fn (mut g FlatGen) gen_map_ref_arg(base_id flat.NodeId, base_type types.Type) {
	if base_type is types.Pointer {
		g.gen_expr(base_id)
	} else {
		g.write('&')
		g.gen_expr(base_id)
	}
}

// gen_map_delete emits map delete output for c.
fn (mut g FlatGen) gen_map_delete(node flat.Node, fn_node &flat.Node, m types.Map, base_type types.Type) {
	c_key := g.tc.c_type(m.key_type)
	g.write('map__delete(')
	g.gen_map_ref_arg(g.a.child(fn_node, 0), base_type)
	g.write(', &(${c_key}[]){')
	g.gen_expr(g.a.child(&node, 1))
	g.write('})')
}

// gen_index_assign emits index assign output for c.
fn (mut g FlatGen) gen_index_assign(node flat.Node) {
	lhs_id := g.a.child(&node, 0)
	lhs := g.a.nodes[int(lhs_id)]
	if lhs.kind == .index {
		base_id := g.a.child(&lhs, 0)
		base_type := g.tc.resolve_type(base_id)
		clean_base := types.unwrap_pointer(base_type)
		if clean_base is types.Map {
			c_key := g.value_c_type(clean_base.key_type)
			c_val := g.value_c_type(clean_base.value_type)
			is_ptr := base_type is types.Pointer
			if is_ptr {
				g.write('map__set(')
			} else {
				g.write('map__set(&')
			}
			g.gen_expr(base_id)
			g.write(', &(${c_key}[]){')
			g.gen_expr_with_expected_type(g.a.child(&lhs, 1), clean_base.key_type)
			g.write('}, &(${c_val}[]){')
			g.gen_expr_with_expected_type(g.a.child(&node, 1), clean_base.value_type)
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
			c_elem := g.value_c_type(arr_type.elem_type)
			g.write('array_set(')
			if base_type is types.Pointer {
				g.write('*')
			}
			g.gen_expr(base_id)
			g.write(', ')
			g.gen_expr(g.a.child(&lhs, 1))
			g.write(', &(${c_elem}[]){')
			g.gen_expr_with_expected_type(g.a.child(&node, 1), arr_type.elem_type)
			g.writeln('});')
			return
		}
	}
	g.gen_assign(node)
}
