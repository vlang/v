	args << t.make_ident(hash_fn)
	args << t.make_ident(eq_fn)
	args << t.make_ident(clone_fn)
	args << t.make_ident(free_fn)
	return t.make_call_typed('new_map', args, map_type)
}

fn (t &Transformer) new_map_call_type(node flat.Node) string {
	if node.kind != .call || node.children_count < 3 {
		return ''
	}
	callee := t.a.child_node(&node, 0)
	if callee.kind != .ident || callee.value != 'new_map' {
		return ''
	}
	key_size := t.a.child_node(&node, 1)
	value_size := t.a.child_node(&node, 2)
	if key_size.kind != .sizeof_expr || value_size.kind != .sizeof_expr || key_size.value.len == 0
		|| value_size.value.len == 0 {
		return ''
	}
	return 'map[${key_size.value}]${value_size.value}'
}

// map_callback_names supports map callback names handling for transform.
fn map_callback_names(key_type string) (string, string, string, string) {
	if key_type == 'string' {
		return 'map_hash_string', 'map_eq_string', 'map_clone_string', 'map_free_string'
	}
	mut size_suffix := '4'
	if key_type in ['u8', 'i8', 'byte', 'bool', 'char'] {
		size_suffix = '1'
	} else if key_type in ['u16', 'i16'] {
		size_suffix = '2'
	} else if key_type in ['int', 'isize', 'usize', 'uint', 'voidptr', 'charptr', 'byteptr']
		|| key_type.starts_with('&') {
		// Match the target's key storage, not the host or a fixed 32-bit int.
		size_suffix = if types.platform_int_bits() == 32 { '4' } else { '8' }
	} else if key_type in ['i64', 'u64', 'f64']
		|| key_type.contains('Arc[') || key_type.contains('Arc_') {
		size_suffix = '8'
	}

	return 'map_hash_int_${size_suffix}', 'map_eq_int_${size_suffix}', 'map_clone_int_${size_suffix}', 'map_free_nop'
}

fn (t &Transformer) map_callback_names_for_type(key_type string) (string, string, string, string) {
	normalized_key := t.normalize_type_alias(key_type)
	if !isnil(t.tc) {
		clean := t.tc.parse_type(normalized_key)
		if clean is types.ArrayFixed {
			base := '${t.tc.c_type(clean)}_map_key'
			return '${base}_hash', '${base}_eq', '${base}_clone', '${base}_free'
		}
	}
	return map_callback_names(normalized_key)
}

// map_index_info supports map index info handling for Transformer.
fn (mut t Transformer) map_index_info(index_id flat.NodeId) ?MapIndexInfo {
	if int(index_id) < 0 {
		return none
	}
	lhs := t.a.nodes[int(index_id)]
	if lhs.kind != .index || lhs.children_count < 2 || lhs.value == 'range' {
		return none
	}
	base_id := t.a.child(&lhs, 0)
	key_id := t.a.child(&lhs, 1)
	mut base_type := t.node_type(base_id)
	base_node := t.a.nodes[int(base_id)]
