fn (mut g FlatGen) map_key_temp_c_type(key_type types.Type) string {
	if key_type is types.Enum {
		return g.enum_storage_c_type(key_type)
	}
	return g.value_c_type(key_type)
}

// map_callback_names supports map callback names handling for FlatGen.
fn (g &FlatGen) map_callback_names(key_type types.Type) (string, string, string, string) {
	// Aliases have the same key representation and callbacks as their base type.
	clean_key := cgen_unalias_type(key_type)
	if clean_key is types.String {
		return 'map_hash_string', 'map_eq_string', 'map_clone_string', 'map_free_string'
	}
	if clean_key is types.ArrayFixed {
		base := '${g.tc.c_type(clean_key)}_map_key'
		return '${base}_hash', '${base}_eq', '${base}_clone', '${base}_free'
	}
	c_key := if clean_key is types.Enum {
		g.enum_storage_c_type(clean_key)
	} else {
		g.tc.c_type(clean_key)
	}
	size_suffix := map_integer_callback_size_suffix(clean_key, c_key, g.target.pointer_bits)

	return 'map_hash_int_${size_suffix}', 'map_eq_int_${size_suffix}', 'map_clone_int_${size_suffix}', 'map_free_nop'
}

fn map_integer_callback_size_suffix(key_type types.Type, c_key string, pointer_bits int) string {
	if c_key in ['u8', 'i8', 'bool', 'char'] {
		return '1'
	}
	if c_key in ['u16', 'i16'] {
		return '2'
	}
	if key_type is types.Pointer || key_type is types.ISize || key_type is types.USize {
		return if pointer_bits == 32 { '4' } else { '8' }
	}
	if c_key in ['i64', 'u64', 'f64', 'double'] || c_key.starts_with('arc__Arc_') {
		return '8'
	}
	return '4'
}

fn (mut g FlatGen) precompute_fixed_array_map_key_types() {
	for node in g.a.nodes {
		if node.kind != .call || node.children_count < 3 {
			continue
		}
		callee := g.a.child_node(&node, 0)
		key_size := g.a.child_node(&node, 1)
