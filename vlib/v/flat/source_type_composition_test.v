module flat

fn test_source_type_composition_collisions_preserve_owned_exact_tuples() {
	left := source_type_id('composition_collision.Left')
	middle := source_type_id('composition_collision.Middle')
	replacement := source_type_id('composition_collision.Replacement')
	right := source_type_id('composition_collision.Right')
	original := [left, middle, right]
	mut borrowed := original.clone()
	// Force the otherwise impractical full-hash collision. Equal bucket/hash,
	// tuple length and endpoints must still compare every numeric component.
	hash := u64(0x4f39_a7b2_529d_06cd)
	slot := int(hash & 4095)
	first := source_type_composition_miss(slot, .multi_return, borrowed, hash)
	borrowed[1] = replacement
	changed := source_type_composition_miss(slot, .multi_return, borrowed, hash)
	assert first != changed
	assert source_type_name(first) == '(composition_collision.Left, composition_collision.Middle, composition_collision.Right)'
	assert source_type_name(changed) == '(composition_collision.Left, composition_collision.Replacement, composition_collision.Right)'
	assert source_type_composition_find(slot, .multi_return, original, hash).id == first
	assert source_type_composition_find(slot, .multi_return, borrowed, hash).id == changed
	// The same numeric tuple and forced hash can describe a different spelling
	// constructor, and a prefix of the tuple is a different cache key.
	function := source_type_composition_miss(slot, .void_function_type, original, hash)
	shorter := source_type_composition_miss(slot, .multi_return, original[..2], hash)
	assert function != first
	assert shorter != first
	assert source_type_name(function) == 'fn(composition_collision.Left, composition_collision.Middle, composition_collision.Right)'
	assert source_type_name(shorter) == '(composition_collision.Left, composition_collision.Middle)'
	assert source_type_composition_find(slot, .multi_return, original, hash).id == first
	assert source_type_composition_find(slot, .void_function_type, original, hash).id == function
	assert source_type_composition_find(slot, .multi_return, original[..2], hash).id == shorter
}

fn test_source_type_composition_concatenates_every_part() {
	parts := [source_type_id('Box['), source_type_id('int'), source_type_id(', '),
		source_type_id('[]string'), source_type_id(']'), u32(0)]
	for _ in 0 .. 2 {
		assert source_type_composition(.concat, parts) == source_type_id('Box[int, []string]')
		assert source_type_composition(.concat, parts[..1]) == parts[0]
		assert source_type_composition(.concat, []u32{}) == 0
		assert source_type_composition(.concat, [u32(0)]) == 0
	}
}

fn test_source_type_composition_caches_empty_spelling() {
	// Force a known key so the exact cache entry can be checked, even when its
	// spelling ID is zero. A miss must differ from a cached empty spelling.
	hash := u64(0x61b3_46c2_68aa_b208)
	slot := int(hash & 4095)
	parts := []u32{}
	assert source_type_composition_miss(slot, .concat, parts, hash) == 0
	entry := source_type_composition_find(slot, .concat, parts, hash)
	assert !isnil(entry)
	assert entry.id == 0
	assert source_type_composition_miss(slot, .concat, parts, hash) == 0
	assert source_type_composition_find(slot, .concat, parts, hash) == entry
	assert isnil(source_type_composition_find(slot, .concat, [u32(0)], hash))
}

fn test_source_type_composition_inferred_expression_keeps_numeric_node_id() {
	for node_id in [u32(0), 42, u32(0x8000_0000), u32(0xffff_ffff)] {
		for _ in 0 .. 2 {
			assert source_type_composition_one(.inferred_expression, node_id) == source_type_id('typeof(__expr_${int(node_id)})')
		}
	}
}

fn test_source_type_composition_fastc_thread_handle_preserves_codec() {
	inputs := ['', 'void', 'void ', 'int', 'pkg.Type', '&[]u8', 'a_b', 'a_5fb', 'é']
	expected := ['__vf_thread_void', '__vf_thread_void', '__vf_thread_kvoid_20', '__vf_thread_kint',
		'__vf_thread_kpkg_2eType', '__vf_thread_k_26_5b_5du8', '__vf_thread_ka_5fb',
		'__vf_thread_ka_5f5fb', '__vf_thread_k_c3_a9']
	for i, raw in inputs {
		for _ in 0 .. 2 {
			assert source_type_composition_one(.fastc_thread_handle, source_type_id(raw)) == source_type_id(expected[i])
		}
	}
	assert source_type_composition_one(.fastc_thread_handle, source_type_id([u8(0), 255].bytestr())) == source_type_id('__vf_thread_k_00_ff')
}

fn test_source_type_composition_fastc_component_preserves_raw_codec() {
	inputs := ['', 'int', 'pkg.Type', '*', '_ptr', 'a*b c.d', 'fn (pkg.T*)', '\t T\n', 'é.*']
	expected := ['', 'int', 'pkg__Type', '_ptr', '_ptr', 'a_ptrb_c__d', 'fn_(pkg__T_ptr)', '\t_T\n',
		'é___ptr']
	for i, raw in inputs {
		id := source_type_id(raw)
		for _ in 0 .. 2 {
			assert source_type_composition_one(.fastc_composite_part, id) == source_type_id(expected[i])
		}
	}
	zero := [u8(0)].bytestr()
	last := [u8(255)].bytestr()
	raw := zero + ' *.' + last
	assert source_type_composition_one(.fastc_composite_part, source_type_id(raw)) == source_type_id(zero + '__ptr__' + last)
}
