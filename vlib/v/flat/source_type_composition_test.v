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
	assert source_type_composition_find(slot, .multi_return, original, hash) == first
	assert source_type_composition_find(slot, .multi_return, borrowed, hash) == changed
	// The same numeric tuple and forced hash can describe a different spelling
	// constructor, and a prefix of the tuple is a different cache key.
	function := source_type_composition_miss(slot, .void_function_type, original, hash)
	shorter := source_type_composition_miss(slot, .multi_return, original[..2], hash)
	assert function != first
	assert shorter != first
	assert source_type_name(function) == 'fn(composition_collision.Left, composition_collision.Middle, composition_collision.Right)'
	assert source_type_name(shorter) == '(composition_collision.Left, composition_collision.Middle)'
	assert source_type_composition_find(slot, .multi_return, original, hash) == first
	assert source_type_composition_find(slot, .void_function_type, original, hash) == function
	assert source_type_composition_find(slot, .multi_return, original[..2], hash) == shorter
}
