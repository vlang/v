module flat

fn test_source_type_recent_ids_do_not_retain_or_trust_borrowed_input_bytes() {
	mut bytes := 'recent_original'.bytes()
	view := unsafe { tos(bytes.data, bytes.len) }
	original := source_type_id(view)
	assert source_type_name(original) == 'recent_original'
	bytes[0] = `R`
	changed := source_type_id(view)
	assert changed != original
	assert source_type_name(changed) == 'Recent_original'
	bytes[0] = `r`
	assert source_type_id(view) == original
	assert source_type_id('') == 0
}

fn test_source_type_recent_id_collisions_recheck_the_owned_spelling() {
	text := 'recent_collision_left'
	left := source_type_id(text)
	right := source_type_id('recent_collision_rite')
	assert left != right
	// A valid owned entry for another spelling is a cache collision, never an answer.
	g_source_type_recent_entries[source_type_recent_slot(text)] = source_type_entry(source_type_table_load(), right)
	assert source_type_id(text) == left
	assert source_type_name(right) == 'recent_collision_rite'
}

fn test_source_type_recent_ids_accept_bounded_unaligned_views() {
	for length in 1 .. 81 {
		for offset in 0 .. 16 {
			mut bytes := []u8{len: offset + length, init: `a` + u8(index % 23)}
			view := unsafe { tos(&u8(bytes.data) + offset, length) }
			text := view.clone()
			id := source_type_id(view)
			assert source_type_name(id) == text
			assert source_type_id(text) == id
			bytes[offset + length - 1] = `Z`
			assert source_type_id(view) != id
			assert source_type_name(id) == text
		}
	}
}
