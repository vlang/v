module flat

fn test_source_type_existing_id_preserves_empty_and_absent_spellings() {
	known := source_type_id('existing_lookup.Known')
	table := source_type_table_load()
	count := C.atomic_load_u32(&table.count)
	assert (source_type_existing_id('') or { panic('empty spelling missing') }) == 0
	assert (source_type_existing_id('existing_lookup.Known') or { panic('known spelling missing') }) == known
	if _ := source_type_existing_id('existing_lookup.NeverInterned') {
		assert false
	}
	assert C.atomic_load_u32(&table.count) == count
}

fn test_source_type_existing_id_rechecks_borrowed_bytes_and_cache_collisions() {
	text := 'existing_lookup.Borrowed'
	id := source_type_id(text)
	other := source_type_id('existing_lookup.Other')
	entry := source_type_entry(source_type_table_load(), other)
	slot := source_type_recent_slot(text)
	// A valid entry for a different spelling must not answer this lookup.
	$if x32 {
		C.atomic_store_u32(voidptr(&g_source_type_recent_entries[slot]), u32(voidptr(entry)))
	} $else {
		C.atomic_store_u64(voidptr(&g_source_type_recent_entries[slot]), u64(voidptr(entry)))
	}
	mut bytes := text.bytes()
	view := unsafe { tos(bytes.data, bytes.len) }
	assert (source_type_existing_id(view) or { panic('borrowed spelling missing') }) == id
	bytes[0] = `E`
	count := C.atomic_load_u32(&source_type_table_load().count)
	if _ := source_type_existing_id(view) {
		assert false
	}
	assert C.atomic_load_u32(&source_type_table_load().count) == count
	bytes[0] = `e`
	assert (source_type_existing_id(view) or { panic('restored spelling missing') }) == id
	assert source_type_name(id) == text
}
