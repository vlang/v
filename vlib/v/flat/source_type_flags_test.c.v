module flat

fn test_source_type_flags_match_entries() {
	spellings := ['flags.Plain', '&flags.Pointer', '[]flags.Array', 'map[string]flags.Value',
		'?flags.Option', '!flags.Result', 'fn (int) flags.Ret', 'shared flags.Shared', 'C.flags_struct',
		'main.FlagsLocal', 'T', 'unknown', 'Array_flags_T', ' flags.Padded ', 'shared_flags.Candidate',
		'(', ')', '[', '([)']
	for text in spellings {
		id := source_type_id(text)
		entry := source_type_read_entry(id)
		assert source_type_shape(id) == entry.shape, text
		assert source_type_query(id) == entry.query, text
	}
}

fn test_source_type_flags_empty_spelling() {
	assert source_type_shape(0) == unsafe { SourceTypeShape(0) }
	assert source_type_query(0) == SourceTypeQuery.complete
}

fn test_source_type_flags_cover_later_chunks() {
	// Enough spellings to publish IDs in a second entry chunk.
	mut ids := []u32{cap: source_type_chunk_size + 8}
	for i in 0 .. source_type_chunk_size + 8 {
		ids << source_type_id('[]flags.Chunk_${i}')
	}
	for id in ids {
		entry := source_type_read_entry(id)
		assert source_type_shape(id).has(.array_type)
		assert source_type_shape(id) == entry.shape
		assert source_type_query(id) == entry.query
	}
}
