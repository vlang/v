module embed_file

// A payload longer than what one C object accepts is emitted in pieces, and the
// generated code joins them at startup, so what join_chunks builds must be exactly
// the bytes the compiler would have embedded.
fn test_join_chunks_concatenates_the_chunks_in_order() {
	payload := [u8(1), 2, 3, 4, 5, 6]
	chunks := [EmbedFileChunk{
		data: unsafe { &payload[0] }
		len:  2
	}, EmbedFileChunk{
		data: unsafe { &payload[2] }
		len:  3
	}, EmbedFileChunk{
		data: unsafe { &payload[5] }
		len:  1
	}]
	joined := join_chunks(&chunks[0], 6)
	assert unsafe { joined.vbytes(6) } == payload
	// The buffer outlives the array it was built from, so it is safe to keep.
	assert unsafe { joined.vbytes(6) } == payload
}

// A zero-length entry with no pointer is the end of the table, and a non-zero
// offset with no further data is simply truncated there.
fn test_join_chunks_stops_at_the_terminator_and_truncates_to_the_requested_length() {
	payload := [u8(9), 8, 7, 6]
	chunks := [EmbedFileChunk{
		data: unsafe { &payload[0] }
		len:  2
	}, EmbedFileChunk{}, EmbedFileChunk{
		data: unsafe { &payload[2] }
		len:  2
	}]
	joined := join_chunks(&chunks[0], 4)
	assert unsafe { joined.vbytes(2) } == [u8(9), 8]
	// The count is what bounds the join, so a count past the data stops short.
	short := join_chunks(&chunks[0], 3)
	assert unsafe { short.vbytes(3) } == [u8(9), 8, 0]
	// A count of zero still returns a usable buffer rather than a null pointer.
	assert join_chunks(&chunks[0], 0) != unsafe { nil }
}

// The index is what `-usecache` looks the embed up in, and it ends with a
// negative id: a lookup for a path that is not there must return that entry
// rather than run past the table.
fn test_find_index_entry_by_path_matches_the_path_and_the_algorithm() {
	first := 'module one'.bytes()
	second := 'module two'.bytes()
	entries := [
		EmbedFileIndexEntry{
			id:   0
			path: 'one.bin'
			algo: 'zstd'
			data: unsafe { &first[0] }
		},
		EmbedFileIndexEntry{
			id:   1
			path: 'two.bin'
			algo: 'zstd'
			data: unsafe { &second[0] }
		},
		EmbedFileIndexEntry{
			id:   -1
			path: ''
			algo: ''
			data: unsafe { nil }
		},
	]
	found := find_index_entry_by_path(unsafe { &entries[0] }, 'two.bin', 'zstd')
	assert found.id == 1
	assert found.path == 'two.bin'
	assert unsafe { found.data } == unsafe { &second[0] }
	// A different algorithm is a different entry even at the same path.
	missed := find_index_entry_by_path(unsafe { &entries[0] }, 'two.bin', 'gzip')
	assert missed.id == -1
	// So is a path that is not in the table at all.
	missing := find_index_entry_by_path(unsafe { &entries[0] }, 'three.bin', 'zstd')
	assert missing.id == -1
}
