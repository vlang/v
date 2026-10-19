module builtin

@[aligned: 64]
struct AlignedCloneCell {
	value u64
}

fn clone_aligned_map_key(destination voidptr, source voidptr) {
	// The raw map callback copies the complete aligned key, including its padding.
	unsafe { vmemcpy(destination, source, sizeof(AlignedCloneCell)) }
}

fn assert_aligned_map_clone_storage(value &map) {
	assert value.data.key_values.key_alignment == 6
	assert value.data.key_values.value_alignment == 6
	if value.data.count == 0 {
		return
	}
	assert usize(value.data.key_values.keys) % 64 == 0
	assert usize(value.data.key_values.values) % 64 == 0
	keys := value.keys()
	values := value.values()
	assert keys.data_alignment() == 64
	assert values.data_alignment() == 64
	assert usize(keys.data) % 64 == 0
	assert usize(values.data) % 64 == 0
	for i in 0 .. keys.len {
		// Raw map exports have the same aligned element layout as the input cells.
		key := unsafe { &AlignedCloneCell(&u8(keys.data) + i * int(sizeof(AlignedCloneCell))) }
		item := unsafe { &AlignedCloneCell(&u8(values.data) + i * int(sizeof(AlignedCloneCell))) }
		assert usize(key) % 64 == 0
		assert usize(item) % 64 == 0
		assert key.value == u64(i + 1)
		assert item.value == key.value * 10
		zero := AlignedCloneCell{}
		stored := unsafe { &AlignedCloneCell(value.get(key, &zero)) }
		assert usize(stored) % 64 == 0
		assert stored.value == item.value
	}
	unsafe {
		keys.free()
		values.free()
	}
}

fn test_map_clones_preserve_key_and_value_alignment_in_every_storage_path() {
	assert sizeof(AlignedCloneCell) == 64
	for reservation in [u32(0), 256] {
		mut original := new_map_aligned(int(sizeof(AlignedCloneCell)),
			int(sizeof(AlignedCloneCell)), map_hash_int_8, map_eq_int_8,
			clone_aligned_map_key, map_free_nop, 64, 64)
		original.reserve(reservation)
		mut empty := unsafe { original.clone() }
		assert_aligned_map_clone_storage(&empty)
		for n in 1 .. 4 {
			key := AlignedCloneCell{ value: u64(n) }
			item := AlignedCloneCell{ value: u64(n * 10) }
			original.set(&key, &item)
		}
		mut copied := unsafe { original.clone() }
		if reservation == 256 {
			assert original.data.even_index >= 128
			assert copied.data.even_index < original.data.even_index
		}
		assert_aligned_map_clone_storage(&copied)
		mut moved := original.move()
		assert_aligned_map_clone_storage(&moved)
		assert_aligned_map_clone_storage(&original)
		key := AlignedCloneCell{ value: 1 }
		item := AlignedCloneCell{ value: 10 }
		original.set(&key, &item)
		assert_aligned_map_clone_storage(&original)
		unsafe {
			empty.free()
			copied.free()
			moved.free()
			original.free()
		}
	}
}

fn test_sparse_typed_map_clone_keeps_aligned_values() {
	mut original := map[string]AlignedCloneCell{}
	original.reserve(256)
	original['only'] = AlignedCloneCell{ value: 42 }
	mut copied := original.clone()
	assert copied['only'].value == 42
	assert usize(voidptr(&copied['only'])) % 64 == 0
	values := copied.values()
	assert usize(values.data) % 64 == 0
	assert values[0].value == 42
	copied['next'] = AlignedCloneCell{ value: 99 }
	assert usize(voidptr(&copied['next'])) % 64 == 0
	assert 'next' !in original
	unsafe {
		values.free()
		copied.free()
		original.free()
	}
}
