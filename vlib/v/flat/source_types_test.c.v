module flat

import hash

fn test_source_type_recent_cache_checks_borrowed_spelling_after_mutation() {
	mut bytes := 'source_types.MutableBorrowedInput'.bytes()
	text := unsafe { tos(bytes.data, bytes.len) }
	id := source_type_id(text)
	assert source_type_name(id) == 'source_types.MutableBorrowedInput'
	bytes[1] = `X`
	changed_id := source_type_id(text)
	assert changed_id != id
	assert source_type_name(changed_id) == 'sXurce_types.MutableBorrowedInput'
	assert source_type_name(id) == 'source_types.MutableBorrowedInput'
	assert source_type_id('source_types.MutableBorrowedInput'.clone()) == id
}

fn test_existing_source_type_does_not_take_the_insertion_lock() {
	text := 'source_types.Existing'
	id := source_type_id(text)
	source_type_lock()
	defer { C.atomic_store_u32(&g_source_types_lock, 0) }
	assert source_type_id(text.clone()) == id
	assert source_type_name(id) == text
}

fn test_source_type_readers_can_finish_on_an_old_index_after_growth() {
	text := 'source_types.Snapshot'
	id := source_type_id(text)
	mut table := source_type_table_load()
	previous := source_type_index_load(table)
	source_type_lock()
	table.grow()
	C.atomic_store_u32(&g_source_types_lock, 0)
	assert previous != source_type_index_load(table)
	assert source_type_find(table, previous, text, hash.sum64_string(text, 0)) == id
	assert source_type_id(text) == id
	new_text := 'source_types.AfterGrowth'
	new_id := source_type_id(new_text)
	assert source_type_find(table, previous, new_text, hash.sum64_string(new_text, 0)) == 0
	assert source_type_id(new_text) == new_id
}

fn intern_source_types_concurrently(worker int, shared []u32) []u32 {
	mut result := []u32{cap: 2048}
	for i in 0 .. 2048 {
		// Distinct insertions grow the index while other workers read existing IDs.
		unique := 'source_types.Worker_${worker}_${i}'
		id := source_type_id(unique)
		assert source_type_name(id) == unique
		assert source_type_id(source_type_name(shared[i % shared.len])) == shared[i % shared.len]
		// The first insertion of these names races across workers too.
		result << source_type_id('source_types.Shared_${i}')
	}
	return result
}

fn test_concurrent_source_type_insertions_and_reads_keep_the_same_identity() {
	mut shared := []u32{cap: 256}
	for i in 0 .. 256 {
		shared << source_type_id('source_types.Published_${i}')
	}
	mut threads := []thread []u32{}
	for worker in 0 .. 8 {
		threads << spawn intern_source_types_concurrently(worker, shared)
	}
	mut expected := []u32{}
	for worker, handle in threads {
		ids := handle.wait()
		if worker == 0 {
			expected = ids.clone()
		} else {
			assert ids == expected
		}
	}
}

fn intern_source_type_in_disposable_scope() u32 {
	$if prealloc {
		scope := unsafe { prealloc_scope_begin() }
		id := source_type_id('source_types.Disposable'.clone())
		mut table := source_type_table_load()
		source_type_lock()
		table.grow()
		C.atomic_store_u32(&g_source_types_lock, 0)
		// The process-wide table and its spelling must survive the caller's arena.
		unsafe {
			prealloc_scope_leave(scope)
			prealloc_scope_free_after(scope)
			prealloc_thread_cleanup()
		}
		return id
	} $else {
		return source_type_id('source_types.Disposable'.clone())
	}
}

fn test_source_type_index_and_spelling_outlive_the_inserting_worker() {
	handle := spawn intern_source_type_in_disposable_scope()
	id := handle.wait()
	assert source_type_name(id) == 'source_types.Disposable'
	assert source_type_id('source_types.Disposable') == id
}
