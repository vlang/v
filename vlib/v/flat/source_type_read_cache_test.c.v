module flat

fn test_source_type_read_cache_rechecks_ids_on_collision() {
	left_text := '&read_cache.Left'
	left := source_type_id(left_text)
	right_text := '[]read_cache.Right'
	right := source_type_id(right_text)
	left_entry := source_type_entry(source_type_table_load(), left)
	right_entry := source_type_entry(source_type_table_load(), right)
	slot := int(left & u32(source_type_read_cache_size - 1))
	// A fully published entry for another ID must never answer a colliding query.
	$if x32 {
		C.atomic_store_u32(voidptr(&g_source_type_read_entries[slot]), u32(voidptr(right_entry)))
	} $else {
		C.atomic_store_u64(voidptr(&g_source_type_read_entries[slot]), u64(voidptr(right_entry)))
	}
	assert source_type_name(left) == left_text
	assert source_type_shape(left) == left_entry.shape
	assert source_type_read_entry(left) == left_entry
	assert source_type_name(right) == right_text
	assert source_type_shape(right) == right_entry.shape
}

fn test_source_type_read_cache_preserves_empty_sentinels() {
	assert source_type_name(0) == ''
	assert source_type_shape(0) == unsafe { SourceTypeShape(0) }
}

fn test_source_type_read_cache_keeps_canonical_borrowed_spelling() {
	mut bytes := 'read_cache.Borrowed'.bytes()
	view := unsafe { tos(bytes.data, bytes.len) }
	id := source_type_id(view)
	assert source_type_name(id) == 'read_cache.Borrowed'
	shape := source_type_shape(id)
	bytes[0] = `R`
	changed := source_type_id(view)
	assert changed != id
	assert source_type_name(id) == 'read_cache.Borrowed'
	assert source_type_shape(id) == shape
	assert source_type_name(changed) == 'Read_cache.Borrowed'
}

fn test_source_type_read_cache_retains_entry_across_growth() {
	text := '[]read_cache.BeforeGrowth'
	id := source_type_id(text)
	assert source_type_name(id) == text
	entry := source_type_read_entry(id)
	mut table := source_type_table_load()
	source_type_lock()
	table.grow()
	C.atomic_store_u32(&g_source_types_lock, 0)
	for i in 0 .. source_type_read_cache_size + 1 {
		source_type_id('read_cache.Growth_${i}')
	}
	assert source_type_name(id) == text
	assert source_type_shape(id) == entry.shape
	assert source_type_read_entry(id) == entry
}

fn cache_source_type_in_disposable_scope() u32 {
	$if prealloc {
		scope := unsafe { prealloc_scope_begin() }
		id := source_type_id('[]read_cache.Disposable'.clone())
		assert source_type_name(id) == '[]read_cache.Disposable'
		assert source_type_shape(id).has(.array_type)
		unsafe {
			prealloc_scope_leave(scope)
			prealloc_scope_free_after(scope)
			prealloc_thread_cleanup()
		}
		return id
	} $else {
		id := source_type_id('[]read_cache.Disposable'.clone())
		assert source_type_name(id) == '[]read_cache.Disposable'
		assert source_type_shape(id).has(.array_type)
		return id
	}
}

fn test_source_type_read_cache_outlives_publishing_worker_arena() {
	handle := spawn cache_source_type_in_disposable_scope()
	id := handle.wait()
	assert source_type_name(id) == '[]read_cache.Disposable'
	assert source_type_shape(id).has(.array_type)
}
