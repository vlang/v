import arena
import strings

fn test_buffer_transfer_keeps_arena_allocation_and_can_be_reused_on_heap() {
	mut storage := arena.new(chunk_size: 1024)
	defer { storage.free() }
	storage.push()
	mut buffer := strings.new_buffer(1)
	buffer.write_string('arena text')
	used_before_transfer := storage.used()
	text := buffer.take_string()
	used_after_transfer := storage.used()
	storage.pop()
	assert used_after_transfer == used_before_transfer
	assert storage.owns(text.str)
	assert text == 'arena text'
	assert unsafe { text.str[text.len] } == 0
	assert buffer.len() == 0
	assert buffer.capacity() == 0
	buffer.write_string('heap text')
	heap_text := buffer.take_string()
	assert !storage.owns(heap_text.str)
	survivor := text.clone()
	storage.reset()
	assert survivor == 'arena text'
	assert heap_text == 'heap text'
	unsafe {
		survivor.free()
		heap_text.free()
		buffer.free()
	}
}

fn test_populated_arena_buffer_growth_after_pop_moves_storage_to_heap() {
	mut storage := arena.new()
	defer { storage.free() }
	storage.push()
	mut buffer := strings.new_buffer(4)
	buffer.write_string('root')
	storage.pop()
	assert !arena.active()
	// Request more than the available capacity, independently of the growth policy.
	buffer.reserve(buffer.capacity() + 1)
	buffer.write_string('-heap')
	assert !arena.active()
	text := buffer.take_string()
	assert !storage.owns(text.str)
	storage.free()
	assert text == 'root-heap'
	assert unsafe { text.str[text.len] } == 0
	unsafe {
		text.free()
		buffer.free()
	}
}

fn test_populated_arena_buffer_growth_in_child_uses_child_lifetime() {
	mut parent := arena.new()
	mut child := arena.new()
	defer {
		child.free()
		parent.free()
	}
	parent.push()
	mut buffer := strings.new_buffer(4)
	buffer.write_string('root')
	child.push()
	buffer.reserve(buffer.capacity() + 1)
	buffer.write_string('-child')
	assert child.is_current()
	assert !parent.is_current()
	text := buffer.take_string()
	assert child.owns(text.str)
	assert !parent.owns(text.str)
	child.pop()
	assert parent.is_current()
	parent.pop()
	assert !arena.active()
	parent.free()
	assert text == 'root-child'
	assert unsafe { text.str[text.len] } == 0
	retained := text.clone()
	child.reset()
	assert retained == 'root-child'
	unsafe {
		retained.free()
		buffer.free()
	}
}

fn test_populated_arena_buffer_free_and_reuse_preserve_current_allocator() {
	for free_while_active in [false, true] {
		mut storage := arena.new()
		storage.push()
		mut buffer := strings.new_buffer(8)
		buffer.write_string('discard')
		other := 'keep'.clone()
		if !free_while_active {
			storage.pop()
		}
		used_before_free := storage.used()
		unsafe { buffer.free() }
		assert buffer.len() == 0
		assert buffer.capacity() == 0
		assert storage.used() == used_before_free
		assert other == 'keep'
		assert storage.is_current() == free_while_active
		buffer.write_string('reuse')
		text := buffer.take_string()
		assert storage.owns(text.str) == free_while_active
		assert storage.is_current() == free_while_active
		if free_while_active {
			storage.pop()
		}
		assert !arena.active()
		assert text == 'reuse'
		assert unsafe { text.str[text.len] } == 0
		retained := text.clone()
		storage.free()
		assert retained == 'reuse'
		if !free_while_active {
			assert text == 'reuse'
			unsafe { text.free() }
		}
		unsafe {
			retained.free()
			buffer.free()
		}
	}
}
