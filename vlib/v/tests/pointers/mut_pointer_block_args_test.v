import pointer_block_args

struct MutableBlockBox {
mut:
	value int
}

struct MutableBlockCounter {
mut:
	value int
}

fn change_generic_block[T](mut box T) {
	box.value = 42
}

fn count_block_call(mut counter MutableBlockCounter) {
	counter.value++
}

fn test_generic_mutable_block_preserves_pointer_storage() {
	mut box := &MutableBlockBox{}
	original := voidptr(box)
	change_generic_block[MutableBlockBox](mut unsafe { box })
	assert voidptr(box) == original
	assert box.value == 42

	mut value := MutableBlockBox{}
	change_generic_block[MutableBlockBox](mut unsafe { &value })
	assert value.value == 42
}

fn test_generic_mutable_block_preserves_side_effects() {
	mut box := &MutableBlockBox{}
	mut counter := MutableBlockCounter{}
	change_generic_block[MutableBlockBox](mut unsafe {
		count_block_call(mut counter)
		box
	})
	assert counter.value == 1
	assert box.value == 42
}

fn test_imported_mutable_block_passes_existing_pointer() {
	mut box := &pointer_block_args.Box{}
	original := voidptr(box)
	pointer_block_args.change_box(mut unsafe { box })
	assert voidptr(box) == original
	assert box.value == 42

	mut value := pointer_block_args.Box{}
	pointer_block_args.change_box(mut unsafe { &value })
	assert value.value == 42
}
