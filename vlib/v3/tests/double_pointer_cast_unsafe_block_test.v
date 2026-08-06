struct PointerCastItem {
	value int
}

fn test_double_pointer_cast_in_unsafe_value_block_preserves_its_type() {
	mut item := &PointerCastItem{
		value: 42
	}
	raw := voidptr(&item)
	actual := unsafe { *(&&PointerCastItem(raw)) }
	assert actual.value == 42
}
