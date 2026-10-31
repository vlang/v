interface DefaultItem {
	value int
}

struct Item {
	value int
}

const default_item = &Item{ value: 42 }

struct Holder {
	item DefaultItem = default_item
}

fn test_interface_field_accepts_pointer_default() {
	first := Holder{}
	second := Holder{ item: &Item{ value: 7 } }
	assert first.item.value == 42
	assert second.item.value == 7
	assert first.item is Item
}
