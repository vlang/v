struct Info {
pub mut:
	value int
}

fn test_mutable_pointer_from_unsafe_index_can_be_mutated() {
	mut item := Info{}
	items := [&item]!
	mut info := unsafe { items[0] }
	info.value = 1
	assert item.value == 1
}
