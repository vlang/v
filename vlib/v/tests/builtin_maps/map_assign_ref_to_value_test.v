struct Foo {
mut:
	val int
}

fn test_map_assign_ref_to_value_uses_copy_for_value_map() {
	mut values := map[string]Foo{}
	mut b := Foo{
		val: 2
	}

	values['two'] = &b
	b.val = 3

	assert values['two'].val == 2
}
