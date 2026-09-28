import strings

struct Holder {
mut:
	items []int
}

fn set_noslices(mut a []int) {
	unsafe { a.flags.set(.noslices) }
}

fn test_array_flags_set_and_clear_outside_builtin() {
	mut a := []int{len: 3}
	assert !a.flags.has(.noslices)
	unsafe { a.flags.set(.noslices) }
	assert a.flags.has(.noslices)
	unsafe { a.flags.set(.noshrink | .nogrow) }
	assert a.flags.all(.noslices | .noshrink | .nogrow)
	unsafe { a.flags.clear(.noslices) }
	assert !a.flags.has(.noslices)
	assert a.flags.has(.noshrink)
	unsafe { a.flags.clear(.noshrink | .nogrow) }
	assert !a.flags.has(.noshrink)
	assert !a.flags.has(.nogrow)
}

fn test_array_flags_set_through_field_and_mut_param() {
	mut h := Holder{
		items: [1, 2, 3]
	}
	unsafe { h.items.flags.set(.noslices) }
	assert h.items.flags.has(.noslices)
	mut b := [4, 5]
	set_noslices(mut b)
	assert b.flags.has(.noslices)
	b << 6
	assert b == [4, 5, 6]
}

fn test_strings_builder_flags() {
	mut sb := strings.new_builder(4)
	assert sb.flags.has(.noslices)
	sb.write_string('hello world')
	assert sb.str() == 'hello world'
	sb.write_string('abc')
	plain := unsafe { sb.reuse_as_plain_u8_array() }
	assert !plain.flags.has(.noslices)
	assert plain.bytestr() == 'abc'
}
