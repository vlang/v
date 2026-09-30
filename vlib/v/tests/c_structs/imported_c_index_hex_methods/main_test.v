module main

import left
import right

fn test_hex_and_index() {
	right.used()
	mut value := left.make()
	ptr := &value
	assert ptr.hex() == 'pointer'
	unsafe {
		callback := ptr.hex
		assert callback() == 'pointer'
	}
	generic_value := left.make_generic_counter()
	assert (&generic_value).hex[int](1) == 'generic pointer'
	assert value[2] == 19
	value[2] = 31
	assert value[1] == 30
	value[2] += 5
	assert value[2] == 36
}

fn test_imported_infix_and_compound_operators() {
	a := left.make()
	mut b := left.make()
	b[0] = 19
	assert a == b
	assert !(a != b)
	assert a < b
	assert b > a
	assert a <= b
	assert b >= a
	assert (a + b)[0] == 36
	assert (a * b)[0] == 323
	b += a
	assert b[0] == 36
	b *= a
	assert b[0] == 612
}
