module io

fn test_copy_bytes_uses_lengths_and_preserves_destination() {
	mut dst := 'xxxxx'.bytes()
	assert copy_bytes(mut dst, 'abc'.bytes()) == 3
	assert dst.bytestr() == 'abcxx'
	assert copy_bytes(mut dst, '123456'.bytes()) == 5
	assert dst.bytestr() == '12345'
	assert dst.len == 5
	mut empty := []u8{cap: 8}
	assert copy_bytes(mut empty, 'abc'.bytes()) == 0
	assert empty.len == 0
	assert copy_bytes(mut dst, []u8{}) == 0
	assert dst.bytestr() == '12345'
}

fn test_copy_bytes_overlapping_slices() {
	mut forward := 'abcdef'.bytes()
	assert copy_bytes(mut forward[2..], forward[..4]) == 4
	assert forward.bytestr() == 'ababcd'
	mut backward := 'abcdef'.bytes()
	assert copy_bytes(mut backward[..4], backward[2..]) == 4
	assert backward.bytestr() == 'cdefef'
	assert copy_bytes(mut backward, backward) == 6
	assert backward.bytestr() == 'cdefef'
}

fn test_copy_bytes_function_value() {
	copier := copy_bytes
	mut dst := []u8{len: 3}
	assert copier(mut dst, 'abcd'.bytes()) == 3
	assert dst.bytestr() == 'abc'
}
