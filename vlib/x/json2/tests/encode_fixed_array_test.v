import x.json2

union NumberOrBytes {
	number u64
	bytes  [8]u8 @[skip]
}

fn test_encode_fixed_array() {
	assert json2.encode([u8(1), 2, 3]!) == '[1,2,3]'
}

fn test_encode_union_with_skipped_fixed_array_field() {
	decoded := json2.decode[NumberOrBytes]('{"number":256}')!

	unsafe {
		assert decoded.bytes == [u8(0), 1, 0, 0, 0, 0, 0, 0]!
	}
	assert json2.encode(decoded) == '{"number":256}'
}
