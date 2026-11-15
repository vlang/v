// vtest vflags: -w
// Deliberate deprecated-type coverage must also run in suites using -W.
module binary

fn test_legacy_signed_encoding_preserves_wire_width() ! {
	value := isize(-42)
	for use_big_endian in [false, true] {
		bytes := encode_binary(value, big_endian: use_big_endian)!
		assert bytes.len == int(sizeof(isize))
		assert decode_binary[isize](bytes, big_endian: use_big_endian)! == value
		values := [value, isize(123)]
		assert decode_binary[[]isize](encode_binary(values, big_endian: use_big_endian)!,
			big_endian: use_big_endian
		)! == values
		mapping := {
			value: isize(123)
		}
		assert decode_binary[map[isize]isize](encode_binary(mapping, big_endian: use_big_endian)!,
			big_endian: use_big_endian
		)! == mapping
	}
}
