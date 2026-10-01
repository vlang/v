// `byte` is not a type (use `u8`), so it is usable as any other name.
struct Packet {
mut:
	byte u8
}

enum Width {
	byte
	word
}

fn (byte Packet) first() u8 {
	return byte.byte
}

fn is_letter(byte u8) bool {
	return byte >= `a` && byte <= `z`
}

fn sum(byte u8, other u8) u8 {
	return byte + other
}

fn byte() u8 {
	return 42
}

fn test_byte_as_fn_name() {
	assert byte() == 42
}

fn test_byte_as_param_field_receiver_and_enum_value_name() {
	mut p := Packet{
		byte: 3
	}
	p.byte++
	assert p.first() == 4
	assert is_letter(`c`)
	assert !is_letter(`A`)
	assert sum(1, 2) == 3
	add_one := fn (byte u8) u8 {
		return byte + 1
	}
	assert add_one(1) == 2
	for byte in [u8(7)] {
		assert byte == 7
	}
	w := Width.byte
	assert w == .byte
}
