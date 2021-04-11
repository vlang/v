// see: https://discordapp.com/channels/592103645835821068/592114487759470596/762270244566728704
enum WireType {
	varint = 0
	_64bit = 1
	length_prefixed = 2
	_32bit = 5
}

fn pack_wire_type(w WireType) byte {
	return byte(w)
}

fn test_casting_an_enum_to_byte() {
	x := WireType.length_prefixed
	y := pack_wire_type(x)
	assert 'x: $x' == 'x: length_prefixed'
	assert 'y: $y.hex()' == 'y: 02'
}

//

fn test_casting_a_float_to_byte() {
	x := 1.23
	b := byte(x)
	assert 'x: $x | b: $b.hex()' == 'x: 1.23 | b: 01'
}

fn test_casting_an_int_to_byte() {
	x := 12
	b := byte(x)
	assert 'x: $x | b: $b.hex()' == 'x: 12 | b: 0c'
}

fn test_casting_a_bool_to_byte() {
	x := true
	b1 := byte(x)
	assert 'x: $x | b: $b1.hex()' == 'x: true | b: 01'
	y := false
	b2 := byte(y)
	assert 'y: $y | b: $b2.hex()' == 'y: false | b: 00'
}
