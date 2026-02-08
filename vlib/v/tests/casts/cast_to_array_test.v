module main

type Bytes = [3]u8
type Str = []u8

fn test_cast_to_fixed_array() {
	mut x := Bytes{}
	x[0] = 1
	x[1] = 2
	x[2] = 3
	dump(x)

	y := [3]u8(x)
	dump(y)
	assert y.str() == '[1, 2, 3]'

	z := Bytes(y)
	dump(z)
	assert z.str() == 'Bytes([1, 2, 3])'
}

fn test_cast_to_array() {
	mut x := Str{len: 3}
	x[0] = 10
	x[1] = 20
	x[2] = 30
	dump(x)

	y := []u8(x)
	dump(y)
	assert y.str() == '[10, 20, 30]'

	z := Str(y)
	dump(z)
	assert z.str() == 'Str([10, 20, 30])'
}
