type Bytes = [3]u8
type Strs = [3]string

fn test_none() {
	b := ?Bytes(none)
	println(b)
	assert b == none

	c := ?Strs(none)
	println(c)
	assert c == none
}

fn test_non_none() {
	b := ?Bytes([u8(1), 2, 3]!)
	println(b)
	assert b != none
	assert b?[2] == 3

	c := ?Strs(['a', 'b', 'c']!)
	println(c)
	assert c != none
	assert c?[2] == 'c'
}
