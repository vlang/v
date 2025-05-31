module main

type Arr = [4]u8

fn test_main() {
	mut a := Arr{}
	a = Arr([u8(5), 4, 3, 2]!)
	assert a == Arr([u8(5), 4, 3, 2]!)
	assert a == [u8(5), 4, 3, 2]!
}
