module main

type Arr = [4]u8

fn foo(a int) ?Arr {
	if a > 0 {
		return Arr([u8(1), 2, 3, 4]!)
	}
	return none
}

fn bar(a int) !Arr {
	if a > 0 {
		return Arr([u8(1), 2, 3, 4]!)
	}
	return error('')
}

fn test_main() {
	f := foo(1) or { Arr([u8(0), 0, 0, 0]!) }
	dump(f)
	assert f == [u8(1), 2, 3, 4]!

	ff := foo(0)
	dump(ff)
	assert ff == none

	b := bar(1) or { Arr([u8(0), 0, 0, 0]!) }
	dump(f)
	assert b == [u8(1), 2, 3, 4]!

	bb := bar(0) or { Arr([u8(0), 0, 0, 0]!) }
	dump(bb)
	assert bb == [u8(0), 0, 0, 0]!
}
