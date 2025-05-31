module main

fn foo(f int) !(int, ?int) {
	if f < 0 {
		return error('f is negative')
	}
	if f > 0 {
		return 1, f
	}
	return 0, none
}

fn test_main() {
	a, b := foo(-1) or { 2, 2 }
	c := b? as int
	assert a == 2
	assert c == 2

	a2, b2 := foo(0) or { 2, 2 }
	c2 := b2? as int
	assert a2 == 0
	assert c2 == 0

	a3, b3 := foo(1) or { 2, 2 }
	c3 := b3? as int
	assert a3 == 1
	assert c3 == 1
}
