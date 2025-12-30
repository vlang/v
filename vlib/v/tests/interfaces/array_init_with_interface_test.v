interface IValue {}

struct Null {}

struct MyInt {
	val int
}

fn test_array_init_with_interface_implicit_cast() {
	// Test that array init with interface element type allows implicit cast
	a := []IValue{len: 3, init: Null{}}
	assert a.len == 3

	b := []IValue{len: 2, init: MyInt{42}}
	assert b.len == 2
}
