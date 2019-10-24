pub const (
	// c = a // TODO
	a = b
	b = 1
)

struct Foo {
	
}	

fn test_const() {
	assert a == 1
	// assert c == 1 // TODO: This will not build yet
}
