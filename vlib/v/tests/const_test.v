pub const (
	c = a
	a = b
	c = a + b
	b = 1
	d = (e / 2) + 7
	e = 9
)

struct Foo {
	
}	

fn test_const() {
	assert a == 1
	assert d == 11
	assert c == 1
}
