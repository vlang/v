// Test for bug #17372: Uninitialized interface type field gives runtime error when accessing it
// Expected behavior: print "nil" instead of causing a runtime crash

interface Boo {}

struct Foo {
	boo Boo
}

@[noinit]
struct Bar {
	boo Boo
}

fn test_uninitialized_interface_field_str() {
	// Printing uninitialized interface field should return "nil", not crash
	s := '${Foo{}.boo}'
	assert s == 'nil', 'Expected "nil" for uninitialized interface, got "${s}"'
}

fn test_uninitialized_interface_field_in_struct_str() {
	// Struct containing uninitialized interface should also work
	foo := Foo{}
	s := '${foo}'
	assert s.contains('boo: nil'), 'Expected struct str to contain "boo: nil", got "${s}"'
}

fn test_noinit_struct_with_interface_field() {
	bar := Bar{}
	s := '${bar.boo}'
	assert s == 'nil', 'Expected "nil" for noinit struct interface field, got "${s}"'
}
