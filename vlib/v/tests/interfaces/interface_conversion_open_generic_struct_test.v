import arrays

// arrays.ReverseIterator[T] has a field `a`, but without type arguments it is no
// type of values: converting an empty interface to `AInterface` must not list it,
// or the C code names the undeclared `arrays__ReverseIterator`.
interface Holder {}

interface AInterface {
	a int
}

struct StructA {
	a int
}

fn as_a(holder Holder) ?AInterface {
	if holder is AInterface {
		return holder
	}
	return none
}

fn test_interface_conversion_skips_open_generic_structs() {
	value := as_a(Holder(StructA{7})) or { panic('no AInterface') }
	assert value.a == 7
	total := arrays.sum([1, 2]) or { 0 }
	assert total == 3
}
