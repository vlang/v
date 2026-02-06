// vtest flance: -d MOREINFO
// Test for issue #25847: C error with sumtype arrays
// Bug: When pushing an array to []SumType where SumType has []SumType as a variant,
// the compiler incorrectly used PUSH_MANY instead of pushing as a single element.

type Prim = int | string | []Prim

fn test_push_array_literal_to_sumtype_array() {
	mut args := []Prim{}
	// Push an array literal as a single element (it should become a Prim variant)
	args << [Prim(1), Prim(2)]
	assert args.len == 1

	inner := args[0] as []Prim
	assert inner.len == 2
	assert (inner[0] as int) == 1
	assert (inner[1] as int) == 2
}

fn test_push_array_variable_to_sumtype_array() {
	mut args := []Prim{}
	// Push an array variable as a single Prim element
	nested := []Prim{len: 2, init: Prim(index)}
	args << nested
	assert args.len == 1

	inner := args[0] as []Prim
	assert inner.len == 2
}

fn test_push_single_elements_still_works() {
	mut args := []Prim{}
	// Push single elements (not arrays)
	args << 42
	args << 'hello'
	args << 123
	assert args.len == 3

	assert (args[0] as int) == 42
	assert (args[1] as string) == 'hello'
	assert (args[2] as int) == 123
}

fn test_empty_sumtype_array_init() {
	mut args := []Prim{}
	assert args.len == 0

	for i in 0 .. 3 {
		args << i
	}
	assert args.len == 3
}
