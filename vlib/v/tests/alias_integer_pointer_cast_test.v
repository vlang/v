@[translated]
module main

type U8 = u8

type SS = string

// `&U8(x)`, with `U8` an alias of an integer type, is a pointer cast like
// `&u8(x)`. C translated by c2v marks an unbounded end pointer with
// `&U8(-1)` (SQLite's `zTerm = (const u8*)(-1)`).
fn end_pointer(n int, z &U8) &U8 {
	mut z_term := &U8(0)
	if n >= 0 {
		z_term = unsafe { z + n }
	} else {
		z_term = &U8((-1))
	}
	return z_term
}

fn test_pointer_cast_to_an_alias_of_an_integer_type() {
	all_ones := &U8(-1)
	assert usize(all_ones) == ~usize(0)
	assert usize(end_pointer(-1, unsafe { nil })) == ~usize(0)
	five := &U8(5)
	assert usize(five) == 5
}

// `&SS('hi')`, with `SS` an alias of a value type, still points to a copy.
fn test_address_of_an_alias_of_a_value_type() {
	s := &SS('hi')
	assert *s == 'hi'
}
