module types

import v3.flat

// Two fixed arrays whose length is written as the same const spelling (`[size]int`)
// but resolves to different lengths share `ArrayFixed.name()`, yet must fold to
// different C representations. The c_type cache must therefore key on semantic
// identity, not on the textual name.
fn test_c_type_cache_distinguishes_same_named_fixed_arrays() {
	mut a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)

	t3 := Type(ArrayFixed{
		elem_type: Type(int_)
		len:       3
		len_expr:  'size'
	})
	t5 := Type(ArrayFixed{
		elem_type: Type(int_)
		len:       5
		len_expr:  'size'
	})

	// Same source spelling: this is what a textual cache key would collapse.
	assert t3.name() == t5.name()
	assert t3.name() == '[size]int'

	c3 := tc.c_type(t3)
	c5 := tc.c_type(t5)

	// The second lookup must not receive the first type's cached C representation.
	assert c3 != c5, 'c_type folded [3]int and [5]int to the same C type: ${c3}'
	assert c3.ends_with('_3'), c3
	assert c5.ends_with('_5'), c5

	// Re-querying returns the same per-type result (cache hit stays correct).
	assert tc.c_type(t3) == c3
	assert tc.c_type(t5) == c5
}

// Interning the same semantic type twice yields the same cached C representation.
fn test_c_type_cache_reuses_entry_for_equal_types() {
	mut a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	first := Type(ArrayFixed{
		elem_type: Type(int_)
		len:       7
		len_expr:  'n'
	})
	again := Type(ArrayFixed{
		elem_type: Type(int_)
		len:       7
		len_expr:  'n'
	})
	assert tc.c_type(first) == tc.c_type(again)
}
