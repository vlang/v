module types

// Checker views share this monotonic cache, including reads outside a worker's
// body range. All accesses must be atomic even when writers own disjoint ranges.
@[direct_array_access; inline]
fn (tc &TypeChecker) has_lexical_smartcast_miss(idx int) bool {
	return idx < tc.lexical_smartcast_misses.len
		&& C.atomic_load_byte(voidptr(&tc.lexical_smartcast_misses[idx])) != 0
}

@[direct_array_access; inline]
fn (tc &TypeChecker) set_lexical_smartcast_miss(idx int) {
	C.atomic_store_byte(voidptr(&tc.lexical_smartcast_misses[idx]), 1)
}
