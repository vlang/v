module fastc

struct StoredEqualityValue45 {
	key     int
	ignored int
}

fn (a StoredEqualityValue45) == (b StoredEqualityValue45) bool {
	return a.key == b.key
}

struct StoredEqualityReference45 {
	key     int
	ignored int
}

fn (a &StoredEqualityReference45) == (b &StoredEqualityReference45) bool {
	return a.key == b.key
}

struct StoredEqualityNested45 {
	value     StoredEqualityValue45
	reference StoredEqualityReference45
}

fn test_stored_equality_honors_value_and_reference_overloads_in_arrays_and_fields() {
	a := StoredEqualityValue45{ key: 4, ignored: 1 }
	b := StoredEqualityValue45{ key: 4, ignored: 2 }
	c := StoredEqualityValue45{ key: 5, ignored: 1 }
	assert a == b
	assert [a] == [b]
	assert [a] != [c]
	assert [[a]] == [[b]]
	assert [[a]] != [[c]]
	assert [[a]!] == [[b]!]
	assert [[a]!] != [[c]!]
	r := StoredEqualityReference45{ key: 4, ignored: 1 }
	s := StoredEqualityReference45{ key: 4, ignored: 2 }
	t := StoredEqualityReference45{ key: 5, ignored: 1 }
	assert r == s
	assert [r] == [s]
	assert [r] != [t]
	left := StoredEqualityNested45{ value: a, reference: r }
	right := StoredEqualityNested45{ value: b, reference: s }
	assert left == right
	assert [left] == [right]
	assert [left] != [StoredEqualityNested45{ value: c, reference: s }]
	assert [left] != [StoredEqualityNested45{ value: b, reference: t }]
	// Pointer-valued elements keep identity even when their pointee overloads equality.
	assert [&r] != [&s]
	assert [&r] == [&r]
}
