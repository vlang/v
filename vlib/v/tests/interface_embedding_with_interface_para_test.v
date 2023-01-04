interface Eq {
	eq(other Eq) bool
}

interface Ord {
	Eq
	lt(other Ord) bool
}

// implement Ord for Int
struct Int {
	value int
}

fn (i Int) eq(other Ord) bool {
	if other is Int {
		return i.value == other.value
	}
	return false
}

fn (i Int) lt(other Ord) bool {
	if other is Int {
		return i.value < other.value
	}
	return false
}

fn compare(x Ord, y Ord) {
	println(x.eq(y))
	assert !x.eq(y)
	println(x.lt(y))
	assert x.lt(y)
}

fn test_interface_embedding_with_interface_para() {
	compare(Int{1}, Int{2})
}
