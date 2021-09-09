struct Set<T> {
mut:
	field []T
}

fn (mut s Set<T>) add<T>(value T) bool {
	mut result := false

	if value !in s.field {
		s.field << value
		result = true
	}

	return result
}

fn (mut s Set<T>) remove<T>(value T) bool {
	mut result := false

	if value in s.field {
		ndx := s.field.index(value)
		s.field.delete(ndx)
		result = true
	}

	return result
}

fn test_generics_array_delete() {
	mut set := Set<int>{}

	mut added := set.add(4)
	println(added)
	assert added

	added = set.add(3)
	println(added)
	assert added

	added = set.add(3)
	println(added)
	assert !added

	println(set)
	mut removed := set.remove(4)
	println(removed)
	assert removed
}
