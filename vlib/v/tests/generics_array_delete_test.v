struct Set[T] {
mut:
	field []T
}

fn (mut s Set[T]) add[T](value T) bool {
	mut result := false

	if value !in s.field {
		s.field << value
		result = true
	}

	return result
}

fn (mut s Set[T]) remove[T](value T) bool {
	mut result := false

	if value in s.field {
		ndx := s.field.index(value)
		s.field.delete(ndx)
		result = true
	}

	return result
}

fn test_generics_array_delete() {
	// int
	mut set1 := Set[int]{}

	mut added := set1.add(4)
	println(added)
	assert added

	added = set1.add(3)
	println(added)
	assert added

	added = set1.add(3)
	println(added)
	assert !added

	println(set1)
	mut removed := set1.remove(4)
	println(removed)
	assert removed

	// f64
	mut set2 := Set[f64]{}

	added = set2.add(4.4)
	println(added)
	assert added

	added = set2.add(3.3)
	println(added)
	assert added

	added = set2.add(3.3)
	println(added)
	assert !added

	println(set2)
	removed = set2.remove(4.4)
	println(removed)
	assert removed

	// string
	mut set3 := Set[string]{}

	added = set3.add('aaa')
	println(added)
	assert added

	added = set3.add('bbb')
	println(added)
	assert added

	added = set3.add('bbb')
	println(added)
	assert !added

	println(set3)
	removed = set3.remove('aaa')
	println(removed)
	assert removed
}
