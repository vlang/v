struct Item {
	id int
}

fn both_empty(a []int, b []int) bool {
	return a == [] && b == []
}

fn first_empty_and(a []int, flag bool) bool {
	return a == [] && flag
}

fn identity(value bool) bool {
	return value
}

fn generic_both_empty[T](a []T, b []T) bool {
	return a == [] && b == []
}

fn test_empty_array_literal_before_logical_and() {
	a := []int{}
	b := []int{}
	c := [1]
	assert a == [] && b == []
	assert a == [] && b == [] && c != []
	assert c != [] && a == [] && b == []
	assert !(a == [] && c == [])
	assert !(c == [] && a == [])
	assert c == [] || a == []
	assert both_empty(a, b)
	assert !both_empty(a, c)
	assert first_empty_and(a, true)
	assert !first_empty_and(a, false)
	assert generic_both_empty(a, b)
	assert !generic_both_empty(['x'], []string{})
	assert identity(a == [] && b == [])
	assert identity((a == [] && b == []))
	both := a == [] && b == []
	assert both
}

fn test_empty_array_literal_before_logical_and_in_conditions() {
	a := []int{}
	flag := true
	mut hits := 0
	if a == [] && flag {
		hits++
	}
	if a != [] && flag {
		hits += 10
	}
	for a == [] && flag {
		hits++
		break
	}
	hits += match true {
		a == [] && flag { 1 }
		else { 100 }
	}
	assert hits == 3
	assert 1 in [1, 2] && flag
	assert 1 !in [] && flag
}

fn test_array_literal_with_elements_before_logical_and() {
	a := [1]
	fixed := [1]!
	flag := true
	assert a == [1] && flag
	assert a == [1] && a != [2]
	assert fixed == [1]! && flag
	assert fixed == [1]! && fixed != [2]!
}

fn test_arrays_of_references_are_still_array_inits() {
	item := &Item{
		id: 7
	}
	item_ref := &item
	mut refs := []&Item{}
	refs << item
	mut ref_refs := []&&Item{}
	ref_refs << item_ref
	mut nested := [][]&Item{}
	nested << refs
	assert refs.len == 1 && refs[0].id == 7
	assert ref_refs.len == 1 && ref_refs[0].id == 7
	assert nested.len == 1 && nested[0][0].id == 7
	assert []&Item{} == [] && []&&Item{} == []
}

fn test_comments_separate_logical_and_after_array_literals() {
	a := []int{}
	flag := true
	assert a == [] && /* separator */
	flag
	assert a == [] && // separator
	flag
	assert a == [] /* separator */
		&& flag
	fixed := [1]!
	assert fixed == [1]! /* separator */
		&& flag
}
