struct Id {
	v int
}

struct Person {
	name string
	age  int
}

fn test_option_struct_ne() {
	assert ?Id(Id{
		v: 1
	}) != ?Id(Id{
		v: 2
	})
	assert ?Id(Id{
		v: 1
	}) != ?Id(none)
}

fn test_option_struct_eq() {
	assert ?Id(Id{
		v: 1
	}) == ?Id(Id{
		v: 1
	})
	assert ?Id(none) == ?Id(none)
}

fn test_option_struct_ne_with_strings() {
	assert ?Person(Person{
		name: 'Alice'
		age:  30
	}) != ?Person(Person{
		name: 'Bob'
		age:  25
	})
	assert ?Person(Person{
		name: 'Alice'
		age:  30
	}) != ?Person(none)
}

fn test_option_struct_eq_with_strings() {
	assert ?Person(Person{
		name: 'Alice'
		age:  30
	}) == ?Person(Person{
		name: 'Alice'
		age:  30
	})
	assert ?Person(none) == ?Person(none)
}

fn cmp(a ?Id, b ?Id) bool {
	return a != b
}

fn test_option_struct_eq_in_fn() {
	assert cmp(Id{ v: 1 }, Id{
		v: 2
	}) == true
	assert cmp(Id{ v: 1 }, Id{
		v: 1
	}) == false
	assert cmp(none, none) == false
	assert cmp(Id{ v: 1 }, none) == true
}

fn make_id(v int) ?Id {
	return Id{
		v: v
	}
}

fn make_none_id() ?Id {
	return none
}

fn test_option_struct_eq_fn_call_result() {
	assert make_id(1) == make_id(1)
	assert make_id(1) != make_id(2)
	assert make_none_id() == make_none_id()
	assert make_id(1) != make_none_id()
}

fn test_option_struct_eq_in_short_circuit() {
	a := ?Id(Id{
		v: 1
	})
	b := ?Id(Id{
		v: 1
	})
	c := ?Id(Id{
		v: 2
	})
	assert true && a == b
	assert true && a != c
	assert false || a == b
	assert !(false && a != b)
}

fn test_option_ptr_struct_eq() {
	a := ?&Id(&Id{
		v: 1
	})
	b := ?&Id(&Id{
		v: 1
	})
	c := ?&Id(&Id{
		v: 2
	})
	d := ?&Id(none)
	e := ?&Id(none)
	assert a == b
	assert a != c
	assert a != d
	assert d == e
	assert a != ?&Id(none)
}

fn test_option_ptr_struct_ne() {
	a := ?&Id(&Id{
		v: 1
	})
	b := ?&Id(&Id{
		v: 2
	})
	assert a != b
	assert a != ?&Id(none)
}
