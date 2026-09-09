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
		age: 30
	}) != ?Person(Person{
		name: 'Bob'
		age: 25
	})
	assert ?Person(Person{
		name: 'Alice'
		age: 30
	}) != ?Person(none)
}

fn test_option_struct_eq_with_strings() {
	assert ?Person(Person{
		name: 'Alice'
		age: 30
	}) == ?Person(Person{
		name: 'Alice'
		age: 30
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

struct OptionCallCounter {
mut:
	calls int
}

fn counted_optional_string(mut counter OptionCallCounter, value string) ?string {
	counter.calls++
	return value
}

fn test_option_eq_side_effects_preserve_short_circuiting() {
	mut left_counter := OptionCallCounter{}
	mut right_counter := OptionCallCounter{}
	assert !(false && counted_optional_string(mut left_counter, 'same') == counted_optional_string(mut right_counter, 'same'))
	assert left_counter.calls == 0
	assert right_counter.calls == 0
	assert true && counted_optional_string(mut left_counter, 'same') == counted_optional_string(mut right_counter, 'same')
	assert left_counter.calls == 1
	assert right_counter.calls == 1
	assert true || counted_optional_string(mut left_counter, 'same') == counted_optional_string(mut right_counter, 'same')
	assert left_counter.calls == 1
	assert right_counter.calls == 1
	assert false || counted_optional_string(mut left_counter, 'same') == counted_optional_string(mut right_counter, 'same')
	assert left_counter.calls == 2
	assert right_counter.calls == 2
	assert [1].any(it == 1) && counted_optional_string(mut left_counter, 'same') == counted_optional_string(mut right_counter, 'same')
	assert left_counter.calls == 3
	assert right_counter.calls == 3
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

struct OptionPointerOverloadValue {
	value int
}

fn (a &OptionPointerOverloadValue) ==(b &OptionPointerOverloadValue) bool {
	return a.value == b.value
}

fn test_option_pointer_payload_uses_reference_equality_overload() {
	a := ?&OptionPointerOverloadValue(&OptionPointerOverloadValue{
		value: 1
	})
	b := ?&OptionPointerOverloadValue(&OptionPointerOverloadValue{
		value: 1
	})
	c := ?&OptionPointerOverloadValue(&OptionPointerOverloadValue{
		value: 2
	})
	n := ?&OptionPointerOverloadValue(none)
	assert a == b
	assert a != c
	assert a != n
	assert n == ?&OptionPointerOverloadValue(none)
}
