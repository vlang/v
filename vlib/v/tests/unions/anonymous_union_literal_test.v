struct AnonymousUnionBox {
	id   int
	data union {
		small u8
		large u64
	}
}

struct AnonymousTextUnionBox {
	data union {
		text string
		number int
	}
}

fn anonymous_union_number() u8 {
	return 23
}

fn test_anonymous_union_literal_keeps_the_declared_layout() {
	box := AnonymousUnionBox{
		id:   42
		data: union { small: 7 }
	}
	assert box.id == 42
	assert unsafe { box.data.small } == 7
	assert sizeof(box.data) == sizeof(u64)
}

fn test_anonymous_union_literal_checks_values_in_the_field_context() {
	box := AnonymousUnionBox{
		data: union { small: anonymous_union_number() }
	}
	assert unsafe { box.data.small } == 23
	text := 'world'
	text_box := AnonymousTextUnionBox{
		data: union { text: text }
	}
	assert unsafe { text_box.data.text } == 'world'
}

fn test_anonymous_union_literal_can_be_empty_in_context() {
	box := AnonymousUnionBox{
		data: union {}
	}
	assert unsafe { box.data.large } == 0
}

fn test_empty_anonymous_union_literal_infers_a_concrete_local_type() {
	value := union {}
	assert value.str() == 'union {}'
}

fn test_anonymous_union_literal_infers_a_local_value() {
	text := 'local'
	value := union { text: text }
	assert unsafe { value.text } == 'local'
	assert value.str() == "union {\n    text: 'local'\n}"
}

fn test_anonymous_union_string_does_not_expose_a_generated_type_name() {
	box := AnonymousUnionBox{
		data: union { large: 9 }
	}
	assert box.str().contains('data: union {')
	assert !box.str().contains('AnonUnion_')
}

fn anonymous_union_identity[T](value T) T {
	return value
}

fn test_anonymous_union_literals_in_containers_and_generic_calls() {
	values := [union { number: 1 }, union { number: 2 }]
	assert unsafe { values[0].number } == 1
	assert unsafe { values[1].number } == 2
	lookup := {
		'a': union { number: 3 }
	}
	assert unsafe { lookup['a'].number } == 3
	value := anonymous_union_identity(union { number: 4 })
	assert unsafe { value.number } == 4
}

fn test_anonymous_union_literals_at_conditional_branch_tails() {
	for choose_small in [true, false] {
		box := AnonymousUnionBox{
			data: if choose_small {
				union { small: 5 }
			} else {
				union { large: 123456789 }
			}
		}
		if choose_small {
			assert unsafe { box.data.small } == 5
		} else {
			assert unsafe { box.data.large } == 123456789
		}
	}
}

union AnonUnion_User {
	number int
}

fn test_named_union_keeps_its_source_name_when_formatted() {
	value := AnonUnion_User{ number: 7 }
	assert value.str().starts_with('AnonUnion_User{')
}

fn test_anonymous_union_and_struct_shapes_stay_distinct() {
	u := union {number: 7}
	s := struct {number: 8}
	assert unsafe { u.number } == 7
	assert s.number == 8
	assert s.str().starts_with('struct {')
}
