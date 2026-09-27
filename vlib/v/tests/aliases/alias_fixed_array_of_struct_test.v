struct Foo {}

type Bar = [4]Foo

struct Baz {
	data int
}

type BazFixed = [2]Baz
type NestedBazFixed = [2]BazFixed

struct Empty {}

type EmptyFixed = [2]Empty
type NestedEmptyFixed = [4][2]Empty

type Dot = [3]f64
type Box = [2]Dot

struct Tst {
	box Box
	val int
}

fn test_alias_fixed_array_of_struct() {
	bar := Bar([Foo{}, Foo{}, Foo{}, Foo{}]!)
	println(bar)
	assert '${bar}' == 'Bar([Foo{}, Foo{}, Foo{}, Foo{}])'
}

fn test_nested_fixed_array_alias_of_named_struct() {
	nested := [2]NestedBazFixed{}
	assert nested.len == 2
	assert nested[0].len == 2
	assert nested[0][0][0].data == 0
}

fn test_fixed_array_alias_of_empty_struct() {
	nested := [2]EmptyFixed{}
	assert nested.len == 2
	assert nested[0].len == 2
}

fn test_direct_fixed_array_alias_of_empty_struct_init() {
	fixed := EmptyFixed{}
	assert fixed.len == 2

	ref_fixed := &EmptyFixed{}
	assert ref_fixed.len == 2
}

fn test_nested_fixed_array_alias_of_empty_struct() {
	nested := NestedEmptyFixed{}
	assert nested.len == 4
	assert nested[0].len == 2

	ref_nested := &NestedEmptyFixed{}
	assert ref_nested.len == 4
}

fn test_nested_fixed_array_alias_in_struct_init() {
	v_box := Box{}
	assert v_box.len == 2
	assert v_box[0].len == 3

	v_tst := Tst{Box([2]Dot{}), 1}
	println(v_tst)
	assert v_tst.val == 1
	assert v_tst.box[0][0] == 0.0
	assert v_tst.box[1][2] == 0.0
}

fn test_blank_plain_assign_nested_fixed_array_of_empty_structs() {
	_ = NestedEmptyFixed{}
}

fn test_reassign_fixed_array_alias() {
	// Not empty struct
	mut fixed := BazFixed{}
	fixed = BazFixed{}
	assert fixed.len == 2

	mut ref_fixed := &BazFixed{}
	ref_fixed = &BazFixed{}
	assert ref_fixed.len == 2

	mut nested_fixed := NestedBazFixed{}
	nested_fixed = NestedBazFixed{}
	assert nested_fixed.len == 2

	mut ref_nested_fixed := &NestedBazFixed{}
	ref_nested_fixed = &NestedBazFixed{}
	assert ref_nested_fixed.len == 2

	// Empty struct
	mut empty_fixed := EmptyFixed{}
	empty_fixed = EmptyFixed{}
	assert empty_fixed.len == 2

	mut ref_empty_fixed := &EmptyFixed{}
	ref_empty_fixed = &EmptyFixed{}
	assert ref_empty_fixed.len == 2

	mut nested_empty_fixed := NestedEmptyFixed{}
	nested_empty_fixed = NestedEmptyFixed{}
	assert nested_empty_fixed.len == 4

	mut ref_nested_empty_fixed := &NestedEmptyFixed{}
	ref_nested_empty_fixed = &NestedEmptyFixed{}
	assert ref_nested_empty_fixed.len == 4
}
