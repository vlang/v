struct Foo {}

type Bar = [4]Foo

struct Baz {
	data int
}

type BazFixed = [2]Baz
type NestedBazFixed = [2]BazFixed

struct Empty {}

type EmptyFixed = [2]Empty

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
