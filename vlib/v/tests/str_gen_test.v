fn test_array_of_floats() {
	// f64 array
	aa := [1.2, 3.4, 5.67]
	assert aa.str() == '[1.2, 3.4, 5.67]'
	assert '$aa' == '[1.2, 3.4, 5.67]'
	// f32 array
	bb := [f32(1.2), 3.4, 5.67]
	assert bb.str() == '[1.2, 3.4, 5.67]'
	assert '$bb' == '[1.2, 3.4, 5.67]'
}

fn test_array_of_bools() {
	aa := [true, false, true]
	assert aa.str() == '[true, false, true]'
	assert '$aa' == '[true, false, true]'
}

fn test_array_of_ints() {
	// int
	a1 := [11, 22, 33]
	assert a1.str() == '[11, 22, 33]'
	assert '$a1' == '[11, 22, 33]'
	// u32
	a2 := [u32(11), 22, 33]
	assert a2.str() == '[11, 22, 33]'
	assert '$a2' == '[11, 22, 33]'
	// i16
	b1 := [i16(11), 22, 33]
	assert b1.str() == '[11, 22, 33]'
	assert '$b1' == '[11, 22, 33]'
	// u16
	b2 := [u16(11), 22, 33]
	assert b2.str() == '[11, 22, 33]'
	assert '$b2' == '[11, 22, 33]'
	// i64
	c1 := [i64(11), 22, 33]
	assert c1.str() == '[11, 22, 33]'
	assert '$c1' == '[11, 22, 33]'
	// u64
	c2 := [u64(11), 22, 33]
	assert c2.str() == '[11, 22, 33]'
	assert '$c2' == '[11, 22, 33]'
}

fn test_array_of_bytes() {
	aa := [`a`, `b`, `c`]
	assert aa.str() == '[a, b, c]'
	assert '$aa' == '[a, b, c]'
}

fn test_array_of_strings() {
	aa := ['aa', 'bb', 'cc']
	assert aa.str() == "['aa', 'bb', 'cc']"
	assert '$aa' == "['aa', 'bb', 'cc']"
}

fn test_map_of_ints() {
	aa := {'a': 1, 'b': 2, 'c': 3}
	assert aa.str() == "{'a': 1, 'b': 2, 'c': 3}"
	assert '$aa' == "{'a': 1, 'b': 2, 'c': 3}"
}

fn test_map_of_strings() {
	aa := {'a': '1', 'b': '2', 'c': '3'}
	assert aa.str() == "{'a': '1', 'b': '2', 'c': '3'}"
	assert '$aa' == "{'a': '1', 'b': '2', 'c': '3'}"
}

fn test_map_of_floats() {
	aa := {'a': 1.1, 'b': 2.2, 'c': 3.3}
	assert aa.str() == "{'a': 1.1, 'b': 2.2, 'c': 3.3}"
	assert '$aa' == "{'a': 1.1, 'b': 2.2, 'c': 3.3}"
}

fn test_map_of_bytes() {
	aa := {'a': `a`, 'b': `b`, 'c': `c`}
	assert aa.str() == "{'a': a, 'b': b, 'c': c}"
	assert '$aa' == "{'a': a, 'b': b, 'c': c}"
}

fn test_map_of_bools() {
	aa := {'a': true, 'b': false, 'c': true}
	assert aa.str() == "{'a': true, 'b': false, 'c': true}"
	assert '$aa' == "{'a': true, 'b': false, 'c': true}"
}

fn test_fixed_array_of_floats() {
	// f64 array
	aa := [1.2, 3.4, 5.67]!!
	assert aa.str() == '[1.2, 3.4, 5.67]'
	assert '$aa' == '[1.2, 3.4, 5.67]'
	// f32 array
	bb := [f32(1.2), 3.4, 5.67]!!
	assert bb.str() == '[1.2, 3.4, 5.67]'
	assert '$bb' == '[1.2, 3.4, 5.67]'
}

fn test_fixed_array_of_bools() {
	aa := [true, false, true]!!
	assert aa.str() == '[true, false, true]'
	assert '$aa' == '[true, false, true]'
}

fn test_fixed_array_of_ints() {
	// int
	a1 := [11, 22, 33]!!
	assert a1.str() == '[11, 22, 33]'
	assert '$a1' == '[11, 22, 33]'
	// u32
	a2 := [u32(11), 22, 33]!!
	assert a2.str() == '[11, 22, 33]'
	assert '$a2' == '[11, 22, 33]'
	// i16
	b1 := [i16(11), 22, 33]!!
	assert b1.str() == '[11, 22, 33]'
	assert '$b1' == '[11, 22, 33]'
	// u16
	b2 := [u16(11), 22, 33]!!
	assert b2.str() == '[11, 22, 33]'
	assert '$b2' == '[11, 22, 33]'
	// i64
	c1 := [i64(11), 22, 33]!!
	assert c1.str() == '[11, 22, 33]'
	assert '$c1' == '[11, 22, 33]'
	// u64
	c2 := [u64(11), 22, 33]!!
	assert c2.str() == '[11, 22, 33]'
	assert '$c2' == '[11, 22, 33]'
}

fn test_fixed_array_of_bytes() {
	aa := [`a`, `b`, `c`]!!
	assert aa.str() == '[a, b, c]'
	assert '$aa' == '[a, b, c]'
}

fn test_fixed_array_of_strings() {
	aa := ['aa', 'bb', 'cc']!!
	assert aa.str() == "['aa', 'bb', 'cc']"
	assert '$aa' == "['aa', 'bb', 'cc']"
}

struct Wrapper {
	foo &string
}
fn test_struct_with_string_pointer() {
	s := 'test'
	w := Wrapper{&s}
	assert '$w' == 'Wrapper {\n    foo: &\'test\'\n}'
	assert w.str() == 'Wrapper {\n    foo: &\'test\'\n}'
}

struct Wrapper2 {
	foo &int
}
fn test_struct_with_int_pointer() {
	i := 5
	w := Wrapper2{&i}
	assert '$w' == 'Wrapper2 {\n    foo: &5\n}'
	assert w.str() == 'Wrapper2 {\n    foo: &5\n}'
}

struct Wrapper3 {
	foo &bool
}
fn test_struct_with_bool_pointer() {
	b := true
	w := Wrapper3{&b}
	assert '$w' == 'Wrapper3 {\n    foo: &true\n}'
	assert w.str() == 'Wrapper3 {\n    foo: &true\n}'
}

struct Foo {}
struct Wrapper4 {
	foo &Foo
}
fn test_struct_with_struct_pointer() {
	b := Foo{}
	w := Wrapper4{&b}
	assert '$w' == 'Wrapper4 {\n    foo: &Foo {\n    }\n}'
	assert w.str() == 'Wrapper4 {\n    foo: &Foo {\n    }\n}'
}

fn test_struct_with_nil() {
	w := Wrapper4{}
	assert '$w' == 'Wrapper4 {\n    foo: &nil\n}'
	assert w.str() == 'Wrapper4 {\n    foo: &nil\n}'
}

struct Wrapper5 {
	foo &f32
}
fn test_struct_with_f32_pointer() {
	i := f32(5.1)
	w := Wrapper5{&i}
	assert '$w' == 'Wrapper5 {\n    foo: &5.1\n}'
	assert w.str() == 'Wrapper5 {\n    foo: &5.1\n}'
}
