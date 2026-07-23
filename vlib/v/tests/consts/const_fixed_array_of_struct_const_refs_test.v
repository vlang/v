// Regression test: a fixed-array const whose elements are references to
// other struct consts must compile. Struct consts are runtime-initialized
// C globals, so referencing them in a static C initializer produced
// "initializer element is not constant"; such arrays must be initialized
// in _vinit instead.
struct Info {
	code i16
	name string
}

const first = Info{1, 'first'}
const second = Info{2, 'second'}
const third = Info{3, 'third'}

const by_ref = [first, second, third]!

// mixing references and literals must also work
const mixed = [first, Info{9, 'literal'}]!

// simple #define'd literal consts must keep working statically
const ia = 5
const ib = 6
const int_refs = [ia, ib]!

fn test_fixed_array_of_struct_const_refs() {
	assert by_ref.len == 3
	assert by_ref[0].code == 1
	assert by_ref[1].name == 'second'
	mut total := 0
	for e in by_ref {
		total += int(e.code)
	}
	assert total == 6
}

fn test_mixed_refs_and_literals() {
	assert mixed[0].name == 'first'
	assert mixed[1].code == 9
}

fn test_simple_define_consts_still_static() {
	assert int_refs[0] + int_refs[1] == 11
}
