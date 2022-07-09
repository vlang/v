module module_with_structs_with_deprecated_fields

pub struct Xyz {
pub mut:
	a int
	b int [deprecated]
	c int [deprecated: 'c use Xyz.a instead'; deprecated_after: '2021-03-01']
	d int [deprecated: 'd use Xyz.a instead'; deprecated_after: '2999-03-01']
}

fn some_internal_function() {
	mut x := Xyz{} // initialisation; no error

	// reads:
	dump(x.a) // no error
	dump(x.b) // no error internally
	dump(x.c) // no error internally
	dump(x.d) // no error internally

	// writes:
	x.a = 1 // no error
	x.b = 1 // no error internally
	x.c = 1 // no error internally
	x.d = 1 // no error internally
}
