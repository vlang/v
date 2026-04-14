module module_with_deprecated_structs

pub struct Xyz {
pub mut:
	a int
	b int @[deprecated]
	c int @[deprecated(msg: 'c use Xyz.a instead', after: '2021-03-01')]
	d int @[deprecated(msg: 'd use Xyz.a instead', after: '2999-03-01')]
}

@[deprecated(msg: 'use New instead', after: '2021-03-01')]
pub struct Old {}

@[deprecated(msg: 'use Future instead', after: '2999-03-01')]
pub struct Present {}

fn some_internal_function() {
	mut x := Xyz{} // initialisation; no error
	mut o := Old{}
	mut p := Present{}

	// reads:
	dump(x.a) // no error
	dump(x.b) // no error internally
	dump(x.c) // no error internally
	dump(x.d) // no error internally
	dump(o) // no error internally
	dump(p) // no error internally

	// writes:
	x.a = 1 // no error
	x.b = 1 // no error internally
	x.c = 1 // no error internally
	x.d = 1 // no error internally
}
