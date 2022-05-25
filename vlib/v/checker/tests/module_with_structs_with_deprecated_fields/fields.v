module module_with_structs_with_deprecated_fields

pub struct Xyz {
pub mut:
	a int
	b int [deprecated]
	c int [deprecated: 'c use Xyz.a instead']
	d int [deprecated: 'd use Xyz.a instead'; deprecated_after: '2021-03-01']
	e int [deprecated: 'e use Xyz.a instead'; deprecated_after: '2021-03-01'; deprecated_only_publicly]
}

fn some_internal_function() {
	mut x := Xyz{}
	dump(x.a) // no error
	dump(x.b) // should generate an error
	dump(x.c) // should generate an error
	dump(x.d) // should generate an error
	dump(x.e) // should NOT be an error, because of deprecated_only_publicly
	//
	x.a = 1 // no error
	x.b = 1 // should generate an error
	x.c = 1 // should generate an error
	x.d = 1 // should generate an error
	x.e = 1 // should NOT be an error, because of deprecated_only_publicly
}
