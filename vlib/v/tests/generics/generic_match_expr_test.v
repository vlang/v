struct Foo[T] {
	a T
}

fn r[T]() Foo[T] {
	return Foo[T]{}
}

fn t[T](v T) !Foo[T] {
	return match typeof(v).name {
		'string' {
			r[T]()
		}
		else {
			r[T]()
		}
	}
}

fn test_main() {
	t(1)!
	t('')!
	assert true
}

type GenericMatchSumtype[T] = GenericMatchY[T] | GenericMatchZ[T]

fn (mut x GenericMatchSumtype[T]) do_it[T]() string {
	match mut x {
		GenericMatchY[T] { return 'doing GenericMatchY' }
		GenericMatchZ[T] { return 'doing GenericMatchZ' }
	}

	return ''
}

struct GenericMatchY[T] {
	name  string
	value T
}

struct GenericMatchZ[T] {
	name  string
	value T
}

fn test_match_generic_sumtype_variant_in_generic_method() {
	y := GenericMatchY[int]{
		name:  'Y'
		value: 1
	}
	z := GenericMatchZ[bool]{
		name:  'Z'
		value: true
	}

	mut x1 := GenericMatchSumtype[int](y)
	assert x1.do_it() == 'doing GenericMatchY'

	mut x2 := GenericMatchSumtype[bool](z)
	assert x2.do_it() == 'doing GenericMatchZ'
}
