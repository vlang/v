struct Outer<T> {
mut:
	inner Inner<T>
}

struct Inner<T> {
	val T
}

fn (mut i Inner<T>) next<S>(input S) f64 {
	$if S is f32 {
		return 32
	} $else {
		panic('"$S.name" is not supported')
		return 0
	}
}

fn test_generics_method_on_nested_struct() {
	mut outer := Outer<f64>{
		inner: Inner<f64>{
			val: 1.1
		}
	}
	r1 := outer.inner.next<f32>(99.0)
	println(r1)
	assert r1 == 32.0

	r2 := outer.inner.next<f64, f32>(99.0)
	println(r2)
	assert r2 == 32.0

	r3 := outer.inner.next(f32(99.0))
	println(r3)
	assert r3 == 32.0
}
